{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Data.Default (def)
import System.Environment (getEnv)

import Agent.Core
import Model.OpenAI (makeOpenAI)

getWeatherTool :: Tool
getWeatherTool = Tool tool schema
  where
    tool = ToolInstance $ do
      city <- getParam @T.Text "city"
      return $ "It's always sunny in " <> city

    schema = ToolSchema
      { toolName = "get_weather"
      , toolDesc = "Get the current weather of a city."
      , toolArgs = [ArgInfo "city" "string" "The city to be queried"]
      }

data AgentEnv = AgentEnv
  { getToolRegistry :: ToolRegistry
  , getGenerationConfig :: GenerationConfig
  , getLLM :: IO LLM
  }

data AgentState = AgentState
  { memory  :: [Message]
  } deriving (Show)


data AgentError =
    LLMCallingFail LLMError
  | BadMessage T.Text
  | BadHistory T.Text
  | ToolNotFound T.Text

type StepM = AgentM AgentEnv AgentState AgentError

takeInputNode :: StepM ()
takeInputNode = do
  input <- liftIO $ do
    TIO.putStr "> "
    hFlush stdout
    TIO.getLine
  let inputMessage = UserMessage input
  modify (\s -> s { memory = inputMessage : memory s })

-- Returns True if there is tool call, false otherwise
llmNode :: StepM Bool
llmNode = do
  env <- ask
  llm <- liftIO $ getLLM env
  let conf = getGenerationConfig env
      toolRegistry = getToolRegistry env
  s <- get
  let sysMessage = SystemMessage "You are an assiatant for weather queries. Only answer questions about weather."
  modify (\s -> s { memory = sysMessage : memory s })
  response <- liftIO $ invoke llm conf (Just toolRegistry) (reverse $ memory s)
  case response of         
    Right aiMessage@(AIMessage txt ts) -> do
      unless (T.null txt) $ liftIO $ TIO.putStrLn txt
      modify (\s -> s { memory = aiMessage : memory s })
      return $ not $ null ts

    Right _ -> do
      throwError $ BadMessage "The LLM doesn't produce AIMessage"

    Left err -> do
      throwError $ LLMCallingFail err

toolNode :: StepM ()
toolNode = do
  env <- ask
  let toolRegistry = getToolRegistry env
  AgentState m <- get
  case m of
    message : _ -> do
      toolResponse <- liftIO $ callToolsAndGenerateMessages message toolRegistry
      case toolResponse of
        Just toolMessages ->
          modify (\s -> s { memory = toolMessages ++ memory s })

        Nothing -> throwError $ BadHistory "Tool node is not followed by AIMessage"

    [] -> throwError $ BadHistory "The history is empty"

agent :: StepM ()
agent = do
  takeInputNode
  toolLoop
    where
      toolLoop :: StepM ()
      toolLoop = do
        routeToTool <- llmNode
        when routeToTool $ do
          toolNode
          toolLoop

main :: IO (Either AgentError ())
main = evalAgent env initState agentLoop
  where
    env = AgentEnv
      { getToolRegistry = registerTools [getWeatherTool]
      , getGenerationConfig = def
      , getLLM = do
          key <- liftIO $ getEnv "OPENAI_API_KEY"
          let modelName = "gpt-5-nano"
              model = makeOpenAI key modelName
          return model
      }

    initState = AgentState { memory = [] }

    agentLoop = do
      agent
      agentLoop
