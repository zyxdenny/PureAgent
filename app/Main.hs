{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.State
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
getWeatherTool = Tool
  ToolSchema
    { toolName = "get_weather"
    , toolDesc = "Get the current weather of a city."
    , toolArgs = [ArgInfo "city" "string" "The city to be queried"]
    }
  (ToolInstance tool)
    where
      tool = do 
        city <- getParam @T.Text "city"
        return $ "It's always sunny in " <> city

toolRegistry :: ToolRegistry
toolRegistry = registerTools [getWeatherTool]


data AgentState = AgentState
  { memory  :: [Message]
  } deriving (Show)


data AgentError =
    LLMCallingFail LLMError
  | BadMessage T.Text
  | BadHistory T.Text
  | ToolNotFound T.Text

type StepM = AgentM () AgentState AgentError

takeInputNode :: StepM ()
takeInputNode = do
  input <- liftIO $ do
    TIO.putStr "> "
    hFlush stdout
    TIO.getLine
  let inputMessage = UserMessage input
  modify (\s -> s { memory = inputMessage : memory s })

-- Returns True if there is tool call, false otherwise
llmNode :: LLM -> GenerationConfig -> StepM Bool
llmNode llm conf = do
  s <- get
  let sysMessage = SystemMessage "You are an assiatant for weather queries. Only answer questions about weather."
  modify (\s -> s { memory = sysMessage : memory s })
  response <- liftIO $ invoke llm conf toolRegistry (reverse $ memory s)
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
  AgentState m <- get
  case m of
    message : _ -> do
      toolResponse <- liftIO $ callToolsAndGenerateMessages message toolRegistry
      case toolResponse of
        Just toolMessages ->
          modify (\s -> s { memory = toolMessages ++ memory s })

        Nothing -> throwError $ BadHistory "Tool node is not followed by AIMessage"

    [] -> throwError $ BadHistory "The history is empty"

agent :: LLM -> GenerationConfig -> StepM ()
agent llm conf = do
  takeInputNode
  toolLoop
    where
      toolLoop :: StepM ()
      toolLoop = do
        routeToTool <- llmNode llm conf
        when routeToTool $ do
          toolNode
          toolLoop

agentLoop :: StepM ()
agentLoop = do
  key <- liftIO $ getEnv "OPENAI_API_KEY"
  let modelName = "gpt-5-nano"
      model = makeOpenAI key modelName

  agent model def 
  agentLoop

main :: IO (Either AgentError ())
main = evalAgent () (AgentState []) agentLoop
