{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Control.Monad.Except
import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Agent.Core
import Data.Default (def)
import Agent.OpenAI (makeOpenAI)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value(..))
import System.Environment (getEnv, lookupEnv)
import Network.HTTP.Conduit (simpleHttp)

getWeather :: ToolInstance
getWeather = ToolInstance $ (
  \params ->
    case Map.lookup "city" params of
      Just (String cityName) -> do
        let url = "https://wttr.in/" ++ T.unpack cityName
        response <- simpleHttp url
        return $ T.pack $ show response
        
      Just _ ->
        return "Error: Parameter 'city' must be a string"
        
      Nothing ->
        return "Error: No city name is provided"
  )

getWeatherTool :: Tool
getWeatherTool = Tool
  { toolName = "get_weather"
  , toolDesc = "Get the current weather of a city."
  , toolArgs = [ArgInfo "city" "string" "The city to be queried"]
  }

myAdd :: ToolInstance
myAdd = ToolInstance $ (
  \params ->
    case (Map.lookup "a" params, Map.lookup "b" params) of
      (Just (Number a), Just (Number b)) -> do
        return $ T.pack $ show $ a + b

      (Just _, Just _) ->
        return "Error: a and b have to be both integers"

      (Nothing, _) ->
        return "Error: parameter a is not provided"

      (_, Nothing) ->
        return "Error: parameter b is not provided"
  )

myAddTool :: Tool
myAddTool = Tool
  { toolName = "add"
  , toolDesc = "Calculate the sum of two integers."
  , toolArgs = [ArgInfo "a" "int" "add nnumber a", ArgInfo "b" "int" "add nnumber b"]
  }

toolMap :: Map.Map T.Text ToolInstance
toolMap = Map.fromList
  [ (toolName getWeatherTool, getWeather)
  , (toolName myAddTool, myAdd)
  ]


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
llmNode :: LLM -> GenerationConfig -> [Tool] -> StepM Bool
llmNode llm conf tools = do
  s <- get
  let sysMessage = SystemMessage "You are an assiatant for weather queries."
  modify (\s -> s { memory = sysMessage : memory s })
  response <- liftIO $ invoke llm conf tools (reverse $ memory s)
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
      toolResponse <- liftIO $ callToolsAndGenerateMessages message toolMap
      case toolResponse of
        Just toolMessages ->
          modify (\s -> s { memory = toolMessages ++ memory s })

        Nothing -> throwError $ BadHistory "Tool node is not followed by AIMessage"

    [] -> throwError $ BadHistory "The history is empty"

agent :: LLM -> GenerationConfig -> [Tool] -> StepM ()
agent llm conf tools = do
  takeInputNode
  toolLoop
 where
  toolLoop :: StepM ()
  toolLoop = do
    routeToTool <- llmNode llm conf tools
    when routeToTool $ do
      toolNode
      toolLoop

agentLoop :: StepM ()
agentLoop = do
  key <- liftIO $ getEnv "OPENAI_API_KEY"
  let modelName = "gpt-5-nano"
      model = makeOpenAI key modelName
      tools = [getWeatherTool]

  agent model def tools
  agentLoop

main :: IO (Either AgentError ())
main = evalAgent () (AgentState []) agentLoop
