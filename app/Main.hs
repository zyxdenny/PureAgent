{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe
import System.IO (hFlush, stdout)
import Agent.Core
import Data.Default (def)
import Agent.OpenAI (makeOpenAI)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value(..))
import System.Environment (getEnv, lookupEnv)
import Network.HTTP.Conduit (simpleHttp)

type ToolM = ReaderT (Map.Map T.Text Value) IO T.Text

getWeather :: ToolM
getWeather = do
  params <- ask
  case Map.lookup "city" params of
    -- 1. Pattern match on 'String' constructor to extract the Text
    Just (String cityName) -> do
      let url = "https://wttr.in/" ++ T.unpack cityName
      response <- simpleHttp url
      return $ T.pack $ show response
      
    -- 2. Handle case where key exists but isn't a string (e.g. Number 42)
    Just _ ->
      return "Error: Parameter 'city' must be a string."
      
    -- 3. Handle missing key
    Nothing ->
      return "Error: No city name is provided."

getWeatherTool :: Tool
getWeatherTool = Tool
  { toolName = "get_weather"
  , toolDesc = "Get the current weather of a city."
  , toolArgs = [ArgInfo "city" "string" "The city to be queried"]
  }

toolMap :: Map.Map T.Text ToolM
toolMap = Map.fromList [(toolName getWeatherTool, getWeather)]


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
    AIMessage _ ts : _ -> do
      toolResults <- liftIO $
        mapM (
          \(ToolCall _ name args) ->
            case Map.lookup name toolMap of
              Just toolM -> do
                result <- runReaderT toolM args
                return $ Just result

              Nothing -> return Nothing
        ) ts

      let toolIDs = map (\(ToolCall iD _ _) -> iD) ts
          toolMessages = catMaybes $ zipWith f toolResults toolIDs
            where
              f (Just res) iD = Just $ ToolMessage res iD
              f Nothing _     = Nothing

      modify (\s -> s { memory = toolMessages ++ memory s })

    _ ->
      throwError $ BadHistory "Tool node is not followed by AIMessage"

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
