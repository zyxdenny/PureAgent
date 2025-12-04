{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Agent.Core
import Data.Default (def)
import Agent.OpenAI (makeOpenAI)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value(..))
import System.Environment (getEnv, lookupEnv)

type ToolM = ReaderT (Map.Map T.Text Value) IO T.Text
getWeather :: ToolM
getWeather = do
  params <- ask
  case Map.lookup "city" params of
    -- 1. Pattern match on 'String' constructor to extract the Text
    Just (String cityName) -> 
      return $ "It's always sunny in " <> cityName <> "."
      
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

type StepM = StateT AgentState IO


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
  messageOrErr <- liftIO $ invoke llm conf tools (reverse $ memory s)
  case messageOrErr of         
    Right aiMessage@(AIMessage txt ts) -> do
      if not $ T.null txt then
        liftIO $ TIO.putStrLn txt
      else
        return ()
      modify (\s -> s { memory = aiMessage : memory s })
      return $ not $ null ts

    Right _ -> do
      liftIO $ putStrLn "The LLM returns Non-AI message???"
      return False

    Left err -> do
      liftIO $ putStrLn $ show err
      return False

toolNode :: StepM ()
toolNode = do
  AgentState m <- get
  case m of
    AIMessage _ ts : _ -> do
      toolResults <- liftIO
                       $ sequence
                       $ map (\(ToolCall _ name args) ->
                                case Map.lookup name toolMap of
                                  Just toolM -> runReaderT toolM args
                                  Nothing    -> return $ "Tool " <> name <> " is not found."
                             ) ts
      let toolIDs = map (\(ToolCall id _ _) -> id) ts
          toolMessages = map (\(r, id) -> ToolMessage r id) $ zip toolResults toolIDs
      modify (\s -> s { memory = toolMessages ++ memory s })

    _ ->
      liftIO $ TIO.putStrLn "Error: Tool node is not followed by AIMessage"

printStateNode :: StepM ()
printStateNode = do
    s <- get
    liftIO $ TIO.putStrLn $ T.pack $ "--- State After Cycle ---\n" ++ show s ++ "\n-------------------------"

agent :: LLM -> GenerationConfig -> [Tool] -> StepM ()
agent llm conf tools = do
  takeInputNode
  toolLoop
  printStateNode
 where
  toolLoop :: StepM ()
  toolLoop = do
    routeToTool <- llmNode llm conf tools
    if routeToTool then do
      toolNode
      toolLoop
    else
      return ()

agentLoop :: StepM ()
agentLoop = do
  key <- liftIO $ getEnv "OPENAI_API_KEY"
  let modelName = "gpt-5-nano"
      model = makeOpenAI key modelName
      tools = [getWeatherTool]
  forever $ agent model def tools

main :: IO ()
main = evalStateT agentLoop (AgentState [])
