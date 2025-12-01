{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Agent.Core as AC
import Data.Default (def)
import Agent.FooAIModel (fooAI)

data AgentState = AgentState
  { memory  :: [Message]
  } deriving (Show)

type StepM = StateT AgentState IO

-- Example tools
tool1 :: Tool
tool1 = Tool
  { toolName = "Tool1"
  , toolDesc = "This is tool 1"
  , toolArgs = [ArgInfo "foo" "String" "Description of foo"]
  }

tool2 :: Tool
tool2 = Tool
  { toolName = "Tool2"
  , toolDesc = "This is tool 2"
  , toolArgs = [ArgInfo "bar" "Int" "Description of bar"]
  }

type AddM = ReaderT (Int, Int) IO T.Text

add :: AddM
add = do
  (a, b) <- ask
  return $ T.pack $ show $ a + b


takeInput :: StepM ()
takeInput = do
  input <- liftIO $ do
    TIO.putStr "> "
    hFlush stdout
    TIO.getLine
  let inputMessage = UserMessage input
  modify (\s -> s { memory = inputMessage : memory s })

llmAct :: LLM -> AC.GenerationConfig -> [Tool] -> StepM ()
llmAct llm conf tools = do
  s <- get
  maybeAiMessage <- liftIO $ invoke llm conf tools (memory s)
  case maybeAiMessage of         
    Just aiMessage@(AIMessage txt _) -> do
      liftIO $ TIO.putStrLn txt
      modify (\s -> s { memory = aiMessage : memory s })

    _ -> do
      liftIO $ TIO.putStrLn "Error"

printState :: StepM ()
printState = do
    s <- get
    liftIO $ TIO.putStrLn $ T.pack $ "--- State After Cycle ---\n" ++ show s ++ "\n-------------------------"

agent :: LLM -> AC.GenerationConfig -> [Tool] -> StepM ()
agent llm conf tools = takeInput >> llmAct llm conf tools >> printState

main :: IO ()
main = evalStateT (forever $ agent fooAI def [tool1, tool2]) (AgentState [])
