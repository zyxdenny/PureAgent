{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Agent.Core as AC
import qualified Agent.FooAIModel as FA

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

llmAct :: LLM llm => llm -> StepM ()
llmAct llm = do
  s <- get
  maybeAiMessage <- liftIO $ AC.invoke llm (memory s)
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

agent :: LLM llm => llm -> StepM ()
agent llm = takeInput >> llmAct llm >> printState

model :: FA.FooAI
model = AC.bindTools AC.init [tool1, tool2]

main :: IO ()
main = evalStateT (forever $ agent model) (AgentState [])
