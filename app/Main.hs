{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Agent.Core

data AgentState = AgentState
  { memory  :: [Message]
  } deriving (Show)

type StepM = StateT AgentState IO

takeInput :: StepM ()
takeInput = do
  input <- liftIO $ do
    TIO.putStr "> "
    hFlush stdout
    TIO.getLine
  let inputMessage = Message User input
  modify (\s -> s { memory = inputMessage : memory s })

llmAct :: LLM llm => llm -> StepM ()
llmAct llm = do
  s <- get
  maybeAiMessage <- liftIO $ chat llm (memory s)
  case maybeAiMessage of         
    Nothing -> do
      liftIO $ TIO.putStrLn "Error"

    Just aiMessage@(Message _ txt) -> do
      liftIO $ TIO.putStrLn txt
      modify (\s -> s { memory = aiMessage : memory s })

printState :: StepM ()
printState = do
    s <- get
    liftIO $ TIO.putStrLn $ T.pack $ "--- State After Cycle ---\n" ++ show s ++ "\n-------------------------"

agent :: LLM llm => llm -> StepM ()
agent llm = takeInput >> llmAct llm >> printState

main :: IO ()
main = evalStateT (forever $ agent FooAI) (AgentState [])
