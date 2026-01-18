{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isAlpha)
import System.IO (hFlush, stdout)
import Data.Default (def)
import System.Environment (getEnv)

import Agent.Core
import Model.OpenAI (makeOpenAI)

data AgentEnv = AgentEnv
  { getGenerationConfig :: GenerationConfig
  , getLLM :: IO LLM
  }

data AgentState = AgentState
  { memory  :: [Message]
  } deriving (Show)

data AgentError =
    LLMCallingFail LLMError
  | BadMessage T.Text

type StepM = AgentM AgentEnv AgentState AgentError

takeInputNode :: StepM ()
takeInputNode = do
  input <- liftIO $ do
    TIO.putStr "> "
    hFlush stdout
    TIO.getLine
  let inputMessage = UserMessage input
  modify (\s -> s { memory = inputMessage : memory s })

-- Returns the response of the LLM
llmNode :: StepM T.Text
llmNode = do
  env <- ask
  llm <- liftIO $ getLLM env
  let conf = getGenerationConfig env
  s <- get
  let sysMessage = SystemMessage "Answer questions from the customer"
  modify (\s -> s { memory = sysMessage : memory s })
  response <- liftIO $ invoke llm conf Nothing (reverse $ memory s)
  case response of         
    Right (AIMessage txt _) -> do
      return txt

    Right _ -> do
      throwError $ BadMessage "The LLM doesn't produce AIMessage"

    Left err -> do
      throwError $ LLMCallingFail err

-- The guardrail censors out Jinping
outputGuardRailNode :: T.Text -> StepM Bool
outputGuardRailNode txt = do
  return $
    not $ null $
    filter
      (\x -> elem x censorList)
      (normalize txt)
      where
        normalize :: T.Text -> [T.Text]
        normalize =
          T.words
          . T.map step
          . T.toCaseFold
          where
            step c
              | isAlpha c = c
              | otherwise = ' '

        censorList :: [T.Text]
        censorList =
          [ "jinping"
          ]

agent :: StepM ()
agent = do
  takeInputNode
  response <- llmNode
  isCensored <- outputGuardRailNode response
  if isCensored then do
    let response = "Sorry, I can't answer this question."
    liftIO $ TIO.putStrLn response
    modify (\s -> s { memory = AIMessage response [] : memory s })
  else do
    unless (T.null response) $ liftIO $ TIO.putStrLn response
    modify (\s -> s { memory = AIMessage response [] : memory s })

main :: IO (Either AgentError ())
main = evalAgent env initState agentLoop
  where
    env = AgentEnv
      { getGenerationConfig = def
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
