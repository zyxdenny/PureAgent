{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Agent where

import Control.Monad.Extra (whenJust)
import Data.Foldable (for_)
import qualified Data.Text as T
import Data.Default (Default(..), def)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (when)
import System.Environment (getEnv)

import Agent.Core
import Model.OpenAI (makeOpenAI)

class HasHistory st where
  -- TODO: Instead of [Message], the return
  -- type should be more generic, such as a
  -- Semigroup
  getHistory  :: st -> [Message]
  updateState :: Message -> st -> st

newtype BaseAgentState = BaseAgentState
  { history :: [Message] }

instance HasHistory BaseAgentState where
  getHistory st = history st
  updateState msg (BaseAgentState h) =
    BaseAgentState (msg : h)

data BaseAgentError =
    LLMCallingFail LLMError
  | BadMessage T.Text
  | BadHistory T.Text
  | ToolNotFound T.Text

class HasBaseAgentError err where
  injectBaseAgentError :: BaseAgentError -> err

instance HasBaseAgentError BaseAgentError where
  injectBaseAgentError = id

data AgentParams = AgentParams
  { modelName :: String
  , apiKey    :: Maybe String
  , toolReg   :: Maybe ToolRegistry
  , instr     :: Maybe T.Text
  , genConfig :: GenerationConfig
  }

instance Default AgentParams where
  def = AgentParams
    { modelName = "gpt-5-nano"
    , apiKey    = Nothing
    , toolReg   = Nothing
    , instr     = Nothing
    , genConfig = def
    }

createAgent
  :: (HasHistory st, HasBaseAgentError err)
  => AgentParams
  -> AgentM env st err ()
createAgent AgentParams{ modelName, apiKey, toolReg, instr, genConfig } =
  toolLoop
    where
      toolLoop = do
        routeToTool <- llmNode
        when routeToTool $ do
          toolNode
          toolLoop

      llmNode = do
        key <-
          maybe (liftIO $ getEnv "OPENAI_API_KEY") return apiKey
        let llm = makeOpenAI key modelName
        whenJust instr $ \i ->
          (modify $ updateState $ SystemMessage i)
        s <- get
        response <-
          liftIO $
          invoke llm genConfig toolReg (reverse $ getHistory s)
        case response of         
          Right aiMsg@(AIMessage _ ts) -> do
            modify $ updateState aiMsg
            return $ not $ null ts

          Right _ -> do
            throwError $
              injectBaseAgentError $
              BadMessage "The LLM doesn't produce AIMessage"

          Left err -> do
            throwError $
              injectBaseAgentError $
              LLMCallingFail err

      toolNode = do
        s <- get
        case getHistory s of
          msg : _ -> do
            toolResponse <-
              liftIO $
                callToolsAndGenerateMessages msg $
                maybe (registerTools []) id toolReg
            case toolResponse of
              Just toolMsgs ->
                for_ toolMsgs $ \toolMsg ->
                  modify (updateState toolMsg)

              Nothing ->
                throwError
                  $ injectBaseAgentError $
                  BadHistory "Tool node is not followed by AIMessage"

          [] ->
            throwError $
              injectBaseAgentError $
              BadHistory "The history is empty"
