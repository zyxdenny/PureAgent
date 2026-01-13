{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Agent.Core where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Default (Default(..))
import Data.Aeson (Value(..))
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Network.HTTP.Client (HttpException)
-- import Data.Maybe

type ToolID = T.Text

-- | Represents a request FROM the AI to run a tool
data ToolCall = ToolCall
  { tcID   :: ToolID
  , tcName :: T.Text
  , tcArgs :: Map.Map T.Text Value
  } deriving (Show, Eq)

data Message
  -- | 1. System: Sets the behavior
  = SystemMessage T.Text
  
  -- | 2. User: The human input
  | UserMessage T.Text
  
  -- | 3. AI: Can contain text AND/OR tool calls
  -- Note: OpenAI can send text content along with tool calls (reasoning)
  | AIMessage T.Text [ToolCall] 
  
  -- | 4. Tool: The result of the function execution
  -- Must include the tool_call_id so the LLM knows which call this answers
  | ToolMessage T.Text ToolID 
  deriving (Show, Eq)

data ArgInfo = ArgInfo
  { argName :: T.Text
  , argType :: T.Text
  , argDesc :: T.Text
  } deriving Show

data Tool = Tool
  { toolName :: T.Text
  , toolDesc :: T.Text
  , toolArgs :: [ArgInfo]
  } deriving Show

data GenerationConfig = GenerationConfig
  { -- | Core Parameters (Supported by almost all LLMs)
    temperature       :: Maybe Double -- ^ 0.0 (deterministic) to 2.0 (random)
  , maxTokens         :: Maybe Int    -- ^ Limit response length
  , topP              :: Maybe Double -- ^ Nucleus sampling
  , stopSequences     :: [T.Text]       -- ^ Stop generating when these appear
  
    -- | Behavior Modifiers
  , jsonMode          :: Bool         -- ^ Force valid JSON output
  , seed              :: Maybe Int    -- ^ For deterministic reproducibility
  
    -- | The "Escape Hatch" 
    -- Allows passing provider-specific parameters not covered above
    -- e.g., OpenAI's "frequency_penalty" or Anthropic's "top_k"
  , extraParams       :: Map.Map T.Text Value 
  } deriving (Show, Eq)

instance Default GenerationConfig where
  def = GenerationConfig
    { temperature   = Nothing -- Let the API decide (usually 0.7 or 1.0)
    , maxTokens     = Nothing
    , topP          = Nothing
    , stopSequences = []
    , jsonMode      = False
    , seed          = Nothing
    , extraParams   = mempty
    }

data LLMError = LLMError
  { llmErrorType    :: LLMErrorType
  , llmErrorMessage :: T.Text
  }

data LLMErrorType
  = LLMHttpError HttpException
  | LLMRateLimited
  | LLMParseResponseError
  deriving (Show)

data LLM = LLM
  { invoke :: GenerationConfig
           -> [Tool]
           -> [Message]
           -> IO (Either LLMError Message)
  }

newtype AgentM env st err a = AgentM
  { unAgentM ::
      StateT st
        (ReaderT env
          (ExceptT err IO)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState st
    , MonadReader env
    , MonadError err
    , MonadIO
    )

runAgent
  :: env
  -> st
  -> AgentM env st err a
  -> IO (Either err (a, st))
runAgent env st (AgentM m) =
  runExceptT $
    runReaderT
      (runStateT m st)
      env

evalAgent
  :: env
  -> st
  -> AgentM env st err a
  -> IO (Either err a)
evalAgent env st (AgentM m) =
  runExceptT $
    runReaderT
      (evalStateT m st)
      env

-- -- Tool instance
-- data ToolInstance where
--   ToolInstance :: Show a =>
--     (Map.Map T.Text Value -> IO a) -> ToolInstance
--
-- runTool :: ToolInstance -> Map.Map T.Text Value -> IO T.Text
-- runTool (ToolInstance f) params = do
--   result <- f params
--   return $ T.pack $ show result
--
-- -- The function takes a message, the tool map and performs the tool call
-- -- to generate a list of tool messages. If the input message is not AIMessage, return Nothing
-- callToolsAndGenerateMessages
--   :: Message
--   -> Map.Map T.Text ToolInstance
--   -> IO (Maybe [Message])
-- callToolsAndGenerateMessages (AIMessage _ ts) toolMap = do
--   toolResults <- liftIO $
--     mapM (
--       \(ToolCall _ name args) ->
--         case Map.lookup name toolMap of
--           Just tool -> do
--             result <- runTool tool args
--             return $ Just result
--
--           Nothing -> return Nothing
--     ) ts
--
--   let toolIDs = map (\(ToolCall iD _ _) -> iD) ts
--       toolMessages = catMaybes $ zipWith f toolResults toolIDs
--         where
--           f (Just res) iD = Just $ ToolMessage res iD
--           f Nothing _     = Nothing
--
--   return $ Just toolMessages
--
-- callToolsAndGenerateMessages _ _ = return Nothing
