{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Agent.Core where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Default (Default(..))
import Data.Aeson (Value(..))
import Network.HTTP.Client (HttpException)
import Data.Maybe
import Data.Proxy
import Data.Scientific (toBoundedInteger, toRealFloat)

type ToolID = T.Text

-- | Represents a request FROM the AI to run a tool
data ToolCall = ToolCall
  { tcID   :: ToolID
  , tcName :: T.Text
  , tcArgs :: Map.Map T.Text Value
  } deriving (Show, Eq)

data Message
  = SystemMessage T.Text
  | UserMessage T.Text
  | AIMessage T.Text [ToolCall] 
  | ToolMessage T.Text ToolID 
  deriving (Show, Eq)

data ArgInfo = ArgInfo
  { argName :: T.Text
  , argType :: T.Text
  , argDesc :: T.Text
  } deriving Show

data GenerationConfig = GenerationConfig
  { -- | Core Parameters (Supported by almost all LLMs)
    temperature       :: Maybe Double -- 0.0 (deterministic) to 2.0 (random)
  , maxTokens         :: Maybe Int    -- Limit response length
  , topP              :: Maybe Double -- Nucleus sampling
  , stopSequences     :: [T.Text]       -- Stop generating when these appear
  
    -- | Behavior Modifiers
  , jsonMode          :: Bool         -- Force valid JSON output
  , seed              :: Maybe Int    -- For deterministic reproducibility
  
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
           -> ToolRegistry
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

--
--
--
--
-- Tools
type Params = Map.Map T.Text Value

data ToolError
  = MissingParam T.Text
  | TypeMismatch
      { paramName    :: T.Text
      , expectedType :: T.Text
      , actualValue  :: Value
      }
  | ToolSpecificError T.Text
  deriving (Show)

newtype ToolM a = ToolM
  { runToolM :: ReaderT Params (ExceptT ToolError IO) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Params
    , MonadError ToolError
    , MonadIO
    )

data ToolSchema = ToolSchema
  { toolName :: T.Text
  , toolDesc :: T.Text
  , toolArgs :: [ArgInfo]
  } deriving Show

data ToolInstance = forall a. Show a => ToolInstance (ToolM a)

data Tool = Tool
  { toolInstance :: ToolInstance
  , toolSchema   :: ToolSchema
  }

type ToolRegistry = Map.Map T.Text (ToolSchema, ToolInstance)

registerTools :: [Tool] -> ToolRegistry
registerTools ts =
  Map.fromList $ map f ts
    where
      f (Tool inst schema) =
        (toolName schema, (schema, inst))

runTool :: ToolInstance -> Params -> IO T.Text
runTool (ToolInstance (ToolM rawTool)) params = do
  result <- liftIO $ runExceptT $ runReaderT rawTool params
  return $ T.pack $
    case result of
      Left  err -> show err
      Right ans -> show ans

-- The function takes a message, the tool map and performs the tool call
-- to generate a list of tool messages. If the input message is not AIMessage, return Nothing
callToolsAndGenerateMessages
  :: Message
  -> ToolRegistry
  -> IO (Maybe [Message])
callToolsAndGenerateMessages (AIMessage _ ts) toolRegistry = do
  toolResults <- liftIO $
    mapM (
      \(ToolCall _ name args) ->
        case Map.lookup name toolRegistry of
          Just (_, tool) -> do
            result <- runTool tool args
            return $ Just result

          Nothing -> return Nothing
    ) ts

  let toolIDs = map (\(ToolCall iD _ _) -> iD) ts
      toolMessages = catMaybes $ zipWith f toolResults toolIDs
        where
          f (Just res) iD = Just $ ToolMessage res iD
          f Nothing _     = Nothing

  return $ Just toolMessages

callToolsAndGenerateMessages _ _ = return Nothing

class ToolArgType a where
  paramType :: proxy a -> T.Text
  fromValue :: Value -> Maybe a

getParam :: forall a. ToolArgType a => T.Text -> ToolM a
getParam name = do
  params <- ask
  case Map.lookup name params of
    Nothing ->
      throwError (MissingParam name)

    Just v ->
      case fromValue @a v of
        Just x  -> pure x
        Nothing ->
          throwError $
            TypeMismatch name (paramType (Proxy @a)) v

-- A list of valid tool types
instance ToolArgType T.Text where
  paramType _ = "string"
  fromValue (String t) = Just t
  fromValue _          = Nothing

instance ToolArgType Int where
  paramType _ = "int"
  fromValue (Number n) = toBoundedInteger n
  fromValue _          = Nothing

instance ToolArgType Float where
  paramType _ = "float"
  fromValue (Number n) = Just (toRealFloat n)
  fromValue _          = Nothing

instance ToolArgType Bool where
  paramType _ = "bool"
  fromValue (Bool b) = Just b
  fromValue _        = Nothing

instance ToolArgType a => ToolArgType (Maybe a) where
  paramType _ = paramType (Proxy @a)
  fromValue Null = Just Nothing
  fromValue v    = Just <$> fromValue @a v

instance ToolArgType a => ToolArgType [a] where
  paramType _ = "array"
  fromValue (Array arr) =
    traverse (fromValue @a) (V.toList arr)
  fromValue _ = Nothing

instance ToolArgType Value where
  paramType _ = "json"
  fromValue v = Just v
