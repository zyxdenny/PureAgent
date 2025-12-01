module Agent.Core where

import qualified Data.Text as T
import Data.Map.Strict (Map)
import Data.Default (Default(..))
import Data.Aeson (Value(..))

type ToolID = String

data ToolCallInfo = ToolCallInfo
  { toolCallID     :: ToolID
  , toolCalledName :: String
  , toolCalledArgs :: [String]
  } deriving Show

type Content = T.Text

data Message =
    AIMessage   Content [ToolCallInfo]
  | UserMessage Content
  | SysMessage  Content
  | ToolMessage Content ToolID
  deriving Show

data ArgInfo = ArgInfo
  { paramName :: String
  , paramType :: String
  , paramDesc :: String
  } deriving Show

data Tool = Tool
  { toolName :: String
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
  , extraParams       :: Map T.Text Value 
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

data LLM = LLM
  { invoke :: GenerationConfig -> [Tool] -> [Message] -> IO (Maybe Message)
  }
