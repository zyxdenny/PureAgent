module Agent.Core where

import qualified Data.Text as T
import Data.Map.Strict (Map)
import Data.Default (Default(..))
import Data.Aeson (Value(..))
import Control.Exception (IOException)

type ToolID = T.Text

-- | Represents a request FROM the AI to run a tool
data ToolCall = ToolCall
  { tcID   :: ToolID
  , tcName :: T.Text
  , tcArgs :: Map T.Text Value
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
  { invoke :: GenerationConfig -> [Tool] -> [Message] -> IO (Either T.Text Message)
  }
