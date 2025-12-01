module Agent.Core where

import qualified Data.Text as T

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

class LLM model where
  init      :: model
  invoke    :: model -> [Message] -> IO (Maybe Message)
  bindTools :: model -> [Tool] -> model
