{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Agent.Tool where

import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Proxy
import Data.Aeson (Value(..))
import Data.Scientific (toBoundedInteger, toRealFloat)

import Agent.Core

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

data ToolInstance = forall a. Show a => ToolInstance (ToolM a)

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
  -> Map.Map T.Text ToolInstance
  -> IO (Maybe [Message])
callToolsAndGenerateMessages (AIMessage _ ts) toolMap = do
  toolResults <- liftIO $
    mapM (
      \(ToolCall _ name args) ->
        case Map.lookup name toolMap of
          Just tool -> do
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
