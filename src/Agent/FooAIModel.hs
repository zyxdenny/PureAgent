{-# LANGUAGE OverloadedStrings #-}

module Agent.FooAIModel where

import System.Random (getStdGen, StdGen, randomR)
import qualified Data.Array as A
import qualified Data.Text as T
import Data.Map.Strict (fromList)
import Data.Aeson (toJSON)
import Agent.Core

invokeFooAI :: [Message] -> [Tool] -> IO (Maybe Message)
invokeFooAI (UserMessage txt : _) tools =
    if elem "tool" $ map T.toLower $ T.words txt then do
      case tools of
        [] -> return Nothing
        t : _ -> return $ Just $ AIMessage "" [ToolCall "1" (toolName t) args]
          where
            args = fromList $ zip keys vals
            keys = map argName $ toolArgs t
            vals = map toJSON ([1..(length $ toolArgs t)] :: [Int])
    else do
      let greetings = ["Hello!", "How are you?", "Nice to meet you!", "Fuck you!"] :: [T.Text]
      let n = length greetings
      let greetingsArray = A.array (0, n - 1) $ zip [0..n - 1] greetings
      gen <- getStdGen
      let (idx, _) = randomR (0, n - 1) gen :: (Int, StdGen)
      return $ Just $ AIMessage (greetingsArray A.! idx) []

invokeFooAI _ _ = return Nothing

fooAI :: LLM
fooAI = LLM $ \_ tools msgs -> invokeFooAI msgs tools
