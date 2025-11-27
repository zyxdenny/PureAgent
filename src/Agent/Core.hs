{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Core where

import qualified Data.Text as T
import Data.Hashable (hash)
import System.Random (mkStdGen, StdGen, randomR)
import qualified Data.Array as A

data Role = System | User | Assistant | Tool deriving Show
data Message = Message { role :: Role, content :: T.Text } deriving Show

class LLM llm where
  generate :: llm -> T.Text -> IO (Maybe T.Text)
  chat :: llm -> [Message] -> IO (Maybe Message)

data FooAI = FooAI

instance LLM FooAI where
  generate _ txt = do
    let greetings = ["Hello!", "How are you?", "Nice to meet you!", "Fuck you!"] :: [T.Text]
    let n = length greetings
    let greetingsArray = A.array (0, n - 1) $ zip [0..n - 1] greetings
    let gen = mkStdGen (hash txt)
    let (idx, _) = randomR (0, n - 1) gen :: (Int, StdGen)
    return $ Just (greetingsArray A.! idx)

  chat fooAI (Message _ txt : _) = do
    maybeResponse <- generate fooAI txt
    case maybeResponse of
      Nothing       -> return Nothing
      Just response -> return $ Just (Message Assistant response)

  chat _ [] = return Nothing
