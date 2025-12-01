{-# LANGUAGE OverloadedStrings #-}

module Agent.FooAIModel where

import System.Random (getStdGen, StdGen, randomR)
import qualified Data.Array as A
import qualified Data.Text as T
import Agent.Core

data FooAI = FooAI { 
  getTools :: [Tool]
}

invokeFooAI :: [Message] -> [Tool] -> IO (Maybe Message)
invokeFooAI (UserMessage txt : _) tools =
    if elem "tool" $ map T.toLower $ T.words txt then do
      case tools of
        [] -> return Nothing
        t : _ -> return $ Just $ AIMessage "" [ToolCallInfo "1" (toolName t) args]
          where
            args = map show [1..(length $ toolArgs t)]
    else do
      let greetings = ["Hello!", "How are you?", "Nice to meet you!", "Fuck you!"] :: [T.Text]
      let n = length greetings
      let greetingsArray = A.array (0, n - 1) $ zip [0..n - 1] greetings
      gen <- getStdGen
      let (idx, _) = randomR (0, n - 1) gen :: (Int, StdGen)
      return $ Just $ AIMessage (greetingsArray A.! idx) []

invokeFooAI _ _ = return Nothing

instance LLM FooAI where
  init = FooAI []

  invoke model msgs = do
    let tools = getTools model
    response <- invokeFooAI msgs tools
    return response

  bindTools model tools = model { getTools = tools }
