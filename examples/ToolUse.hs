{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Default (def)

import Agent.Core
import Agent.Agent

getWeatherTool :: Tool
getWeatherTool = Tool tool schema
  where
    tool = ToolInstance $ do
      city <- getParam @T.Text "city"
      return $ "It's always sunny in " <> city

    schema = ToolSchema
      { toolName = "get_weather"
      , toolDesc = "Get the current weather of a city."
      , toolArgs = [ArgInfo "city" ArgString "The city to be queried"]
      }

type MyAgentM = AgentM () BaseAgentState BaseAgentError

weatherAgent :: MyAgentM ()
weatherAgent =
  createAgent $ def
    { modelName = "gpt-5-nano"
    , toolReg = Just $ registerTools [getWeatherTool]
    , instr = Just "You are an assiatant for weather queries. Only answer questions about weather."
    }

main :: IO ()
main = do
  result <- runAgent () initState weatherAgent
  case result of
    Left err ->
      print "An error has happened" 

    Right ((), BaseAgentState h) ->
      print h

    where
      initState = BaseAgentState { history = [ UserMessage "What's the weather like in HK?" ] }
