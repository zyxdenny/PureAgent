{-# LANGUAGE OverloadedStrings #-}

module Agent.OpenAI where

import Agent.Core
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe, Pair) -- Import Pair
import qualified Data.Aeson.Key as Key            -- Import Key logic
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Network.HTTP.Simple
import Data.Maybe (catMaybes)
import Control.Exception (try, IOException)
import qualified Data.Vector as V

type APIKey = String
type ModelName = String

-- | Constructor
makeOpenAI :: APIKey -> ModelName -> LLM
makeOpenAI apiKey modelName = LLM
  { invoke = \conf tools msgs -> runOpenAI apiKey modelName conf tools msgs
  }

-- | Main execution logic
runOpenAI :: APIKey -> ModelName -> GenerationConfig -> [Tool] -> [Message] -> IO (Either T.Text Message)
runOpenAI apiKey model conf tools msgs = do
    let payload = object $
            [ "model"       .= model
            , "messages"    .= map messageToOpenAI msgs
            ]
            ++ configToJSON conf
            ++ if null tools 
                 then [] 
                 else [ "tools" .= map toolToOpenAI tools
                      , "tool_choice" .= ("auto" :: String) 
                      ]

    req <- parseRequest "POST https://api.openai.com/v1/chat/completions"
    let req' = setRequestHeader "Authorization" ["Bearer " <> BS.pack apiKey]
             $ setRequestHeader "Content-Type" ["application/json"]
             $ setRequestBodyJSON payload
             $ req

    result <- try (httpJSON req') :: IO (Either IOException (Response Value))
    
    case result of
        Left err -> return $ Left $ T.pack $ show err 
        Right response -> do
            let responseBody = getResponseBody response
            
            -- Attempt to parse
            case parseOpenAIResponse responseBody of
                Just message -> return $ Right message
                Nothing -> do
                    -- Print the raw JSON to stdout for debugging
                    putStrLn "\n[Error] Failed to parse OpenAI Response:"
                    print responseBody 
                    putStrLn "----------------------------------------\n"
                    
                    return $ Left "Error: Can't parse response"

--------------------------------------------------------------------------------
-- Serialization (Haskell -> OpenAI JSON)
--------------------------------------------------------------------------------

messageToOpenAI :: Message -> Value
messageToOpenAI msg = case msg of
    SystemMessage content -> object ["role" .= ("system" :: String), "content" .= content]
    UserMessage content   -> object ["role" .= ("user" :: String), "content" .= content]
    ToolMessage content tid -> object
        [ "role"         .= ("tool" :: String)
        , "tool_call_id" .= tid
        , "content"      .= content
        ]
    AIMessage content toolCalls -> object
        [ "role"       .= ("assistant" :: String)
        , "content"    .= content
        , "tool_calls" .= if null toolCalls then Null else toJSON (map toolCallToOpenAI toolCalls)
        ]

toolCallToOpenAI :: ToolCall -> Value
toolCallToOpenAI tc = object
    [ "id"   .= tcID tc
    , "type" .= ("function" :: String)
    , "function" .= object
        [ "name"      .= tcName tc
        , "arguments" .= argsText -- Serialize Map -> JSON String
        ]
    ]
  where
    argsText :: T.Text
    argsText = TE.decodeUtf8 $ LBS.toStrict $ encode (tcArgs tc)

toolToOpenAI :: Tool -> Value
toolToOpenAI tool = object
    [ "type" .= ("function" :: String)
    , "function" .= object
        [ "name"        .= toolName tool
        , "description" .= toolDesc tool
        , "parameters"  .= object
            [ "type"       .= ("object" :: String)
            , "properties" .= object (map argToProperty (toolArgs tool))
            , "required"   .= map argName (toolArgs tool)
            ]
        ]
    ]
  where
    -- FIX 1: Convert Text -> Key here
    argToProperty :: ArgInfo -> Pair
    argToProperty arg = (Key.fromText (argName arg), object 
        [ "type"        .= argType arg
        , "description" .= argDesc arg
        ])

configToJSON :: GenerationConfig -> [Pair]
configToJSON conf = catMaybes
    [ ("temperature" .=) <$> temperature conf
    , ("max_tokens"  .=) <$> maxTokens conf
    , ("top_p"       .=) <$> topP conf
    , ("stop"        .=) <$> if null (stopSequences conf) then Nothing else Just (stopSequences conf)
    , ("seed"        .=) <$> seed conf
    ] 
    ++ (if jsonMode conf then [("response_format" .= object ["type" .= ("json_object" :: String)])] else [])
    ++ map toPair (Map.toList (extraParams conf))
  where
    -- Convert (Text, Value) -> (Key, Value)
    toPair (k, v) = (Key.fromText k, v)

--------------------------------------------------------------------------------
-- Parsing (OpenAI JSON -> Haskell)
--------------------------------------------------------------------------------

parseOpenAIResponse :: Value -> Maybe Message
parseOpenAIResponse v = parseMaybe parser v
  where
    parser :: Value -> Parser Message
    parser val = withObject "Response" (\o -> do
        choices <- o .: "choices"
        case choices of
            (c:_) -> parseMessage =<< c .: "message"
            []    -> fail "No choices returned"
        ) val

    parseMessage :: Value -> Parser Message
    parseMessage val = withObject "Message" (\o -> do
        content <- o .:? "content" .!= ""
        rawTC   <- o .:? "tool_calls"
        parsedTC <- case rawTC of
            Just (Array arr) -> mapM parseToolCall arr
            _                -> return $ V.empty
        return $ AIMessage content (V.toList parsedTC)
        ) val

    parseToolCall :: Value -> Parser ToolCall
    parseToolCall val = withObject "ToolCall" (\o -> do
        tid  <- o .: "id"
        func <- o .: "function"
        name <- func .: "name"
        
        argsStr <- func .: "arguments" :: Parser T.Text
        
        let argsValue = case decodeStrict (TE.encodeUtf8 argsStr) of
              Just v -> v
              Nothing -> Null
        
        let argsMap = case argsValue of
              Object obj -> toMap obj
              _          -> mempty 
        
        return $ ToolCall tid name argsMap
        ) val
    
    -- Helper to convert Aeson Object to Map
    toMap :: Object -> Map.Map T.Text Value
    toMap obj = case fromJSON (Object obj) of
            Success m -> m
            Error _   -> mempty
