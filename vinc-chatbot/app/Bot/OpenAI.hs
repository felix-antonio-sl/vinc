{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.OpenAI where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Simple
import Control.Monad.IO.Class (MonadIO, liftIO)

-- Configuración
data Config = Config
  { apiKey :: Text
  , model :: Text
  , temperature :: Float
  } deriving (Show, Generic)

-- Tipos de datos para la API
data ChatRequest = ChatRequest
  { messages :: [Message]
  , modelName :: Text
  , temp :: Float
  } deriving (Show, Generic)

data Message = Message
  { role :: Text
  , content :: Text
  } deriving (Show, Generic)

data ChatResponse = ChatResponse
  { choices :: [Choice]
  } deriving (Show, Generic)

data Choice = Choice
  { message :: Message
  } deriving (Show, Generic)

instance ToJSON ChatRequest where
  toJSON ChatRequest{..} = object
    [ "messages" .= messages
    , "model" .= modelName
    , "temperature" .= temp
    ]

instance FromJSON ChatResponse
instance FromJSON Choice
instance ToJSON Message
instance FromJSON Message

-- Función principal para obtener completions
getCompletion :: MonadIO m => Config -> Text -> m Text
getCompletion Config{..} prompt = do
  let request = ChatRequest
        { messages = [Message "user" prompt]
        , modelName = model
        , temp = temperature
        }

  response <- liftIO $ do
    initReq <- parseRequest "POST https://api.openai.com/v1/chat/completions"
    let req = setRequestBodyJSON request
            $ addRequestHeader "Authorization" ("Bearer " <> encodeUtf8 apiKey)
            $ setRequestHeader "Content-Type" ["application/json"]
            $ initReq
    
    httpJSON req

  let ChatResponse choicesList = getResponseBody response
  return $ content . message $ head choicesList

-- Configuración por defecto
defaultConfig :: Text -> Config
defaultConfig key = Config
  { apiKey = key
  , model = "gpt-4o"
  , temperature = 0
  }