{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Web.Scotty as S
import Network.Wai.Middleware.Cors
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import System.Environment (getEnv)
import qualified Database.Neo4j as Neo4j
import qualified OpenAI
import qualified Bot.Core as Bot
import qualified Bot.OpenAI as OpenAI
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import qualified Data.Text as T

-- Configuración
data AppConfig = AppConfig
  { neo4jUri :: String
  , openaiKey :: String
  , port :: Int
  } deriving (Show)

-- Inicializar configuración
initConfig :: IO AppConfig
initConfig = do
  neo4jUri <- getEnv "VINC_NEO4J_URI"
  openaiKey <- getEnv "VINC_OPENAI_KEY"
  port <- read <$> getEnv "VINC_PORT"
  return $ AppConfig neo4jUri openaiKey port

-- Tipo de datos para recibir mensajes
data ChatRequest = ChatRequest
  { userMessage :: Text
  , sessionId :: Text
  } deriving (Show, Generic)

instance FromJSON ChatRequest

-- Tipo de datos para responder al usuario
data ChatResponse = ChatResponse
  { botMessage :: Text
  , sessionId :: Text
  } deriving (Show, Generic)

instance ToJSON ChatResponse

main :: IO ()
main = do
  config <- initConfig
  putStrLn $ "Iniciando servidor en puerto " ++ show (port config)
  
  -- Configuración de BotConfig
  let botConfig = Bot.BotConfig
        { dbConfig = Neo4j.Neo4jConfig { uri = neo4jUri config }
        , openAIConfig = OpenAI.defaultConfig (T.pack $ openaiKey config)
        }

  S.scotty (port config) $ do
    -- Configurar CORS
    S.middleware simpleCors
    
    -- Ruta principal
    S.get "/" $ do
      S.text "VINC Chatbot API v1.0"
    
    -- Ruta para consultas del chatbot
    S.post "/api/chat" $ do
      chatReq <- S.jsonData :: S.ActionM ChatRequest
      let userMsg = userMessage chatReq
          sessId = sessionId chatReq
      
      -- Inicializar o recuperar el contexto de la sesión
      ctx <- liftIO $ Bot.initBot botConfig
      
      -- Procesar el mensaje
      (response, newCtx) <- liftIO $ Bot.processMessage botConfig ctx (T.unpack userMsg)
      
      -- Responder al usuario
      S.json $ ChatResponse (T.pack response) sessId 