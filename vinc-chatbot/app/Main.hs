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

main :: IO ()
main = do
  config <- initConfig
  putStrLn $ "Iniciando servidor en puerto " ++ show (port config)
  
  S.scotty (port config) $ do
    -- Configurar CORS
    S.middleware simpleCors
    
    -- Ruta principal
    S.get "/" $ do
      S.text "VINC Chatbot API v1.0"
    
    -- Ruta para consultas del chatbot
    S.post "/api/chat" $ do
      -- TODO: Implementar lógica del chatbot
      S.json ("Chatbot en desarrollo" :: String) 