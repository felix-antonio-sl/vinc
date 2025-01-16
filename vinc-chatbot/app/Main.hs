{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Web.Scotty as S
import Network.Wai.Middleware.Cors
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import System.Environment (getEnv)
import qualified Bot.Core as Bot
import qualified Bot.OpenAI as OpenAI
import qualified Database.Neo4j as Neo4j
import qualified Data.Text as Text
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified System.IO as IO
import Control.Exception (catch, SomeException)
import qualified Configuration.Dotenv as Dotenv

data ChatRequest = ChatRequest 
    { userMessage :: Text.Text
    , reqSessionId :: Text.Text
    } deriving (Generic, Show)

data ChatResponse = ChatResponse
    { botMessage :: Text.Text
    , respSessionId :: Text.Text
    } deriving (Generic, Show)

instance ToJSON ChatRequest
instance FromJSON ChatRequest
instance ToJSON ChatResponse 
instance FromJSON ChatResponse

main :: IO ()
main = do
    putStrLn "Iniciando servidor VINC chatbot..."
    
    -- Cargar variables de entorno desde .env
    Dotenv.loadFile Dotenv.defaultConfig
    putStrLn "Variables de entorno cargadas"

    -- Obtener variables de entorno
    portStr <- getEnv "PORT" `catch` handleMissing "PORT"
    let port = read portStr :: Int

    openaiKey <- Text.pack <$> getEnv "OPENAI_API_KEY" `catch` handleMissing "OPENAI_API_KEY"
    neo4jUri <- getEnv "NEO4J_URI" `catch` handleMissing "NEO4J_URI"
    neo4jUser <- getEnv "NEO4J_USER" `catch` handleMissing "NEO4J_USER"
    neo4jPass <- getEnv "NEO4J_PASSWORD" `catch` handleMissing "NEO4J_PASSWORD"

    let botConfig = Bot.BotConfig
            { Bot.openAIConfig = OpenAI.defaultConfig openaiKey
            , Bot.dbConfig = Neo4j.defaultConfig 
                { Neo4j.uri = neo4jUri
                , Neo4j.user = neo4jUser
                , Neo4j.password = neo4jPass
                }
            }

    -- Inicializar el bot
    initialCtx <- Bot.initBot botConfig

    S.scotty port $ do
        S.middleware simpleCors

        S.post "/chat" $ do
            chatReq <- S.jsonData
            result <- S.liftIO $ catch 
                (do
                    Bot.processMessage botConfig initialCtx (userMessage chatReq)
                )
                (\e -> do
                    putStrLn $ "Error inesperado: " ++ show (e :: SomeException)
                    return ("Error al procesar el mensaje. Por favor, intente más tarde.", initialCtx)
                )
            S.json ChatResponse
                { botMessage = fst result
                , respSessionId = Bot.sessionId (snd result)
                }

    putStrLn $ "Servidor escuchando en puerto " ++ show port

-- Manejar variables de entorno faltantes
handleMissing :: String -> SomeException -> IO String
handleMissing varName _ = do
    putStrLn $ "Error: La variable de entorno '" ++ varName ++ "' no está definida."
    IO.hFlush IO.stdout
    error $ "Variable de entorno faltante: " ++ varName 