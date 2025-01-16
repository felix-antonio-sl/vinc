{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Core where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Database.Neo4j as DB
import qualified Bot.OpenAI as OpenAI
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Object)

-- Tipos de datos para el chatbot
data ChatContext = ChatContext
  { sessionId :: Text
  , history :: [ChatMessage]
  , relevantDocs :: [DB.DocumentNode]
  }

data ChatMessage = ChatMessage
  { role :: Text
  , content :: Text
  }

-- Configuración del chatbot
data BotConfig = BotConfig
  { dbConfig :: DB.Neo4jConfig
  , openAIConfig :: OpenAI.Config
  }

-- Inicializar chatbot
initBot :: BotConfig -> IO ChatContext
initBot config = do
  -- Inicializar conexiones y estado inicial
  return ChatContext
    { sessionId = "new-session"
    , history = []
    , relevantDocs = []
    }

-- Procesar mensaje
processMessage :: MonadIO m => BotConfig -> ChatContext -> Text -> m (Text, ChatContext)
processMessage config ctx message = do
  -- 1. Establecer conexión con Neo4j
  pipe <- liftIO $ DB.connectDB (dbConfig config)
  
  -- 2. Buscar documentos relevantes en Neo4j
  textDocs <- DB.getContextForQuery pipe message
  -- Convertir los resultados de texto en DocumentNodes
  let docs = map (\text -> DB.DocumentNode { DB.content = text }) textDocs
  
  -- 3. Preparar contexto para OpenAI
  let prompt = preparePrompt ctx docs message
  
  -- 4. Obtener respuesta de OpenAI
  response <- OpenAI.getCompletion (openAIConfig config) prompt
  
  -- 5. Actualizar contexto
  let newCtx = updateContext ctx message response docs
  
  return (response, newCtx)

-- Funciones auxiliares
preparePrompt :: ChatContext -> [DB.DocumentNode] -> Text -> Text
preparePrompt ChatContext{..} docs userMessage =
  T.unlines
    [ "Eres un asistente especializado en el Concurso VINC 8% 2024."
    , "Contexto relevante:"
    , T.unlines (map DB.content docs)
    , "Historial de conversación:"
    , formatHistory history
    , "Usuario: " <> userMessage
    ]

formatHistory :: [ChatMessage] -> Text
formatHistory = T.unlines . map formatMessage
  where
    formatMessage ChatMessage{..} = role <> ": " <> content

-- Función para actualizar el contexto (asume que existe)
updateContext :: ChatContext -> Text -> Text -> [DB.DocumentNode] -> ChatContext
updateContext ctx msg resp docs =
  ctx { history = history ctx ++ [ChatMessage "Usuario" msg, ChatMessage "Bot" resp]
      , relevantDocs = docs
      } 