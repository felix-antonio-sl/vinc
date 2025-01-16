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
  -- 1. Buscar documentos relevantes en Neo4j
  let dbPipe = undefined -- TODO: Manejar conexión Neo4j
  docs <- DB.getContextForQuery dbPipe message
  
  -- 2. Preparar contexto para OpenAI
  let prompt = preparePrompt ctx docs message
  
  -- 3. Obtener respuesta de OpenAI
  response <- OpenAI.getCompletion (openAIConfig config) prompt
  
  -- 4. Actualizar contexto
  let newCtx = updateContext ctx message response docs
  
  return (response, newCtx)

-- Funciones auxiliares
preparePrompt :: ChatContext -> [Text] -> Text -> Text
preparePrompt ChatContext{..} docs userMessage =
  T.unlines
    [ "Eres un asistente especializado en el Concurso VINC 8% 2024."
    , "Contexto relevante:"
    , T.unlines docs
    , "Historial de conversación:"
    , formatHistory history
    , "Usuario: " <> userMessage
    ]

formatHistory :: [ChatMessage] -> Text
formatHistory = T.unlines . map formatMessage
  where
    formatMessage ChatMessage{..} = role <> ": " <> content

updateContext :: ChatContext -> Text -> Text -> [Text] -> ChatContext
updateContext ctx@ChatContext{..} userMsg botResponse _ =
  ctx { history = history ++ 
          [ ChatMessage "user" userMsg
          , ChatMessage "assistant" botResponse
          ]
      } 