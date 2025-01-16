{-# LANGUAGE OverloadedStrings #-}

module Database.Neo4j where

import Data.Text (Text)
import qualified Data.Text as T
import Database.Bolt
import Control.Monad.IO.Class (MonadIO, liftIO)

-- Configuración de Neo4j
data Neo4jConfig = Neo4jConfig
  { uri :: String  -- O bien puedes guardar "user", "password", etc.
  }

-- Representación de un nodo de documento
data DocumentNode = DocumentNode
  { content :: Text
  }

-- Función para conectar a Neo4j usando Hasbolt
connect :: MonadIO m => Neo4jConfig -> m Pipe
connect config = liftIO $ do
  -- Ajusta user, password, host, port, etc. según tu entorno
  pipe <- acquire $ def
    { user = "neo4j"
    , password = "password"
    , host = "localhost"
    , port = 7687
    }
  return pipe

-- Función para ejecutar una consulta Cypher y devolver líneas de texto
runCypher :: MonadIO m => Pipe -> Text -> [(Text, Value)] -> m [Text]
runCypher pipe cypher params = do
  records <- liftIO $ run pipe $ Query cypher params
  return $ mapMaybe (recordToText "d.content") records

-- Función auxiliar para convertir un campo de nombre "d.content" al tipo Text
recordToText :: Text -> Record -> Maybe Text
recordToText fieldName record =
  case recordValue fieldName record of
    Just (T x) -> Just x
    _          -> Nothing

-- Una función de ejemplo para filtrar documentos relevantes
getContextForQuery :: MonadIO m => Pipe -> Text -> m [Text]
getContextForQuery pipe queryTxt = runCypher pipe cypher params
  where
    cypher = "MATCH (d:Document) \
             \WHERE d.content CONTAINS $query \
             \RETURN d.content LIMIT 5"
    -- La librería Hasbolt usa Value para parámetros
    params = [("query", T queryTxt)] 