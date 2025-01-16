{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Neo4j where

import Database.Bolt
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)

-- Tipos de datos para el grafo
data DocumentNode = DocumentNode
  { docId :: Text
  , title :: Text
  , content :: Text
  , category :: Text
  } deriving (Show)

data RelationType = REFERENCES | REQUIRES | EXPLAINS
  deriving (Show, Eq)

-- Configuración de la conexión
data Neo4jConfig = Neo4jConfig
  { host :: Text
  , port :: Int
  , user :: Text
  , password :: Text
  }

-- Inicializar conexión
initNeo4j :: Neo4jConfig -> IO Pipe
initNeo4j Neo4jConfig{..} = connect $ def
  { host = host
  , port = port
  , user = user
  , password = password
  }

-- Consultas básicas
createDocument :: MonadIO m => Pipe -> DocumentNode -> m Bool
createDocument pipe DocumentNode{..} = do
  let query = "CREATE (d:Document {docId: $docId, title: $title, content: $content, category: $category})"
  let params = [ ("docId", T $ docId)
               , ("title", T $ title)
               , ("content", T $ content)
               , ("category", T $ category)
               ]
  run pipe $ queryP query params
  return True

findRelatedDocuments :: MonadIO m => Pipe -> Text -> m [DocumentNode]
findRelatedDocuments pipe docId = do
  let query = "MATCH (d:Document)-[:REFERENCES|REQUIRES|EXPLAINS]-(r:Document) WHERE d.docId = $docId RETURN r"
  records <- run pipe $ queryP query [("docId", T docId)]
  return $ map recordToDocument records
  where
    recordToDocument record = DocumentNode
      { docId = record ! "docId"
      , title = record ! "title"
      , content = record ! "content"
      , category = record ! "category"
      }

-- Funciones auxiliares para el chatbot
getContextForQuery :: MonadIO m => Pipe -> Text -> m [Text]
getContextForQuery pipe query = do
  let cypher = "MATCH (d:Document) WHERE d.content CONTAINS $query RETURN d.content LIMIT 5"
  records <- run pipe $ queryP cypher [("query", T query)]
  return $ map (\r -> r ! "d.content") records 