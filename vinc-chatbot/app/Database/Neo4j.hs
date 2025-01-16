{-# LANGUAGE OverloadedStrings #-}

module Database.Neo4j where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Database.Bolt as Bolt
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default.Class (Default(..))
import Data.Maybe (mapMaybe)

-- Configuración de Neo4j
data Neo4jConfig = Neo4jConfig
  { uri :: String
  }

-- Representación de un nodo de documento
data DocumentNode = DocumentNode
  { content :: Text
  }

-- Función para conectar a Neo4j usando Hasbolt
connectDB :: MonadIO m => Neo4jConfig -> m Bolt.Pipe
connectDB _config = liftIO $ do
  pipe <- Bolt.connect $ def
    { Bolt.user = "neo4j"
    , Bolt.password = "password"
    , Bolt.host = "localhost"
    , Bolt.port = 7687
    }
  return pipe

-- Función para ejecutar una consulta Cypher y devolver líneas de texto
runCypher ::
  MonadIO m =>
  Bolt.Pipe ->
  Text ->               -- Consulta Cypher
  [(Text, Bolt.Value)] ->  -- Parámetros en forma de lista
  m [Text]
runCypher pipe cypher paramsList = do
  -- Convertimos la lista ‘[(Text, Bolt.Value)]’ a ‘Map Text Bolt.Value’
  let paramMap = M.fromList paramsList
  -- Usamos ‘Bolt.run pipe’ para ejecutar la acción de Hasbolt ‘queryP’
  records <- liftIO . Bolt.run pipe $ Bolt.queryP cypher paramMap
  return $ mapMaybe (extractContent "d.content") records
  where
    extractContent :: Text -> Bolt.Record -> Maybe Text
    extractContent field rec =
      case rec `Bolt.at` field of
        Right (Bolt.T txt) -> Just txt
        _                  -> Nothing

-- Función de ejemplo para filtrar documentos relevantes
getContextForQuery :: MonadIO m => Bolt.Pipe -> Text -> m [Text]
getContextForQuery pipe queryTxt =
  runCypher pipe cypher params
  where
    cypher = T.unlines
      [ "MATCH (d:Document)"
      , "WHERE d.content CONTAINS $query"
      , "RETURN d.content LIMIT 5"
      ]
    params = [("query", Bolt.T queryTxt)]