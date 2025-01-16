{-# LANGUAGE OverloadedStrings #-}

module Database.Neo4j where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Database.Bolt as Bolt
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default.Class (Default(..))
import Data.Maybe (catMaybes)
import Network.URI (parseURI, URI(..), URIAuth(..))
import Text.Read (readMaybe)

-- Configuración de Neo4j
data Neo4jConfig = Neo4jConfig
  { uri      :: String
  , user     :: String
  , password :: String
  } deriving (Show)

-- Representación de un nodo de documento
data DocumentNode = DocumentNode
  { content :: Text
  }

-- Actualizar la configuración para asegurar la conexión correcta
defaultConfig :: Neo4jConfig
defaultConfig = Neo4jConfig
    { uri = "bolt://localhost:7687"    -- Cambiado de 0.0.0.0 a localhost
    , user = "neo4j"
    , password = "password"
    }

-- Función para conectar a Neo4j usando Hasbolt
connectDB :: MonadIO m => Neo4jConfig -> m Bolt.Pipe
connectDB config = liftIO $ do
    putStrLn $ "Intentando conectar a Neo4j 5.26.0 en: " ++ uri config
    pipe <- Bolt.connect $ def
        { Bolt.host = "localhost"
        , Bolt.port = 7687
        , Bolt.user = T.pack (user config)
        , Bolt.password = T.pack (password config)
        , Bolt.version = Bolt.V5_0     -- Actualizado para Neo4j 5.x
        }
    putStrLn "Conexión establecida con Neo4j"
    return pipe

-- Función para ejecutar una consulta Cypher y devolver líneas de texto
runCypher ::
  MonadIO m =>
  Bolt.Pipe ->
  Text ->
  [(Text, Bolt.Value)] ->
  m [Text]
runCypher pipe cypher paramsList = do
  let paramMap = M.fromList paramsList
  records <- liftIO . Bolt.run pipe $ Bolt.queryP cypher paramMap
  maybeVals <- liftIO $ mapM extractContent records -- IO [Maybe Text]
  return (catMaybes maybeVals)                     -- Filtramos Nothing
  where
    extractContent :: Bolt.Record -> IO (Maybe Text)
    extractContent rec = do
      val <- Bolt.run pipe (rec `Bolt.at` "d.content")
      case val of
        Bolt.T txt -> return (Just txt)
        _          -> return Nothing

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