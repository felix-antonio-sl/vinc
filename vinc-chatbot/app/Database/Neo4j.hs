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

-- Función para conectar a Neo4j usando Hasbolt
connectDB :: MonadIO m => Neo4jConfig -> m Bolt.Pipe
connectDB config = liftIO $ do
  let uriStr = uri config
  case parseURI uriStr of
    Just (URI { uriScheme = scheme, uriAuthority = Just auth }) ->
      if scheme /= "bolt:" 
        then error "La URI debe comenzar con 'bolt://'"
        else do
          let host = uriRegName auth
              portStr = drop 1 (uriPort auth)
              port = case portStr of
                      "" -> 7687
                      str -> case readMaybe str of
                              Just p  -> p
                              Nothing -> error $ "Puerto inválido en la URI de Neo4j: " ++ str
          pipe <- Bolt.connect $ def
            { Bolt.user     = T.pack (user config)
            , Bolt.password = T.pack (password config)
            , Bolt.host     = host
            , Bolt.port     = port
            }
          return pipe
    _ -> error "URI de Neo4j inválida. Debe tener el formato 'bolt://host:port'"

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