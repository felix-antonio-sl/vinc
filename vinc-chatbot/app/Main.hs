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
import qualified Data.Text.Lazy as T
import qualified Data.Text as Text

data ChatRequest = ChatRequest 
    { userMessage :: T.Text
    , sessionId :: T.Text 
    } deriving (Generic, Show)

data ChatResponse = ChatResponse
    { botMessage :: T.Text
    , sessionId :: T.Text
    } deriving (Generic, Show)

instance ToJSON ChatRequest
instance FromJSON ChatRequest
instance ToJSON ChatResponse 
instance FromJSON ChatResponse

main :: IO ()
main = do
    port <- read <$> getEnv "PORT"
    openaiKey <- Text.pack <$> getEnv "OPENAI_API_KEY"
    neo4jUri <- getEnv "NEO4J_URI"
    neo4jUser <- getEnv "NEO4J_USER" 
    neo4jPass <- getEnv "NEO4J_PASSWORD"

    let botConfig = Bot.BotConfig
            { Bot.openAIConfig = OpenAI.defaultConfig openaiKey
            , Bot.dbConfig = Neo4j.Config 
                { Neo4j.uri = neo4jUri
                , Neo4j.username = neo4jUser
                , Neo4j.password = neo4jPass
                }
            }

    S.scotty port $ do
        S.middleware simpleCors

        S.post "/chat" $ do
            chatReq <- S.jsonData
            let initialContext = Bot.ChatContext
                    { Bot.sessionId = sessionId chatReq
                    , Bot.history = []
                    , Bot.relevantDocs = []
                    }
            (response, newCtx) <- S.liftAndCatchIO $ 
                Bot.processMessage botConfig initialContext (userMessage chatReq)
            S.json ChatResponse
                { botMessage = response
                , sessionId = Bot.sessionId newCtx
                } 