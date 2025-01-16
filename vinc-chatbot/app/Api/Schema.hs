{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Api.Schema where

import Data.Morpheus.Types
import GHC.Generics
import Data.Text (Text)

-- Tipos de datos para las consultas
data ChatInput = ChatInput
  { message :: Text
  , sessionId :: Text
  } deriving (Generic)

data ChatResponse = ChatResponse
  { response :: Text
  , context :: [Text]
  , suggestions :: [Text]
  } deriving (Generic)

-- Query
data Query m = Query
  { chatHistory :: ChatArgs -> m [ChatResponse]
  } deriving (Generic)

data ChatArgs = ChatArgs
  { sessionId :: Text
  } deriving (Generic)

-- Mutation
data Mutation m = Mutation
  { sendMessage :: ChatInput -> m ChatResponse
  } deriving (Generic)

-- Resolvers
rootResolver :: RootResolver IO () Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver = Query
        { chatHistory = \ChatArgs{} -> pure [] -- Implementar la lógica
        }
    , mutationResolver = Mutation
        { sendMessage = \ChatInput{} ->
            pure ChatResponse
              { response = "Respuesta de prueba"
              , context = []
              , suggestions = []
              }
        }
    , subscriptionResolver = undefined
    }