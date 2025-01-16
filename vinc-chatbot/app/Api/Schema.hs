{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

-- Resolvers
rootResolver :: RootResolver IO () Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {..}
    , mutationResolver = Mutation {..}
    , subscriptionResolver = Undefined
    }

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

instance Monad m => MapAPI Query m where
  type API Query m = API Query m

instance Monad m => MapAPI Mutation m where
  type API Mutation m = API Mutation m 