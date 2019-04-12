{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Modules.Organization.Schema  where
  
import Protolude hiding (Enum)

import qualified Data.Aeson as Aeson

import           Data.Data
import           GHC.Generics
import qualified Data.ByteString               as B
import           Data.Generics.Twins            ( gzipWithT
                                                )
import qualified Data.Generics as G
import           GraphQL.Parser
import           GraphQL.AST
import           Text.RawString.QQ

-- import GraphQL.Value (FromValue, toValue)

-- data DogStuff = DogStuff { _toy :: Text, _likesTreats :: Bool } deriving (Show, Generic)
-- instance FromValue DogStuff
-- instance HasAnnotatedInputType DogStuff
-- instance Defaultable DogStuff where
--   defaultFor _ = Just (DogStuff "shoe" False)

-- type Query = Object "Query" '[]
--   '[ Argument "dogStuff" DogStuff :> Field "description" Text ]

-- root :: Handler IO Query
-- root = pure description

-- description :: DogStuff -> Handler IO Text
-- description (DogStuff toy likesTreats)
--   | likesTreats = pure $ "likes treats and their favorite toy is a " <> toy
--   | otherwise = pure $ "their favorite toy is a " <> toy

-- example :: Text -> IO Response
-- example = interpretAnonymousQuery @Query root


-- main :: IO ()
-- main = do
--   response <- example "{ description(dogStuff: {_toy: \"bone\", _likesTreats: true}) }"
--   putStrLn $ Aeson.encode $ toValue response
--   response' <- example "{ description }"
--   putStrLn $ Aeson.encode $ toValue response'

      
data NoArg = NoArg
data GQLAnswer a = GQLAnswer a
data SchemaT a = SchemaT a
data GQLQuery a = GQLQuery a
data GQLField returnType argumentType = GQLFieldC returnType
data GQLFieldType = GQLScalar | GQLNonScalar
data Queried argument selection = Asked argument | AskedSelection argument selection | Unasked

type family GQLType a
type instance GQLType (Identity (GQLField returnType argumentType) ) = returnType
type instance GQLType (SchemaT (GQLField returnType argumentType) ) = Maybe (argumentType -> returnType)
type instance GQLType (GQLQuery (GQLField returnType argumentType) ) = Queried argumentType argumentType
type instance GQLType (GQLAnswer (GQLField returnType argumentType) ) = Queried returnType argumentType

data Test a = Test  { id :: GQLType (a (GQLField Text ()) )
, more :: GQLType (a (GQLField (Maybe (More a)) ()))
, field_a :: GQLType (a (GQLField Text ()))
} deriving (Generic, Typeable)

data More a = More  { didi :: GQLType (a (GQLField Text ()) )
                    , momo :: GQLType (a (GQLField Text ()))
                    , yea :: GQLType (a (GQLField Integer ()))
} deriving (Generic, Typeable)

type AnotherTest = Test Identity
type MoreCore = More Identity

deriving instance Eq MoreCore
deriving instance Eq AnotherTest
deriving instance Typeable AnotherTest
-- deriving instance G.Data AnotherTest

type TestSchema = Test SchemaT
type TestQuery = Test GQLQuery
type TestAnswer = Test GQLAnswer

type MoreSchema = More SchemaT
type MoreQuery = More GQLQuery
type MoreAnswer = More GQLAnswer

olol = Test { id = "okok", field_a = "okoko", more = Nothing } :: AnotherTest

oyeah :: AnotherTest
oyeah = Test "okok" Nothing "koko" 

ouuk = [SelectionField (Field Nothing (Name "TPE") [] [] [])]

testResolve
  :: [Selection] -> TestSchema -> AnotherTest
testResolve selectionList schema =
  Test { 
    id = "ok", 
    field_a = "okoko", 
    more = Nothing 
  }



testSchema =
  Test { id = Just (\_ -> "ok" :: Text), field_a = Nothing, more = Nothing } :: TestSchema
omomomo =
    Test { id = Asked "okok", field_a = Unasked, more = Asked Nothing } :: TestAnswer

testQuery =
  Test { id = Asked (), field_a = Unasked, more = Asked () } :: TestQuery


-- data GQLQuery a = Requested a | Unrequested

  -- data IsRequested args realType = Requested args realType | Unrequested
  -- tot = Requested Nothing ("ok"::Text)
  
  -- data What a = Identity a
  -- -- data Requested a = Actual a | Unrequested
  -- data Test a b c = Test  { id :: a Text
  --                     , field_a :: b Text
  --                     , field_b :: c Integer
  -- } deriving (Generic)
  
  -- type CoreTest = Test Id Id Id
  
  -- ol = Test { id = "ok" :: Text } :: CoreTest
  
  -- type TestSchema = Test Query
  -- type TestMain = Test Identity
  -- type TestQuery = Test (Query Identity)
  
  -- type QueryDocPart = Test Query Identity
  
  -- testor :: Text -> Text -> QueryDocPart
  -- testor a b = Test { id      = Requested
  --                     , field_a = Requested
  --                     }
  
  -- okok = Test { id      = Requested (decode "{}")
  --             , field_a = Requested
  --             , field_b = Unrequested
  --           }
  
  -- testfunc :: Text -> Text -> TestSchema
  -- testfunc a b = Test { id      = Requested ("ok" :: Text)
  --                       , field_a = Requested ("yea" :: Text)
  --                       , field_b = Requested 1234
  --                     }
  
  -- okok = Test { id      = Requested _
  --                 , field_a = Requested _
  --                 , field_b = Requested _
  --                 }
  
  -- test = Test Requested ((Actual ("okok" :: Text)) (Actual ("kkkk" :: Text)))
  -- test = Test Requested { id      = Actual ("ok" :: Text)
  --                       , field_a = Actual ("yea" :: Text)
  --                       }
  
  
  