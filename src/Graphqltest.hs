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

module Graphqltest where

import Protolude hiding (Enum)

import qualified Data.Aeson as Aeson

import           Data.Data
import           Data.Int
import           Data.Proxy
import           GHC.Generics
import qualified Data.ByteString               as B
import           Data.Generics.Twins            ( gzipWithT
                                                )
import qualified Data.Generics as G

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

main :: IO ()
main = do
    print $ selectors (Proxy :: Proxy (Rep AnotherTest))
    


data QuoteRecord = QuoteRecord  { symbol              :: Text
                                , open                :: Text
                                , high                :: Text
                                , low                 :: Text
                                , price               :: Text
                                , volume              :: Text
                                , latestTradingDay    :: Text
                                , previousClose       :: Text
                                , change              :: Text
                                , changePercent       :: Text
                                } deriving (Eq, Show, Generic)

data NoArg = NoArg
data GQLAnswer a = GQLAnswer a
data SchemaT a = SchemaT a
data SchemaQuery a = SchemaQuery a
data GQLField a b = GQLFieldC a
data Queried a = Asked a | Unasked

type family GQLType a
type instance GQLType (Identity (GQLField a b) ) = a
type instance GQLType (SchemaT (GQLField a b) ) = Maybe (b -> a)
type instance GQLType (SchemaQuery (GQLField a b) ) = Queried (Maybe (b -> a))
type instance GQLType (GQLAnswer (GQLField a b) ) = Queried a

data Test a = Test  { id :: GQLType (a (GQLField Text NoArg) )
                    , field_a :: GQLType (a (GQLField Text NoArg))
                    , field_b :: GQLType (a (GQLField Integer NoArg))
} deriving (Generic, Typeable)

type AnotherTest = Test Identity

deriving instance Eq AnotherTest
deriving instance Typeable AnotherTest
deriving instance G.Data AnotherTest

type TestSchema = Test SchemaT
type TestQuery = Test SchemaQuery
type TestAnswer = Test GQLAnswer

olol = Test { id = "okok", field_a = "okoko", field_b = 12 } :: AnotherTest
testSchema = Test { id = Just (\a -> "ok"::Text), field_a = Nothing, field_b = Nothing } :: TestSchema
more =
    Test { id = Asked "okok", field_a = Unasked, field_b = Asked 12 } :: TestAnswer

mergeInt :: (G.Data AnotherTest, G.Data b) => AnotherTest -> b -> b
mergeInt = G.mkQ (G.mkT (identity :: Integer -> Integer))
                 (\a -> G.mkT (\b -> a + b :: Integer))

resolveTest :: TestQuery -> TestAnswer
resolveTest schema = more

class Selectors rep where
  selectors :: Proxy rep -> [([Char], TypeRep)]

instance Selectors f => Selectors (M1 D x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
  selectors _ =
    [ ( selName (undefined :: M1 S s (K1 R t) ()) , typeOf (undefined :: t) ) ]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors _ = selectors (Proxy :: Proxy a) ++ selectors (Proxy :: Proxy b)

instance Selectors U1 where
  selectors _ = []

-- data Query a = Requested a | Unrequested

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


