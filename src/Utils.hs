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

module Utils where

import Protolude hiding (Enum)

import qualified Data.Aeson as Aeson

import           Data.Data
import           GHC.Generics

main :: IO ()
main = print $ selectors (Proxy :: Proxy (Rep Test))

data Test = Test  { id :: Text } deriving (Generic, Typeable)

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


