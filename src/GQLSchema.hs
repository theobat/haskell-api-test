{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE PartialTypeSignatures       #-}

module GQLSchema
  (   Object(..)
    , BetonDirectDb(..)
    , GQLResolver(..)
  )
where

import           Data.Text                  (Text)
import           Database.Beam              as B
import           Database.Beam.Postgres
import           Protolude
import RootSQL (BetonDirectDb)

data GQLResolver = GQLResolver
data SQLResolver = SQLResolver

-- | The object in GQL
-- |
data Object sqlTableType coreType = Object {
    sqlTable :: DatabaseEntity Postgres BetonDirectDb (TableEntity sqlTableType)
  , __typename :: Text
  , resolver :: coreType GQLResolver
  , sqlResolver :: coreType SQLResolver
}

data List underlyingType = List underlyingType
