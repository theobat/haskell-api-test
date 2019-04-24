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
{-# LANGUAGE MultiParamTypeClasses #-}

module Modules.Organization.Schema  where

import Protolude hiding (Enum)

import qualified Data.Aeson as Aeson

import qualified Modules.Organization.SchemaSQL as SQL
import qualified Utils as SQLUtils
import qualified RootSQL as RootSQL
import qualified GQLSchema as GQLSchema

data HaskellType = HaskellType
data GQLSchemaType = GQLSchemaType
data NoArgument = NoArgument
-- data SQLMonster = SQLMonster Text | SQLMonster

type family GType a returnType argumentType (monadType:: * -> *)
type instance GType HaskellType returnType argumentType monadType = returnType
type instance GType GQLSchema.GQLResolver returnType argumentType monadType = argumentType -> monadType returnType

data OrganizationCore a = OrganizationCore {
  id :: GType a Text NoArgument Identity-- initialValue ? monad ? SQLType
  , name :: GType a Text NoArgument IO
} deriving (Generic, Typeable)

type Organization = OrganizationCore HaskellType
type OrganizationResolver = OrganizationCore GQLSchema.GQLResolver


organizationSchema :: GQLSchema.Object SQL.OrganizationT OrganizationCore
organizationSchema = GQLSchema.Object {
    GQLSchema.sqlTable = RootSQL.organization RootSQL.betonDirectDb
  , GQLSchema.__typename = "Organization"
  , GQLSchema.resolver = hahaha
  , GQLSchema.sqlResolver = undefined
}


okok :: Organization
okok = OrganizationCore {
  id = "okok"
  , name = "okqsdd"
}
hahaha :: OrganizationResolver
hahaha = OrganizationCore {
  id = (\_ -> "okqoskd")
  , name = (\_ -> pure "okqoskd")
}

-- data NoArg = NoArg
-- data GQLAnswer a = GQLAnswer a
-- data SchemaT a = SchemaT a
-- data GQLQuery a = GQLQuery a
-- data GQLField returnType argumentType = GQLFieldC returnType
-- data GQLFieldType = GQLScalar | GQLNonScalar
-- data Queried argument selection = Asked argument | AskedSelection argument selection | Unasked

-- type family GQLType a
-- type instance GQLType (Identity (GQLField returnType argumentType) ) = returnType
-- type instance GQLType (SchemaT (GQLField returnType argumentType) ) = Maybe (argumentType -> returnType)
-- type instance GQLType (GQLQuery (GQLField returnType argumentType) ) = Queried argumentType argumentType
-- type instance GQLType (GQLAnswer (GQLField returnType argumentType) ) = Queried returnType argumentType

-- data Test a = Test  { id :: GQLType (a (GQLField Text ()) )
-- , more :: GQLType (a (GQLField (Maybe (More a)) ()))
-- , field_a :: GQLType (a (GQLField Text ()))
-- } deriving (Generic, Typeable)

-- data More a = More  { didi :: GQLType (a (GQLField Text ()) )
--                     , momo :: GQLType (a (GQLField Text ()))
--                     , yea :: GQLType (a (GQLField Integer ()))
-- } deriving (Generic, Typeable)

-- type AnotherTest = Test Identity
-- type MoreCore = More Identity

-- deriving instance Eq MoreCore
-- deriving instance Eq AnotherTest
-- deriving instance Typeable AnotherTest
-- -- deriving instance G.Data AnotherTest

-- type TestSchema = Test SchemaT
-- type TestQuery = Test GQLQuery
-- type TestAnswer = Test GQLAnswer

-- type MoreSchema = More SchemaT
-- type MoreQuery = More GQLQuery
-- type MoreAnswer = More GQLAnswer

-- olol = Test { id = "okok", field_a = "okoko", more = Nothing } :: AnotherTest

-- oyeah :: AnotherTest
-- oyeah = Test "okok" Nothing "koko" 

-- ouuk = [SelectionField (Field Nothing (Name "TPE") [] [] [])]

-- testResolve
--   :: [Selection] -> TestSchema -> AnotherTest
-- testResolve selectionList schema =
--   Test { 
--     id = "ok", 
--     field_a = "okoko", 
--     more = Nothing 
--   }



-- testSchema =
--   Test { id = Just (\_ -> "ok" :: Text), field_a = Nothing, more = Nothing } :: TestSchema
-- omomomo =
--     Test { id = Asked "okok", field_a = Unasked, more = Asked Nothing } :: TestAnswer

-- testQuery =
--   Test { id = Asked (), field_a = Unasked, more = Asked () } :: TestQuery


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


