{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE PartialTypeSignatures       #-}

module Modules.Organization.SchemaSQL where

import           Data.Text                  (Text)
import           Database.Beam              as B
import           Database.Beam.Postgres
import           Protolude

data OrganizationT f = Organization
  { id    :: Columnar f Text
  , name :: Columnar f Text
  } deriving (Generic)

type Organization = OrganizationT Identity
type OrganizationKey = PrimaryKey OrganizationT Identity
deriving instance Show Organization
deriving instance Eq Organization
deriving instance Show OrganizationKey
deriving instance Eq OrganizationKey

instance Beamable OrganizationT

instance Table OrganizationT where
  data PrimaryKey OrganizationT f = OrganizationId (Columnar f Text) deriving (Generic)
  primaryKey = OrganizationId . id

instance Beamable (PrimaryKey OrganizationT)