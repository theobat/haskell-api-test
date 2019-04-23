{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE PartialTypeSignatures       #-}

module Modules.Plant.SchemaSQL where

import           Data.Text                      ( Text )
import           Database.Beam                 as B
import           Database.Beam.Postgres
import           Protolude
import           Data.UUID                      ( UUID )

data PlantT f = Plant
  { id    :: Columnar f UUID
  , name :: Columnar f Text
  } deriving (Generic)

type Plant = PlantT Identity
type PlantKey = PrimaryKey PlantT Identity
deriving instance Show Plant
deriving instance Eq Plant
deriving instance Show PlantKey
deriving instance Eq PlantKey

instance Beamable PlantT

instance Table PlantT where
  data PrimaryKey PlantT f = PlantId (Columnar f UUID) deriving (Generic)
  primaryKey = PlantId . id

instance Beamable (PrimaryKey PlantT)
