{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE PartialTypeSignatures       #-}

module RootSQL where

import           Data.Text                  (Text)
import           Database.Beam              as B
import           Database.Beam.Postgres
import           Protolude
import qualified Modules.Organization.SchemaSQL as OrganizationSQL

data BetonDirectDb f = BetonDirectDb {
  organization :: f (TableEntity OrganizationSQL.OrganizationT)
} deriving (Generic, Database Postgres)

betonDirectDb :: DatabaseSettings Postgres BetonDirectDb
betonDirectDb = defaultDbSettings

customConnect :: IO Connection
customConnect = connect defaultConnectInfo { connectDatabase = "beton_direct_web" }

boundedQuery = limit_ 1 $ offset_ 1 $ all_ (organization betonDirectDb)

testQuery = runBeamPostgresDebug putStrLn <$> customConnect
  orgs <- runSelectReturningList $ select boundedQuery
  mapM_ (liftIO . putStrLn . show) orgs