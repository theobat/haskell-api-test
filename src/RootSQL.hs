{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE PartialTypeSignatures       #-}

module RootSQL
  ( testQuery
    , GQLSchemaObject(..)
    , BetonDirectDb(..)
    , betonDirectDb
    , GQLResolver(..)
  )
where

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

ok = runBeamPostgresDebug

boundedQuery :: Q PgSelectSyntax BetonDirectDb _ _
boundedQuery = limit_ 1 $ offset_ 1 $ all_ (organization betonDirectDb)

testQuery :: IO ()
testQuery = do 
    conn <- runBeamPostgresDebug putStrLn <$> customConnect
    ok <- conn $ runSelectReturningList $ select $ do 
      organizationList <- boundedQuery
      pure (OrganizationSQL.id organizationList)
    print ok
    -- orgs <- runSelectReturningList $ select boundedQuery
    -- mapM_ (liftIO . putStrLn . show) orgs
  
-- data Ala = Ala Text

-- whot :: IO Ala
-- whot = case (pure (Just "ok")) of 
--   IO (Just a) -> IO Ala a
data GQLResolver = GQLResolver
data SQLResolver = SQLResolver

-- | The object in GQL
-- |
data GQLSchemaObject sqlTableType coreType = GQLSchemaObject {
    sqlTable :: DatabaseEntity Postgres BetonDirectDb (TableEntity sqlTableType)
  , __typename :: Text
  , resolver :: coreType GQLResolver
  , sqlResolver :: coreType SQLResolver
}

