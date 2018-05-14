{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE PartialTypeSignatures       #-}

module Beamtest where

import           Data.Text                  (Text)
import           Database.Beam              as B
import           Database.Beam.Postgres
import           Protolude

data UserT f = User
  { _userEmail     :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName  :: Columnar f Text
  , _userPassword  :: Columnar f Text
  } deriving (Generic)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User

instance Beamable UserT
instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
    primaryKey = UserId . _userEmail
instance Beamable (PrimaryKey UserT)


data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers :: f (TableEntity UserT) }
                        deriving Generic

instance Database be ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings

-- allUsers :: Q PgSelectSyntax _ _ _
-- allUsers = all_ (_shoppingCartUsers shoppingCartDb)

-- insertUsers :: Connection -> IO ()
-- insertUsers conn =
--   withDatabaseDebug putStrLn conn $ B.runInsert $
--     B.insert (_shoppingCartUsers shoppingCartDb) $
--     insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
--                  , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
--                  , User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
--                  , User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
--                  , User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
--                  , User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
--                  , User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
--                  , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
--                  ]

boundedQuery :: Q PgSelectSyntax _ _ _
boundedQuery = limit_ 1 $ offset_ 1 $
              orderBy_ (asc_ . _userFirstName) $
              all_ (_shoppingCartUsers shoppingCartDb)

-- selectAllUsers :: Connection -> IO ()
-- selectAllUsers conn =
--   withDatabaseDebug putStrLn conn $ do
--     let test = all_ (_shoppingCartUsers shoppingCartDb)
--     users <- runSelectReturningList $ select test
--     mapM_ (liftIO . putStrLn . show) users

-- connect :: IO ()
-- connect = do
--   conn <- connectPostgreSQL "host=localhost dbname=shoppingcart1"