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

module NaiveHasqlTest where

import Protolude
import Data.Functor.Contravariant
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Session as Session
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Connection as Connection
import Data.UUID
import qualified Data.Aeson as Aeson
import qualified Control.Retry as Retry

main :: IO ()
main = do
  Right connection <- Connection.acquire connectionSettings
  result <- pure "ok" -- Session.run (testQuery <$> (fromString "0804c857-bbc9-45f2-a213-8b1e776c7f8c")) connection
  print result
  where
    connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "beton_direct_web"

getConnection :: IO (Either _ Connection.Connection)
getConnection = Retry.retrying (Retry.exponentialBackoff 50 <> Retry.limitRetries 20) (const $ return . isLeft) simpleGetConnection

simpleGetConnection :: _ -> IO (Either _ Connection.Connection)
simpleGetConnection _ = do
  _ <- print "Connecting to DB"
  connectionAttempt <- Connection.acquire connectionSettings
  _ <- case connectionAttempt of
    Left error -> print error
    Right _ -> mempty
  pure connectionAttempt
  where
    connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "beton_direct_web"

processQueryId :: Connection.Connection -> UUID -> IO (Either Session.QueryError Aeson.Value)
processQueryId connection id = do
  Session.run (testQuery id) connection

-- makeConnection :: (Connection.Connection -> IO()) -> IO ()
-- makeConnection processConnection = do 
--   eitherConnection <- Connection.acquire connectionSettings
--   connection <- case eitherConnection of
--     Right connection -> pure connection
--     Left error -> do
--       _ <- putStrLn "Big error in connection to Database"
--       eitherConnection
--   print (processConnection connection)
--   where
--     connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "beton_direct_web"

    -- * Sessions
-- 
-- Session is an abstraction over the database connection and all possible errors.
-- It is used to execute statements.
-- It is composable and has a Monad instance.
-- 
-- It's recommended to define sessions in a dedicated 'Sessions'
-- submodule of your project.
-------------------------

testQuery :: UUID -> Session Aeson.Value
testQuery (a) = do
  res1 <- Session.statement [a] findOrg
  pure $ res1
  -- Get the sum of a and b
  -- sumOfAAndB <- Session.statement (a, b) sumStatement
  -- Divide the sum by c and get the modulo as well
  -- Session.statement (sumOfAAndB, c) divModStatement


-- * Statements
-- 
-- Statement is a definition of an individual SQL-statement,
-- accompanied by a specification of how to encode its parameters and
-- decode its result.
-- 
-- It's recommended to define statements in a dedicated 'Statements'
-- submodule of your project.
-------------------------


data CorporationTest = CorporationTest {
  id :: UUID
  , name :: Text
}

findOrg :: Statement [UUID] Aeson.Value
findOrg = Statement sql encoder decoder True where
  sql = "select to_jsonb(name) from organization where id=ANY($1)"
  encoder =
    Encoders.param (Encoders.array (Encoders.dimension foldl' (Encoders.element Encoders.uuid)) )
  decoder = Decoders.singleRow (Decoders.column Decoders.jsonb)

-- testInsertA :: Statement [CorporationTest] _
-- testInsertA = Statement sql encoder ignore True where
--   sql = "insert into "
--   encoder =
--     Encoders.param (Encoders.array (Encoders.dimension foldl' (Encoders.element Encoders.uuid)) )
--   decoder = Decoders.rowList (Decoders.column Decoders.jsonb)
-- testInsertB :: Statement [CorporationTest]

sumStatement :: Statement (Int64, Int64) Int64
sumStatement = Statement sql encoder decoder True where
  sql = "select count(*) + $1 from organization"
  encoder =
    (fst >$< Encoders.param Encoders.int8) <>
    (snd >$< Encoders.param Encoders.int8)
  decoder = Decoders.singleRow (Decoders.column Decoders.int8)

divModStatement :: Statement (Int64, Int64) (Int64, Int64)
divModStatement = Statement sql encoder decoder True where
  sql = "select $1 / $2, $1 % $2"
  encoder =
    (fst >$< Encoders.param Encoders.int8) <>
    (snd >$< Encoders.param Encoders.int8)
  decoder = Decoders.singleRow row where
    row =
      (,) <$>
      Decoders.column Decoders.int8 <*>
      Decoders.column Decoders.int8