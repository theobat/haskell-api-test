{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
module WebServer
    ( main
    )

where
import           Protolude
import  qualified Web.Scotty as Scotty
import  qualified Data.Aeson as JSON

import qualified Modules.Organization.Schema as OrganizationSchema
import qualified NaiveHasqlTest as NaiveHasqlTest
import Data.Maybe
import qualified Control.Retry as Retry
import NaiveHasqlTest (getConnection, processQueryId)
import Data.UUID

import Data.Monoid (mconcat)  

main = do 
  connection <- getConnection
  Scotty.scotty 3000 $ Scotty.get "/:word" $ do
      beam <- Scotty.param "word"
      id <- case fromString beam of
        Just id -> pure id
        Nothing -> Scotty.raise "Error in uuid transformation"
      res <- pure $ (\idThunk -> idThunk id) <$> (processQueryId <$> connection)
      first <- liftIO $ case res of
        Right query -> do 
          test <- query
          pure $ case test of
            Right value ->  value
            Left error ->  "an error occured in SQL query"
        Left error -> pure "An error occured in DB connection"
      Scotty.json $ first


okok :: Text -> OrganizationSchema.Organization
okok a = OrganizationSchema.OrganizationCore { id = a }
