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

import qualified Modules.Organization.Types.OrganizationCore as OrganizationCore

import Data.Monoid (mconcat)  

main = Scotty.scotty 3000 $
      Scotty.get "/:word" $ do
        beam <- Scotty.param "word"
        Scotty.json $ [beam]
        Scotty.html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


okok :: Text -> OrganizationCore.T
okok a = OrganizationCore.T { id = a }