{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ScottyWrapper
  ( scottyServer,
    testServer
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.Morpheus (Interpreter (..))
import Data.Morpheus.Document (toGraphQLDocument)
import Data.Morpheus.Server
  ( GQLState,
    gqlSocketApp,
    initGQLState,
  )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import Network.WebSockets (defaultConnectionOptions)
-- examples
import TestGraphQL (rootResolver)
import Web.Scotty
  ( body,
    file,
    get,
    post,
    raw,
    scottyApp,
    scotty,
  )

testServer :: IO ()
testServer = scotty 3000 $ post "/api" $ raw =<< (liftIO . (interpreter rootResolver) =<< body)

scottyServer :: IO ()
scottyServer = do
  state <- initGQLState
  httpApp <- httpServer state
  print "listen to port 3000"
  Warp.runSettings settings $
    WaiWs.websocketsOr defaultConnectionOptions (wsApp state) httpApp
  where
    settings = Warp.setPort 3000 Warp.defaultSettings
    wsApp = gqlSocketApp rootResolver
    httpServer :: GQLState IO () -> IO Wai.Application
    httpServer state = scottyApp $ post "/" $ raw =<< (
      do
        liftIO $ print "recevied query"
        -- liftIO $ print (show body)
        (liftIO . handle state =<< body)
      )
    handle = interpreter rootResolver

