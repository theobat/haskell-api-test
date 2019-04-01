--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Websocket
    ( test
    )
where
import           Protolude


--------------------------------------------------------------------------------
import           Control.Concurrent             ( forkIO )
import           Control.Monad                  ( forever
                                                , unless
                                                )
import           Control.Monad.Trans            ( liftIO )
import           Network.Socket                 ( withSocketsDo )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Network.WebSockets            as WS


--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
    print ("Connected!"::Text)

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)


--------------------------------------------------------------------------------
test :: IO ()
test = withSocketsDo $ WS.runClient
    (T.unpack "events.voip.ovh.net")
    80
    (T.unpack (session "15724800"))
    app


session :: Text -> Text
-- session sessionId = "echo.websocket.org"
session sessionId = "/v2/session/" <> sessionId <> "/events/websocket"