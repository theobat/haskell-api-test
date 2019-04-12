{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module RabbitMQTestServer where

import           Protolude               hiding ( Enum )

import           Control.Concurrent         (MVar, newEmptyMVar, putMVar,
                                             takeMVar)
-- import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text
import           Data.Maybe                 (fromJust)
import           Network.AMQP
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson                    as Aeson

main :: IO ()
main = do
    conn  <- openConnection "127.0.0.1" "/" "guest" "guest"
    ch    <- openChannel conn

    qos ch 0 1 False
    declareQueue ch newQueue {queueName = rpcQueue}

    m <- newEmptyMVar
    _ <- consumeMsgs ch rpcQueue Ack $ handleRequest ch m
    print " [x] Awaiting RPC requests"
    takeMVar m

    closeConnection conn
  where
    rpcQueue = "rpc_queue"

data Test = Test
  { numbers :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

handleRequest :: Channel -> MVar () -> (Message, Envelope) -> IO ()
handleRequest ch m (msg, envelope) = do
    let requestBody = msgBody msg
    let okok = (Aeson.decode requestBody) :: Maybe Test
    -- let unpackedBody = BL.unpack requestBody
    -- let test = decodeUtf8 requestBody
    -- let requestBody = msgBody msg
    -- n <- readIO . BL.unpack . msgBody $ msg
    print okok
    -- putStrLn $ " [.] fib(" ++ show n ++ ")"

    let result = fib . numbers <$> okok
    print result
    -- let response = newMsg { msgCorrelationID = msgCorrelationID msg
    --                       , msgBody = msgBody msg
    --                       }
    -- publishMsg ch "" replyTo response
    ackEnv envelope
    putMVar m ()
  where
    replyTo = fromJust $ msgReplyTo msg

fib :: Int -> Int
fib n
    | n >= 2    = fib (n - 1) + fib (n - 2)
    | n == 1    = 1
    | otherwise = 0
