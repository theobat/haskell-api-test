module Main (main) where
  -- import Protolude
  -- import Beamtest
  -- import Websocket
  -- import qualified RabbitMQTestServer as AMQP
  import qualified WebServer as WebServer

  -- main :: IO ()
  -- main = do
  --   conn <- connectPostgreSQL "host=localhost dbname=shoppingcart1"
  -- return ()
  main :: IO ()
  main = WebServer.main

  