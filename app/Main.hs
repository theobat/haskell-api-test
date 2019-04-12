module Main (main) where
  -- import Protolude
  -- import Beamtest
  -- import Websocket
  -- import qualified RabbitMQTestServer as AMQP
  import qualified Graphqltest as GQL

  -- main :: IO ()
  -- main = do
  --   conn <- connectPostgreSQL "host=localhost dbname=shoppingcart1"
  -- return ()
  main :: IO ()
  main = GQL.main

  