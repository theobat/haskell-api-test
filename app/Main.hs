module Main (main) where
  import Protolude
  import Beamtest

  main :: IO ()
  main = do
    conn <- connectPostgreSQL "host=localhost dbname=shoppingcart1"
  return ()