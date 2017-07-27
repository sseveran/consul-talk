{-# LANGUAGE OverloadedStrings #-}

import Network.Consul

main :: IO ()
main = do
  consul <- initializeConsulClient "localhost" 8500 Nothing
  let put = KeyValuePut "/key1" "Hello Haskell" Nothing Nothing
  putKey consul put Nothing
  result <- getKey consul "/key1" Nothing Nothing Nothing
  print result
