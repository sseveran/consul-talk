{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Network.Consul

main :: IO ()
main = do
  consul <- initializeConsulClient "localhost" 8500 Nothing
  let put = KeyValuePut "/key2" "Hello Haskell" Nothing Nothing
  putKey consul put Nothing

  let sr = SessionRequest Nothing (Just "MySession") Nothing ["serfHealth"] (Just Release) (Just "30s")
  Just session <- createSession consul sr Nothing
  print session
  let put2 = KeyValuePut "/key2" "" Nothing Nothing
  _ <- putKeyAcquireLock consul put2 session Nothing
  result3 <- getKey consul "/key2" Nothing Nothing Nothing
  print result3

  threadDelay 10000000
  renewSession consul session Nothing
  result4 <- getSessionInfo consul session Nothing
  print result4
  destroySession consul session Nothing
  result5 <- getKey consul "/key2" Nothing Nothing Nothing
  print result5
