{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Network.Consul

main :: IO ()
main = do
  consul <- initializeConsulClient "localhost" 8500 Nothing
  let serviceRequest = RegisterService Nothing "MyService" ["master"] (Just 8080) (Just $ Ttl "120s")
  registerService consul serviceRequest Nothing
  service <- getServiceHealth consul "MyService"
  print service
