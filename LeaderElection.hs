{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad (forever)
import Control.Monad.Trans
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Network.Consul
import Network.HTTP.Client
import System.FilePath
import Web.Scotty

main :: IO ()
main = do
  server <- async runServer
  client <- async runClient

  wait server
  cancel client

runServer :: IO ()
runServer = do
  consul <- initializeConsulClient "localhost" 8500 Nothing
  let put1 = KeyValuePut "/someservice/shards/0" "" Nothing Nothing
  putKey consul put1 Nothing

  let sessionRequest = SessionRequest Nothing (Just "MySession") Nothing ["serfHealth"] (Just Release) (Just "10s")
  Just session <- createSession consul sessionRequest Nothing
  withSession consul Nothing 15 session (runSessionAction consul) lostAction

  where
    lostAction = return ()

    runSessionAction consul session = do
      mShard <- findShard consul session
      case mShard of
        Nothing -> return ()
        Just x -> do
          let partitionId = T.pack $ takeFileName $ T.unpack x
          Just self <- getSelf consul
          let req = RegisterService (Just $ T.concat["someservice-",mName $ sMember self]) "someservice" [partitionId] (Just 8080) (Just $ Ttl "10s")
          runService consul req runServiceAction Nothing

    runServiceAction = do
      scotty 8080 $ do
        get "/:word" $ do
          beam <- param "word"
          html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
      return ()

runClient :: IO ()
runClient = do
  consul <- initializeConsulClient "localhost" 8500 Nothing
  forever $ do
    threadDelay 10000000
    Just service <- getServiceHealth consul "someservice"
    mapM_ callService $ L.filter predicate service

  where
    predicate :: Health -> Bool
    predicate h = (L.elem "0" $ seTags $ hService h) && (L.all (\ x -> Passing == cStatus x) $ hChecks h)

    callService :: Health -> IO ()
    callService health = do
      let address = nAddress $ hNode health
          Just port = sePort $ hService health
      req <- parseRequest $ "http://" ++ T.unpack address ++ ":" ++ show port ++ "/beam"
      man <- newManager defaultManagerSettings
      response <- httpLbs req man
      print $ responseBody response

findShard :: ConsulClient -> Session -> IO (Maybe Text)
findShard consul session = getKeys consul "/someservice/shards/" Nothing Nothing Nothing >>= go
  where
    -- Make this into a guard
    attemptLock :: KeyValue -> IO Bool
    attemptLock kv = do
      case kvSession kv of
        Just _ -> do
          return False
        Nothing -> do
          case kvValue kv of
            Just x -> do
              let request = KeyValuePut (kvKey kv) x Nothing Nothing
              putKeyAcquireLock consul request session Nothing
            Nothing -> do
              let request = KeyValuePut (kvKey kv) "" Nothing Nothing
              putKeyAcquireLock consul request session Nothing

    go :: [KeyValue] -> IO (Maybe Text)
    go [] = return Nothing
    go (x:xs) = do
      result <- attemptLock x
      case result of
        True -> return $ Just (kvKey x)
        False -> go xs
