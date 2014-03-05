{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import Web.Spock
import Web.Spock.Worker
import           Database.Persist hiding (get)
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH
import Control.Concurrent
import qualified Test.HTTP as HT
import qualified Data.Text.Lazy as TL
import Control.Concurrent.STM
import Control.Monad.Trans

sessCfg = SessionCfg "spocktest" (72*60*60) 42

main = do
  forkIO $ runServer
  threadDelay (1*1000*1000)
  runTest

runServer = do
  tv <- atomically $ newTVar (0::Int)
  withSqlitePool ":memory:" 1 $ \pool -> do 
    spock 3000 sessCfg (PCPool pool) tv routes

routes = do
  worker <- newWorker 10 doWork workErrH
  get "/" $ text "hello"
  get "/get_counter" $ do 
    counter <- liftIO . atomically . readTVar =<< getState
    text $ TL.pack $ show counter
  get "/increment/:delay/:val" $ do
    delay <- param "delay"
    val <- param "val"
    addWork (WorkIn $ fromInteger delay) val worker
    text "OK"

doWork incr_with = do
  tv <- getState
  liftIO $ atomically $ modifyTVar tv (+incr_with)
  return WorkComplete

workErrH _ _ = do return WorkError

runTest = do
  HT.httpTest $ do
    HT.session "testHello" "http://localhost:3000" $ do
       hello <- HT.get "/"
       HT.assert "root is hello" $ hello=="hello"
       assertCounter "initial" "0"
       HT.get "/increment/1/1" >>= HT.assertEq "increasingOK" "OK"
       assertCounter "after incr" "0"
       liftIO $ threadDelay (2*1000*1000)
       assertCounter "after delay" "1"

assertCounter msg val = do
   c0 <- HT.get "/get_counter"
   HT.assertEq (msg ++" counter") val c0

