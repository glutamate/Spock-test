{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import Web.Spock
import           Database.Persist hiding (get)
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH
import Control.Concurrent
import qualified Test.HTTP as HT

sessCfg = SessionCfg "spocktest" (72*60*60) 42

main = do
  forkIO $ runServer
  threadDelay (1*1000*1000)
  runTest

runServer = 
  withSqlitePool ":memory:" 1 $ \pool -> do 
    spock 3000 sessCfg (PCPool pool) () routes

routes = do
  get "/" $ text "hello"

runTest = do
  HT.httpTest $ do
    HT.session "testHello" "http://localhost:3000" $ do
       hello <- HT.get "/"
       HT.assert "root is hello" $ hello=="hello"

