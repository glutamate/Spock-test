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

sessCfg = SessionCfg "pricing" (72*60*60) 42


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main = do
  forkIO $ runServer
  threadDelay (2*1000*1000)
  runTest

runServer = 
  withSqlitePool "/tmp/spocktestdb" 1 $ \pool -> do 
    spock 3000 sessCfg (PCPool pool) () routes

routes = do
  get "/" $ text "hello"

runTest = do
  HT.httpTest $ do
    HT.session "testHello" "http://localhost:3000" $ do
       hello <- HT.get "/"
       HT.assert "root is hello" $ hello=="hullo"