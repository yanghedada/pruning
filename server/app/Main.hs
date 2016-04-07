{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.WebSockets
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Concurrent
import Control.Monad
import Data.HashMap.Lazy as HM

import Api
import Database
import Constant
import Types

app :: MVar MessagePool -> ServerApp
app mp pend = do
    case (requestPath $ pendingRequest pend) of
        "/register" -> do
            conn <- acceptRequest pend
            appRegister conn
        "/login" -> do
            conn <- acceptRequest pend
            appLogin mp conn
        "/post" -> do
            conn <- acceptRequest pend
            appPost mp conn
        "/logout" -> do
            conn <- acceptRequest pend
            appLogout mp conn
        "/sync" -> do
            conn <- acceptRequest pend
            appSync mp conn
        "/ping" -> do
            conn <- acceptRequest pend
            appPing conn
        _ -> rejectRequest pend ""

main :: IO ()
main = do
    runSqlite sqlTable (runMigration migrateAll)
    msgPool <- newMVar (empty :: MessagePool)
    results <- runSqlite sqlTable $ selectList ([] :: [Filter TokenMap]) []
    forM_ results $ \(Entity _ val) -> do
        let t = tokenMapToken val
        modifyMVar_ msgPool (return . HM.insert t [])
    runServer "0.0.0.0" 4564 $ app msgPool
