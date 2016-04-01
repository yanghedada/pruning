{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.WebSockets
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Concurrent
import Data.HashMap.Lazy

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
            appSync conn
        _ -> rejectRequest pend ""

main :: IO ()
main = do
    runSqlite sqlTable (runMigration migrateAll)
    msgPool <- newMVar (empty :: MessagePool)
    runServer "0.0.0.0" 4564 $ app msgPool
