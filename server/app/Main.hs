{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.WebSockets

import Api

app :: ServerApp
app pend = do
    case (requestPath $ pendingRequest pend) of
        "/register" -> do
            conn <- acceptRequest pend
            appRegister conn
        "/login" -> do
            conn <- acceptRequest pend
            appLogin conn
        "/post" -> do
            conn <- acceptRequest pend
            appPost conn
        "/logout" -> do
            conn <- acceptRequest pend
            appLogout conn
        "/sync" -> do
            conn <- acceptRequest pend
            appSync conn
        _ -> rejectRequest pend ""

main :: IO ()
main = do
    runServer "0.0.0.0" 4564 app
