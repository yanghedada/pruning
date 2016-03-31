{-# LANGUAGE OverloadedStrings #-}
module Api where

import Network.WebSockets
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS


import Crypto
import JSON

appRegister :: Connection -> IO ()
appRegister conn = do
    jregister <- receiveData conn >>= return . decode
    case jregister of
        Just jr -> performRegisterAction conn jr
        Nothing -> sendTextData conn (encode obj) where
            obj = object [ "code" .= Number 400, "msg" .= String "bad request" ]

performRegisterAction :: Connection -> JRegister -> IO ()
performRegisterAction conn jr = do
    putStrLn $ show jr

appLogin :: Connection -> IO ()
appLogin conn = undefined

appPost :: Connection -> IO ()
appPost conn = undefined

appLogout :: Connection -> IO ()
appLogout conn = undefined

appSync :: Connection -> IO ()
appSync = undefined
