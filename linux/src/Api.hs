{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Api where
import Network.WebSockets
import Data.Aeson
import Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import GHC.Generics
import Control.Lens ((^.))
import Control.Concurrent
import Control.Exception
import System.IO.Error
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson.Lens (key, _String)
import Data.Monoid
import System.IO
import System.Timeout

import Types
import Util


data MSG = MSG {
    msg :: T.Text,
    msgid :: T.Text
} deriving (Generic)

instance FromJSON MSG

--                           ip     port  username devicename
getInfo :: Configuration -> (String, Int, T.Text, T.Text)
getInfo conf = (conf ^. serverInfoL . serverIPL,
                conf ^. serverInfoL . serverPortL,
                T.pack $ conf ^. userInfoL . usernameL,
                T.pack $ conf ^. userInfoL . usernameL)

appRegister :: T.Text -> T.Text -> ClientApp LBS.ByteString
appRegister u p conn = do
    sendTextData conn (encode v)
    receiveData conn where
        v = object ["username" .= String u, "password" .= String p]

exeRegister :: Configuration -> T.Text -> IO LBS.ByteString
exeRegister conf p = do
    let (ip, port, u, _) = getInfo conf
    runClient ip port "/register" $ appRegister u p

appLogin :: T.Text -> T.Text -> T.Text -> ClientApp LBS.ByteString
appLogin u p d conn = do
    sendTextData conn (encode v)
    receiveData conn where
        v = object ["username" .= String u, "password" .= String p,
                    "deviceName" .= String d]

exeLogin :: Configuration -> T.Text -> IO LBS.ByteString
exeLogin conf p = do
    let (ip, port, u, d) = getInfo conf
    runClient ip port "/login" $ appLogin u p d

appLogout :: Token -> ClientApp LBS.ByteString
appLogout tok conn = do
    sendTextData conn (encode v)
    receiveData conn where
        v = object ["token" .= String tok]

exeLogout :: Configuration -> Token -> IO LBS.ByteString
exeLogout conf tok = do
    let (ip, port, _, _) = getInfo conf
    runClient ip port "/logout" $ appLogout tok

appPost :: Token -> T.Text -> ClientApp LBS.ByteString
appPost tok msg conn = do
    sendTextData conn (encode v)
    receiveData conn where
        v = object ["token" .= tok, "data" .= msg]

exePost :: Configuration -> Token -> T.Text -> IO LBS.ByteString
exePost conf tok msg = do
    let (ip, port, _, _) = getInfo conf
    runClient ip port "/post" $ appPost tok msg

appSync :: Configuration -> ClientApp ()
appSync conf conn = do
    tok <- getTokenFromConfig conf
    {-TIO.putStrLn tok-}
    let log = getLogFileFromConfig conf
        v = object ["token" .= tok]
    sendTextData conn (encode v)
    resp <- receiveData conn
    processResp resp "Sync" $ do
        tid <- forkIO $ syncLoop conn log
        forkPingThread conn 240
        pingLoop conn 240 tid conf

appPing :: ClientApp Bool
appPing conn = do
    sendTextData conn ("ping" :: T.Text)
    result <- timeout 4000000 $ receiveData conn
    case result of
        Nothing -> return False
        Just t -> if t == ("ping" :: T.Text) then return True else
                    error "this is really a big surprise!!! BUG AGAIN!"

exePing :: Configuration -> IO Bool
exePing conf = do
    let (ip, port, _, _) = getInfo conf
    runClient  ip port "/ping" $ appPing

pingLoop :: Connection -> Int -> ThreadId -> Configuration -> IO ()
pingLoop conn n tid conf = do
    b <- exePing conf
    if b then do
            threadDelay (n * 1000000) -- default 5 min
            pingLoop conn n tid conf
        else do
            killThread tid
            Prelude.putStrLn "ping failed..., will retry connection in 1 minutes"
            threadDelay 6000000
            exeSync conf

syncLoop :: Connection -> FilePath -> IO ()
syncLoop conn log = do
    msg <- receiveData conn
    let msg' = encodeUtf8 $ msg ^. key "msg" . _String
        msg'' = msg' <> "\n" <> BS.replicate 80 '-' <> "\n"
        Just m = decode msg
        v = object ["msgid" .= String (msgid m), "status" .= String "ok"]
    BS.appendFile log msg''
    forkIO $ setClipboard . decodeUtf8 $ msg'
    catch (sendTextData conn (encode v)) handler
    syncLoop conn log where
        handler :: SomeException -> IO ()
        handler e = print e >> error "this should be very very rare, \
        \ and I have no clue what happened. You are on your own, sir"

exeSync :: Configuration -> IO ()
exeSync conf = do
    let (ip, port, _, _) = getInfo conf
        log = getLogFileFromConfig conf
    catch (runClient ip port "/sync" $ appSync conf) handler where
        handler :: SomeException -> IO ()
        handler e = do
                TIO.hPutStrLn stderr "fail to connect... will retry in 1 min"
                threadDelay 6000000
                exeSync conf
