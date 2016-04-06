{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Api where
import Network.WebSockets
import Data.Aeson
import Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import GHC.Generics
import Control.Lens ((^.))
import Control.Concurrent
import Control.Exception
import System.IO.Error
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson.Lens (key, _String)
import Data.Monoid
import System.IO

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
        v = object ["jrusername" .= String u, "jrpassword" .= String p]

exeRegister :: Configuration -> T.Text -> IO LBS.ByteString
exeRegister conf p = do
    let (ip, port, u, _) = getInfo conf
    runClient ip port "/register" $ appRegister u p

appLogin :: T.Text -> T.Text -> T.Text -> ClientApp LBS.ByteString
appLogin u p d conn = do
    sendTextData conn (encode v)
    receiveData conn where
        v = object ["jlusername" .= String u, "jlpassword" .= String p,
                    "jldeviceName" .= String d]

exeLogin :: Configuration -> T.Text -> IO LBS.ByteString
exeLogin conf p = do
    let (ip, port, u, d) = getInfo conf
    runClient ip port "/login" $ appLogin u p d

appLogout :: Token -> ClientApp LBS.ByteString
appLogout tok conn = do
    sendTextData conn (encode v)
    receiveData conn where
        v = object ["jltoken" .= String tok]

exeLogout :: Configuration -> Token -> IO LBS.ByteString
exeLogout conf tok = do
    let (ip, port, _, _) = getInfo conf
    runClient ip port "/logout" $ appLogout tok

appPost :: Token -> T.Text -> ClientApp LBS.ByteString
appPost tok msg conn = do
    sendTextData conn (encode v)
    receiveData conn where
        v = object ["jptoken" .= tok, "jpdata" .= msg]

exePost :: Configuration -> Token -> T.Text -> IO LBS.ByteString
exePost conf tok msg = do
    let (ip, port, _, _) = getInfo conf
    runClient ip port "/post" $ appPost tok msg

appSync :: Configuration -> ClientApp ()
appSync conf conn = do
    tok <- getTokenFromConfig conf
    TIO.putStrLn tok
    let log = getLogFileFromConfig conf
        v = object ["jstoken" .= tok]
    sendTextData conn (encode v)
    tid <- forkIO $ syncLoop conn log
    pingLoop conn 10 tid conf

pingLoop :: Connection -> Int -> ThreadId -> Configuration -> IO ()
pingLoop conn n tid conf = do
    result <- try $ sendPing conn ("ping"::LBS.ByteString)
    case result of
        Left (SomeException e) -> do
            killThread tid
            Prelude.putStrLn "connection issue..., will retry in 1 minutes"
            threadDelay 6000000
            exeSync conf
        Right _ -> do
            threadDelay (n * 1000000) -- default 5 min
            pingLoop conn n tid conf

syncLoop :: Connection -> FilePath -> IO ()
syncLoop conn log = do
    msg <- receiveData conn
    let msg' = LBS.fromStrict . encodeUtf8 $ msg ^. key "msg" . _String <> "\n"
        Just m = decode msg
        v = object ["jsamsgid" .= String (msgid m), "jsastatus" .= String "ok"]
    LBS.appendFile log msg'
    forkIO $ setClipboard $ LBS.unpack msg'
    catch (sendTextData conn (encode v)) handler
    syncLoop conn log where
        handler :: SomeException -> IO ()
        handler _ = error "this should be very very rare, what happened???"

exeSync :: Configuration -> IO ()
exeSync conf = do
    let (ip, port, _, _) = getInfo conf
        log = getLogFileFromConfig conf
    catch (runClient ip port "/sync" $ appSync conf) handler where
        handler :: ConnectionException -> IO ()
        handler _ = do
                TIO.hPutStrLn stderr "establishing connection failed... retry in 60 s"
                threadDelay 6000000
                exeSync conf
