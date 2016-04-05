{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Client where
import Network.WebSockets
import Data.Aeson
import Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import GHC.Generics

type Token = T.Text

data MSG = MSG {
    msg :: T.Text,
    msgid :: T.Text
} deriving (Generic)

instance FromJSON MSG

serverIP :: String
serverIP = "162.211.226.148"
--serverIP = "127.0.0.1"

serverPort :: Int
serverPort = 4564

appRegister :: T.Text -> T.Text -> ClientApp ()
appRegister u p conn = do
    sendTextData conn (encode v)
    msg <- receiveData conn
    TIO.putStrLn msg where
        v = object ["jrusername" .= String u, "jrpassword" .= String p]

exeRegister :: T.Text -> T.Text -> IO ()
exeRegister u p = runClient serverIP serverPort "/register" $ appRegister u p

appLogin :: T.Text -> T.Text -> ClientApp ()
appLogin u p conn = do
    sendTextData conn (encode v)
    msg <- receiveData conn
    TIO.putStrLn msg where
        v = object ["jlusername" .= String u, "jlpassword" .= String p, 
                    "jldeviceName" .= String "Arch Linux"]

exeLogin :: T.Text -> T.Text -> IO ()
exeLogin u p = runClient serverIP serverPort "/login" $ appLogin u p

appLogout :: Token -> ClientApp ()
appLogout tok conn = do
    sendTextData conn (encode v)
    msg <- receiveData conn
    TIO.putStrLn msg where
        v = object ["jltoken" .= String tok]

exeLogout :: Token -> IO ()
exeLogout tok = runClient serverIP serverPort "/logout" $ appLogout tok

appPost :: Token -> T.Text -> ClientApp ()
appPost tok msg conn = do
    sendTextData conn (encode v)
    msg <- receiveData conn
    TIO.putStrLn msg where
        v = object ["jptoken" .= tok, "jpdata" .= msg]

exePost :: Token -> T.Text -> IO ()
exePost tok msg = runClient serverIP serverPort "/post" $ appPost tok msg

appSync :: Token -> ClientApp ()
appSync tok conn = do
    sendTextData conn (encode v)
    forkPingThread conn 300
    syncLoop conn where
        v = object ["jstoken" .= tok]

syncLoop :: Connection -> IO ()
syncLoop conn = do
    msg <- receiveData conn
    LBS.putStrLn msg
    let Just m = decode msg
        v = object ["jsamsgid" .= String (msgid m), "jsastatus" .= String "ok"]
    sendTextData conn (encode v)
    syncLoop conn

exeSync :: Token -> IO ()
exeSync token = runClient serverIP serverPort "/sync" $ appSync token
