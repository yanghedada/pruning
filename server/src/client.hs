{-# LANGUAGE OverloadedStrings #-}
module Client where
import Network.WebSockets
import Data.Aeson
import Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T

type Token = T.Text

serverIP :: String
serverIP = "127.0.0.1"

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
