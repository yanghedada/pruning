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

import Types

type Token = T.Text

data MSG = MSG {
    msg :: T.Text,
    msgid :: T.Text
} deriving (Generic)

instance FromJSON MSG

appRegister :: T.Text -> T.Text -> ClientApp LBS.ByteString
appRegister u p conn = do
    sendTextData conn (encode v)
    receiveData conn where
        v = object ["jrusername" .= String u, "jrpassword" .= String p]

exeRegister :: Configuration -> T.Text -> IO LBS.ByteString
exeRegister conf p = do
    let ip = conf ^. serverInfoL . serverIPL
        port = conf ^. serverInfoL . serverPortL
        u = T.pack $ conf ^. userInfoL . usernameL
    runClient ip port "/register" $ appRegister u p

appLogin :: T.Text -> T.Text -> T.Text -> ClientApp LBS.ByteString
appLogin u p d conn = do
    sendTextData conn (encode v)
    receiveData conn where
        v = object ["jlusername" .= String u, "jlpassword" .= String p,
                    "jldeviceName" .= String d]

exeLogin :: Configuration -> T.Text -> IO LBS.ByteString
exeLogin conf p = do
    let ip = conf ^. serverInfoL . serverIPL
        port = conf ^. serverInfoL . serverPortL
        u = T.pack $ conf ^. userInfoL . usernameL
        d = T.pack $ conf ^. userInfoL . devicenameL
    runClient ip port "/login" $ appLogin u p d

appLogout :: Token -> ClientApp LBS.ByteString
appLogout tok conn = do
    sendTextData conn (encode v)
    receiveData conn where
        v = object ["jltoken" .= String tok]

exeLogout :: Configuration -> Token -> IO LBS.ByteString
exeLogout conf tok = do
    let ip = conf ^. serverInfoL . serverIPL
        port = conf ^. serverInfoL . serverPortL
    runClient ip port "/logout" $ appLogout tok

{-appPost :: Token -> T.Text -> ClientApp ()-}
{-appPost tok msg conn = do-}
    {-sendTextData conn (encode v)-}
    {-msg <- receiveData conn-}
    {-TIO.putStrLn msg where-}
        {-v = object ["jptoken" .= tok, "jpdata" .= msg]-}

{-exePost :: Token -> T.Text -> IO ()-}
{-exePost tok msg = runClient serverIP serverPort "/post" $ appPost tok msg-}

{-appSync :: Token -> ClientApp ()-}
{-appSync tok conn = do-}
    {-sendTextData conn (encode v)-}
    {-forkPingThread conn 300-}
    {-syncLoop conn where-}
        {-v = object ["jstoken" .= tok]-}

{-syncLoop :: Connection -> IO ()-}
{-syncLoop conn = do-}
    {-msg <- receiveData conn-}
    {-LBS.putStrLn msg-}
    {-let Just m = decode msg-}
        {-v = object ["jsamsgid" .= String (msgid m), "jsastatus" .= String "ok"]-}
    {-sendTextData conn (encode v)-}
    {-syncLoop conn-}

{-exeSync :: Token -> IO ()-}
{-exeSync token = runClient serverIP serverPort "/sync" $ appSync token-}
