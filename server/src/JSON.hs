{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module JSON where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)

import Types

data JRegister = JRegister {
    jrusername :: Text,
    jrpassword :: Text
} deriving (Show)

instance FromJSON JRegister where
    parseJSON = withObject "register" $ \o ->
        JRegister <$> o .: "username" <*> o .: "password"

data JLogin = JLogin {
    jlusername :: Text,
    jlpassword :: Text,
    jldeviceName :: Text
} deriving (Show)

instance FromJSON JLogin where
    parseJSON  = withObject "login" $ \o ->
        JLogin  <$> o .: "username"
                <*> o .: "password"
                <*> o .: "deviceName"

data JPost = JPost {
    jptoken :: Token,
    jpdata :: Text
} deriving (Show)

instance FromJSON JPost where
    parseJSON = withObject "post" $ \o ->
        JPost <$> o .: "token"
              <*> o .: "data"

data JSync = JSync {
    jstoken :: Token
} deriving (Show)

instance FromJSON JSync where
    parseJSON = withObject "sync" $ \o ->
        JSync <$> o .: "token"

data JSyncAck = JSyncAck {
    jsamsgid :: Text,
    jsastatus :: Text
} deriving (Show)

instance FromJSON JSyncAck where
    parseJSON = withObject "syncack" $ \o ->
        JSyncAck <$> o .: "msgid"
                 <*> o .: "status"

data JLogout = JLogout {
    jltoken :: Text
    -- TODO: reason :: Text
} deriving (Show)

instance FromJSON JLogout where
    parseJSON = withObject "logout" $ \o ->
        JLogout <$> o .: "token"
