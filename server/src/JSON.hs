{-# LANGUAGE DeriveGeneric #-}
module JSON where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)

import Types

-- TODO: create FromJSON instance manually to ease the nameming issue

data JRegister = JRegister {
    jrusername :: Text,
    jrpassword :: Text
} deriving (Show, Generic)

instance FromJSON JRegister

data JLogin = JLogin {
    jlusername :: Text,
    jlpassword :: Text,
    jldeviceName :: Text
} deriving (Show, Generic)

instance FromJSON JLogin

data JPost = JPost {
    jptoken :: Token,
    jpdata :: Text
} deriving (Show, Generic)

instance FromJSON JPost

data JSync = JSync {
    jstoken :: Token
} deriving (Show, Generic)

instance FromJSON JSync

data JLogout = JLogout {
    jltoken :: Text
    -- TODO: reason :: Text
} deriving (Show, Generic)

instance FromJSON JLogout
