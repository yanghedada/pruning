{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import GHC.Generics
import Data.Yaml
import Control.Lens
import Lens

data ServerInfo = ServerI {
    serverIP :: String,
    serverPort :: Int
} deriving (Generic, Show)
makeLensesL ''ServerInfo

data UserInfo = UserI {
    username :: String,
    devicename :: String
} deriving (Generic, Show)
makeLensesL ''UserInfo

data StoreInfo = StoreI {
    storeDir :: FilePath
} deriving (Generic, Show)
makeLensesL ''StoreInfo

data Configuration = Conf {
    serverInfo :: ServerInfo,
    userInfo :: UserInfo,
    storeInfo :: StoreInfo
} deriving (Generic, Show)
makeLensesL ''Configuration

instance FromJSON Configuration
instance ToJSON Configuration

instance FromJSON ServerInfo
instance ToJSON ServerInfo

instance FromJSON UserInfo
instance ToJSON UserInfo

instance FromJSON StoreInfo
instance ToJSON StoreInfo

type Username = String
type ConfigFile = FilePath
type RegisterOpts = (Maybe Username, Maybe ConfigFile)
type LoginOpts = RegisterOpts
type LogoutOpts = Maybe ConfigFile
