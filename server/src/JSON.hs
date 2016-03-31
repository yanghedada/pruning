{-# LANGUAGE DeriveGeneric #-}
module JSON where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)

data JRegister = JRegister {
    username :: Text,
    password :: Text
} deriving (Show, Generic)

instance FromJSON JRegister
