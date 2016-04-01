{-# LANGUAGE OverloadedStrings #-}
module Constant where

import Data.Text (Text)

sqlTable :: Text
sqlTable = "data.db"

sqlErrorStr :: Text
sqlErrorStr = "error while executing sql command"
