{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

import Constant
import Types

share [mkPersist sqlSettings, mkSave "entityDefs", mkMigrate "migrateAll"] [persistLowerCase|
User
    username        Text
    UniqueUsername  username
    password        Text
    aeskey          Text
    deriving        Show

IdMap
    user     UserId
    tokens   [Token]
    UniqueUser user
    deriving Show

TokenMap
    token   Token
    UniqueToken token
    user    UserId
    deriving Show
|]
