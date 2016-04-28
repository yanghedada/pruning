{-# LANGUAGE OverloadedStrings #-}
module Logger where

import Network.WebSockets
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Monoid
import Data.Maybe

logNoUrl :: PendingConnection -> IO ()
logNoUrl pend = do
    let path = requestPath $ pendingRequest pend
    BS.putStrLn $ "invalid url access detected -- " <> path

logInvalidJson :: LBS.ByteString -> IO ()
logInvalidJson t = LBS.putStrLn $ "invalid json detected -- " <> t

