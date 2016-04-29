module Constant where

import System.Directory
import System.IO.Unsafe
import System.FilePath.Posix

toolName :: String
toolName = "cliper"

clientName :: String
clientName = toolName ++ "c"

daemonName :: String
daemonName = toolName ++ "d"

configFile' :: FilePath
configFile' = "." ++ toolName ++ ".yaml"

configFile :: FilePath
configFile =  unsafePerformIO $ (</> configFile') <$> getHomeDirectory

defaultServerIP :: String
defaultServerIP = "104.207.144.233"

defaultServerPort :: Int
defaultServerPort = 4564

storeDir' :: FilePath
storeDir' = "." ++ toolName ++ "/"

defaultStoreDir :: FilePath
defaultStoreDir = unsafePerformIO $ (</> storeDir') <$> getHomeDirectory

cacheFileName :: FilePath
cacheFileName = ".cache"

logFileName :: FilePath
logFileName = "message.log"
