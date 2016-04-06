{-# LANGUAGE OverloadedStrings #-}
module Util where

import System.IO
import System.Process
import System.Random
import Control.Lens
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid 
import Types
import Constant

askForValue :: (Show a, Read a) => String -> a -> IO a
askForValue prompt def = do
    putStrLn $ prompt ++ " (default: " ++ show def ++ "):"
    cand <- getLine
    return $ if null cand then def else read cand

askForStringValue :: String -> String -> IO String
askForStringValue prompt def = do
    putStrLn $ prompt ++ " (default: " ++ def ++ "):"
    cand <- getLine
    return $ if null cand then def else cand

askForPassword :: String -> IO String
askForPassword prompt = do
    putStrLn prompt
    hSetEcho stdin False
    pwd <- getLine
    hSetEcho stdin True
    return pwd

getDeviceName :: IO String
getDeviceName = init <$> readProcess "uname" ["-r"] []

genRandomSequence :: Int -> IO String
genRandomSequence n = do
    gen <- newStdGen
    return . take n $ randomRs ('a', 'z') gen

tokenToCache :: String -> Token -> T.Text
tokenToCache username token = T.pack username <> "\n" <> token

cacheToToken :: T.Text -> (String, Token)
cacheToToken text =
    let (u:t) = T.lines text
    in (T.unpack u, T.concat t)

getTokenFromConfig :: Configuration -> IO Token
getTokenFromConfig conf = do
    let storedir = conf ^. storeInfoL . storeDirL
        cacheFile = storedir </> cacheFileName
    snd . cacheToToken <$> T.readFile cacheFile

getCacheFileFromConfig :: Configuration -> FilePath
getCacheFileFromConfig conf = conf ^. storeInfoL . storeDirL </> cacheFileName

getLogFileFromConfig :: Configuration -> FilePath
getLogFileFromConfig conf = 
    let u = conf ^. userInfoL . usernameL
    in conf ^. storeInfoL . storeDirL </> (u <> "_" <> logFileName)

setClipboard :: String -> IO ()
setClipboard msg = callCommand $ 
    "echo \"" ++ msg  ++ "\" | xclip -selection clipboard"
