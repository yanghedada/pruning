module Util where

import System.IO
import System.Process

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
