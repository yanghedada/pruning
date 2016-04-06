{-# LANGUAGE OverloadedStrings #-}
module OptParse where

import Data.Typeable
import Options.Applicative
import System.Directory
import System.IO
import System.FilePath.Posix
import qualified Data.Yaml as Y
import Data.Yaml.Config
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Lens
import Data.Aeson.Lens
import Control.Exception
import System.IO.Error (isDoesNotExistError)

import Constant
import Util
import Types
import Api

initParser :: Parser (Maybe ConfigFile)
initParser = optional $ helper <*> strOption (short 'c' <> long "config"
                <> metavar "file" <> help "Specify config file")

initCmd :: Maybe FilePath -> IO ()
initCmd fp = do
    let conf = case fp of
                    Just x -> x
                    Nothing -> configFile
    ip <- askForStringValue "What's the server IP" defaultServerIP
    port <- askForValue "What's the server port for connection" defaultServerPort
    username <- askForStringValue "What's the username do you want to use" ""
    storedir <- askForStringValue "Where do you want to store everything" defaultStoreDir
    dev <- getDeviceName
    let userconf = Conf (ServerI ip port) (UserI username dev) (StoreI storedir)
    createDirectoryIfMissing True storedir
    BS.writeFile conf $ Y.encode userconf
    putStrLn $ "config saved as: \ESC[1;33m" ++ conf ++ "\ESC[0m"

getSetting :: FilePath -> IO Configuration
getSetting conf =
    catch (loadYamlSettings [conf] [] ignoreEnv :: IO Configuration) handler where
        handler :: Y.ParseException -> IO Configuration
        handler e = do
            ext <- doesFileExist conf
            if ext then do
                putStrLn "The config file is invalid"
                error . show $ e else
                error "Have you run \ESC[1;33m\"init\"\ESC[0m first?"

registerParser :: Parser RegisterOpts
registerParser = helper <*> ((,) <$> username <*> configfile) where
    username = optional $ strArgument (metavar "USERNAME")
    configfile = optional $ strOption (short 'c' <> long "config"
                            <> metavar "file" <> help "Specify config file")

getRegPassword :: IO String
getRegPassword = do
    pwd1 <- askForPassword "enter the password: "
    pwd2 <- askForPassword "enter the password again: "
    if pwd1 == pwd2 then
        if null pwd1    then putStrLn "can not empty" >> getRegPassword
                        else return pwd1
                    else
        putStrLn "password mismatched, try again: " >> getRegPassword

registerCmd :: RegisterOpts -> IO ()
registerCmd (u, c) = do
    let conf = case c of
                Nothing -> configFile
                Just c' -> c'
    config <- getSetting conf
    let user = case u of
                Nothing -> config ^. userInfoL . usernameL
                Just u' -> u'
        newconfig = config & userInfoL . usernameL .~ user
    putStrLn $ "registering as user: " ++ user
    pwd <- getRegPassword
    resp <- exeRegister newconfig (T.pack pwd)
    if resp ^?! key "code" . _Number == 200 then
        putStrLn "Register successfully" >> BS.writeFile conf (Y.encode newconfig)
        else T.putStrLn $
            "Register failed, the server responded: " <> resp ^. key "msg" . _String

loginParser :: Parser LoginOpts
loginParser = registerParser

loginCmd :: LoginOpts -> IO ()
loginCmd (u, c) = do
    let conf = case c of
                Nothing -> configFile
                Just c' -> c'
    config <- getSetting conf
    let user = case u of
                Nothing -> config ^. userInfoL . usernameL
                Just u' -> u'
        newconfig = config & userInfoL . usernameL .~ user
    putStrLn $ "login as user: " ++ user
    pwd <- askForPassword "enter the password: "
    resp <- exeLogin newconfig (T.pack pwd)
    if resp ^?! key "code" . _Number == 200 then do
        putStrLn "Login successfully"
        storeToken newconfig resp
        BS.writeFile conf (Y.encode newconfig) 
        else T.putStrLn $
            "Login failed, the server responded: " <> resp ^. key "msg" . _String

storeToken :: Configuration -> LBS.ByteString -> IO ()
storeToken conf resp = do
    let token = resp ^. key "token" . _String
        storedir = conf ^. storeInfoL . storeDirL
        cacheFile = storedir </> cacheFileName
    createDirectoryIfMissing True storedir
    T.writeFile cacheFile $ tokenToCache (conf ^. userInfoL . usernameL) token

tokenToCache :: String -> T.Text -> T.Text
tokenToCache username token = T.pack username <> "\n" <> token

cacheToToken :: T.Text -> (String, T.Text)
cacheToToken text =
    let (u:t) = T.lines text
    in (T.unpack u, T.concat t)

logoutParser :: Parser LogoutOpts
logoutParser = helper <*> (optional $ strOption (long "config" <> short 'c' 
    <> metavar "file" <> help "Specify config file"))

logoutCmd :: LogoutOpts -> IO ()
logoutCmd c = do
    let conf = case c of
                Nothing -> configFile
                Just c' -> c'
    config <- getSetting conf
    let storedir = config ^. storeInfoL . storeDirL
        cacheFile = storedir </> cacheFileName
    (_, tok) <- cacheToToken <$> T.readFile cacheFile
    resp <- exeLogout config tok
    if resp ^?! key "code" . _Number == 200 then do
        putStrLn "Logout successfully"
        removeFile cacheFile else T.putStrLn $
        "Logout failed, the server responded: " <> resp ^. key "msg" . _String


