{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
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
import System.IO.Error

import Constant
import Util
import Types
import Api

configOption :: Parser (Maybe FilePath)
configOption = optional $ strOption (short 'c' <> long "config"
                <> metavar "file" <> help "Specify config file")

initParser :: Parser (Maybe ConfigFile)
initParser = helper <*> configOption

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
                error . show $ e else do
                print e
                error "Have you run \ESC[1;33m\"init\"\ESC[0m first?"

getConfig :: Maybe FilePath -> IO (FilePath, Configuration)
getConfig conf = do
    let c = case conf of
                Nothing -> configFile
                Just c' -> c'
    config <- getSetting c
    return (c, config)

registerParser :: Parser RegisterOpts
registerParser = helper <*> ((,) <$> username <*> configfile) where
    username = optional $ strArgument (metavar "USERNAME")
    configfile = configOption

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
    (conf, config) <- getConfig c
    let user = case u of
                Nothing -> config ^. userInfoL . usernameL
                Just u' -> u'
        newconfig = config & userInfoL . usernameL .~ user
    putStrLn $ "registering as user: " ++ user
    pwd <- getRegPassword
    resp <- exeRegister newconfig (T.pack pwd)
    processResp resp "Register" $  BS.writeFile conf (Y.encode newconfig)

loginParser :: Parser LoginOpts
loginParser = registerParser

loginCmd :: LoginOpts -> IO ()
loginCmd (u, c) = do
    (conf, config) <- getConfig c
    let user = case u of
                Nothing -> config ^. userInfoL . usernameL
                Just u' -> u'
        newconfig = config & userInfoL . usernameL .~ user
    putStrLn $ "login as user: " ++ user
    pwd <- askForPassword "enter the password: "
    resp <- exeLogin newconfig (T.pack pwd)
    processResp resp "Login" $ do
        storeToken newconfig resp
        BS.writeFile conf (Y.encode newconfig)

storeToken :: Configuration -> LBS.ByteString -> IO ()
storeToken conf resp = do
    let token = resp ^. key "token" . _String
        storedir = conf ^. storeInfoL . storeDirL
        cacheFile = storedir </> cacheFileName
    createDirectoryIfMissing True storedir
    T.writeFile cacheFile $ tokenToCache (conf ^. userInfoL . usernameL) token

logoutParser :: Parser LogoutOpts
logoutParser = helper <*> configOption

logoutCmd :: LogoutOpts -> IO ()
logoutCmd c = do
    (_, config) <- getConfig c
    tok <- getTokenFromConfig config
    resp <- exeLogout config tok
    processResp resp "Logout" $ removeFile (getCacheFileFromConfig config)

postParser :: Parser PostOpts
postParser = helper <*> ((,,) <$> configfile <*> postfile <*> postdata) where
        configfile = configOption
        postfile = optional $ strOption (short 'f' <> long "file"
                    <> metavar "post_file" <> help "Specify file to post, \
                    \this has precedence over argument STRING")
        postdata = optional $ strArgument (metavar "STRING")

postCmd :: PostOpts -> IO ()
postCmd (c, f, d) = do
    (_, config) <- getConfig c
    tok <- getTokenFromConfig config
    case f of
        Just f' -> sendFile config tok f'
        Nothing -> case d of
                    Nothing -> putStrLn "Nothing to do"
                    Just str -> sendString config tok str

sendFile :: Configuration -> Token -> FilePath -> IO ()
sendFile conf token fp = readFile fp >>= sendString conf token

sendString :: Configuration -> Token -> String -> IO ()
sendString conf tok msg = do
    let !t = T.pack msg
    resp <- exePost conf tok t
    processResp resp "Post" (return ())

statusParser :: Parser StatusOpts
statusParser = helper <*> configOption

statusCmd :: StatusOpts -> IO ()
statusCmd c = do
    (_, config) <- getConfig c
    b <- doesFileExist (getCacheFileFromConfig config)
    if b then putStrLn "Seems like you have already logged in" else
            putStrLn "Seems like you have not logged in yet"

daemonParser :: Parser DaemonOpts
daemonParser = helper <*> configOption

daemonCmd :: DaemonOpts -> IO ()
daemonCmd c = do
    (_, config) <- getConfig c
    _ <- getTokenFromConfig config -- tried to query token before even connect
    exeSync config

