{-# LANGUAGE OverloadedStrings #-}
module Api where

import Control.Monad
import Network.WebSockets
import Data.Aeson
import Data.Text as T (Text, null) 
import qualified Data.ByteString.Lazy.Char8 as LBS
import Database.Persist
import Database.Persist.Sqlite hiding (Connection)
import Data.Scientific
import Control.Exception
import qualified Data.List as L
import Control.Concurrent
import qualified Data.HashMap.Lazy as HM
import Control.Monad.IO.Class (liftIO)
import System.Timeout
import Data.Maybe (fromJust)

import Crypto
import JSON
import Database
import Constant
import Types

respondRegisterMessage :: Connection -> Scientific -> Text -> IO ()
respondRegisterMessage conn code msg = sendTextData conn (encode obj) where
        obj = object [ "code" .= Number code, "msg" .= String msg]

respondLoginMessage :: Connection -> Scientific -> Text -> Token -> IO ()
respondLoginMessage conn code msg token = sendTextData conn (encode obj) where
        obj = object [  "code" .= Number code,
                        "msg" .= String msg,
                        "token" .= String token]

respondLogoutMessage :: Connection -> Scientific -> Text -> IO ()
respondLogoutMessage = respondRegisterMessage

respondPostMessage :: Connection -> Scientific -> Text -> IO ()
respondPostMessage = respondRegisterMessage

respondSyncErrorMessage :: Connection -> Scientific -> Text -> IO ()
respondSyncErrorMessage = respondRegisterMessage

appRegister :: Connection -> IO ()
appRegister conn = do
    jregister <- liftM decode $ receiveData conn
    case jregister of
        Just jr -> performRegisterAction conn jr
        Nothing -> respondRegisterMessage conn 400 "bad request"

performRegisterAction :: Connection -> JRegister -> IO ()
performRegisterAction conn jr =
    catch (
        if T.null u then respondRegisterMessage conn 422 "username cannot be empty" 
        else do
            runSqlite sqlTable $ insert $ User u p key
            respondRegisterMessage conn 200 "") handler where
                u = jrusername jr
                p = jrpassword jr
                key = generateAesKey u p
                handler (SomeException _) =
                    respondRegisterMessage conn 422
                        "exception during database operation, probably because the\
                        \ username has been taken"

appLogin :: MVar MessagePool -> Connection -> IO ()
appLogin msgp conn = do
    jlogin <- liftM decode $ receiveData conn
    case jlogin of
        Just jl -> performLoginAction conn jl msgp
        Nothing -> respondLoginMessage conn 400 "bad request" ""

performLoginAction :: Connection -> JLogin -> MVar MessagePool -> IO ()
performLoginAction conn jl msgp = do
    maybeUser <- runSqlite sqlTable $ selectFirst
        [UserUsername ==. jlusername jl, UserPassword ==. jlpassword jl] []
    case maybeUser of
        Just (Entity uid _) -> do
            tok <- genRandomToken
            b <- insertTokenDb tok uid msgp
            mp <- readMVar msgp
            {-print mp-}
            if b then respondLoginMessage conn 200 "" tok else
                        respondLoginMessage conn 422 sqlErrorStr ""
        Nothing -> respondLoginMessage conn 422 "username / password mismatch" ""

insertTokenDb :: Token -> Key User -> MVar MessagePool -> IO Bool
insertTokenDb tok uid msgp =
    catch (
        runSqlite sqlTable $ do
            insert $ TokenMap tok uid
            ts <- selectFirst [IdMapUser ==. uid] []
            case ts of
                Nothing ->
                    void $ insert $ IdMap uid [tok]
                Just (Entity rid (IdMap _ tks)) -> update rid [IdMapTokens =. (tok:tks)]
            liftIO $ modifyMVar_ msgp (return . HM.insert tok [])
            return True) handler where
                handler (SomeException _) = return False

appPost :: MVar MessagePool -> Connection -> IO ()
appPost msgPool conn = do
    jpost <- liftM decode $ receiveData conn
    case jpost of
        Just jp -> performPostAction conn jp msgPool
        Nothing -> respondPostMessage conn 400 "bad request"

performPostAction :: Connection -> JPost -> MVar MessagePool -> IO ()
performPostAction conn jp msgp = do
    mtokenMap <- runSqlite sqlTable $ selectFirst [TokenMapToken ==. jptoken jp] []
    case mtokenMap of
        Just (Entity _ tokenmap) -> do
            addMessageToToken (tokenMapUser tokenmap) (jptoken jp) (jpdata jp) msgp
            respondPostMessage conn 200 ""
        Nothing -> respondPostMessage conn 422 "no such token"

addMessageToToken :: Key User -> Token -> Text -> MVar MessagePool -> IO ()
addMessageToToken uid token msg msgp = do
    midmap <- runSqlite sqlTable $ selectFirst [IdMapUser ==. uid] []
    case midmap of
        Nothing -> error "OMG!!! BUGS!" -- this should not happen
        Just (Entity _ (IdMap _ toks)) ->
            forM_ (L.delete token toks) (\tok ->
                modifyMVar_ msgp (return . HM.adjust (++[msg]) tok))

appLogout :: MVar MessagePool -> Connection -> IO ()
appLogout msgp conn = do
    jlogout <- liftM decode $ receiveData conn
    case jlogout of
        Just jl -> performLogoutAction conn jl msgp
        Nothing -> respondLogoutMessage conn 400 "bad request"

performLogoutAction :: Connection -> JLogout -> MVar MessagePool -> IO ()
performLogoutAction conn jl msgp = do
    mtokenMap <- runSqlite sqlTable $ selectFirst [TokenMapToken ==. jltoken jl] []
    case mtokenMap of
        Just (Entity _ tokenmap) -> do
            b <- deleteTokenDb (jltoken jl) (tokenMapUser tokenmap) msgp
            if b then respondLogoutMessage conn 200 "" else
                    respondLogoutMessage conn 422 sqlErrorStr
        Nothing -> respondLogoutMessage conn 422 "no such token"

deleteTokenDb :: Token -> Key User -> MVar MessagePool -> IO Bool
deleteTokenDb tok uid msgp =
    catch (runSqlite sqlTable $ do
        deleteWhere [TokenMapToken ==. tok]
        ts <- selectFirst [IdMapUser ==. uid] []
        case ts of
            Nothing -> return False -- if this happens, we have severe bug(s)
            Just (Entity rid (IdMap _ tks)) -> do
                            update rid [IdMapTokens =. L.delete tok tks]
                            liftIO $ modifyMVar_ msgp (return . HM.delete tok)
                            return True) handler where
                        handler (SomeException _) = return False

appSync :: MVar MessagePool -> Connection -> IO ()
appSync msgp conn = do
    jsync <- liftM decode $ receiveData conn
    case jsync of
        Nothing -> respondSyncErrorMessage conn 400 "bad request"
        Just js -> do
            mp <- readMVar msgp
            if jstoken js `HM.member` mp then do
                forkIO $ keepAlive conn
                sendMessagesOfToken (jstoken js) msgp conn
            else respondSyncErrorMessage conn 422 "no such token"

keepAlive :: Connection -> IO ()
keepAlive conn = do
    (receiveData conn :: IO Text) >>= sendTextData conn
    keepAlive conn

sendMessagesOfToken :: Token -> MVar MessagePool -> Connection -> IO ()
sendMessagesOfToken token msgp conn = do
    mp <- readMVar msgp
    let handler (PatternMatchFail _) = return Nothing
    msgs' <- catch (return $ HM.lookup token mp) handler
    case msgs' of
        Nothing -> return ()
        Just msgs ->
            if L.null msgs then do
                threadDelay 500000
                sendMessagesOfToken token msgp conn
            else do
            msgid <- genRandomMsgId
            let obj = object [ "msg" .= String (head msgs),
                                "msgid" .= String msgid ]
            sendTextData conn (encode obj)
            jsa' <- timeout 3000000 $ liftM decode $ receiveData conn
            case jsa' of
                Just (Just jsa) ->
                    if jsamsgid jsa == msgid && jsastatus jsa == "ok" then do
                        modifyMVar_ msgp (return . HM.adjust tail token)
                        sendMessagesOfToken token msgp conn else
                            sendMessagesOfToken token msgp conn
                Just Nothing -> respondSyncErrorMessage conn 400
                    "you don't know how to respond to my message!"
                Nothing -> return ()

