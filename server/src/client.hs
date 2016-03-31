{-# LANGUAGE OverloadedStrings #-}
import Network.WebSockets
import Data.Aeson
import Data.Text.IO as TIO

main :: IO ()
main = runClient "127.0.0.1" 4564 "/register" app

app :: ClientApp ()
app conn = do
    sendTextData conn (encode v)
    msg <- receiveData conn
    TIO.putStrLn msg where
        v = object ["userame" .= String "frefreak", "password" .= String "123456"]



