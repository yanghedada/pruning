module Crypto where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text, pack)
import System.Random

import Types

aesEncrypt :: ByteString -> ByteString
aesEncrypt = id

aesDecrypt :: ByteString -> ByteString
aesDecrypt = id

rsaEncrypt :: ByteString -> ByteString
rsaEncrypt = id

rsaDecrypt :: ByteString -> ByteString
rsaDecrypt = id

-- generate aes key used for encryption
-- from username and password
generateAesKey :: Text -> Text -> Token
generateAesKey t1 t2 = t1 <> t2

genRandomToken' :: Int -> IO Token
genRandomToken' n = do
    gen <- newStdGen
    return . pack . take n $ randomRs ('A', 'Z') gen

genRandomToken :: IO Token
genRandomToken = genRandomToken' 32
