{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Transport.Class
    ( Transport(..)
    , put
    , get
    , getE
    ) where

import qualified Data.ByteString         as BS
import           Data.Monoid
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Data.Text               as T

import           Database.Bolt.Exception

-- Transport

class Transport t where
    send :: t -> BS.ByteString -> IO ()
    recv :: t -> Int -> IO BS.ByteString
    close :: t -> IO ()

    sendMany :: t -> [BS.ByteString] -> IO ()
    sendMany conn = mapM_ (send conn)

put :: Transport t => t -> Put -> IO ()
put conn = send conn . runPut

get :: Transport t => t -> Int -> Get a -> IO a
get conn n g = getE conn n g >>= either bad return
  where
    bad e = transportErr $ "Bad data received: " <> T.pack e

getE :: Transport t => t -> Int -> Get a -> IO (Either String a)
getE conn n g = runGet g <$> recv conn n
