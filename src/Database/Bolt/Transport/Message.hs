{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Transport.Message
    ( sendmsg
    , recvmsg
    ) where

import           Data.Monoid
import           Data.PackStream
import qualified Data.Text                       as T

import           Database.Bolt.Exception
import           Database.Bolt.Transport.Class
import qualified Database.Bolt.Transport.Chunked as Chunked

-------- Message I/O --------

sendmsg :: (Transport t, ToPackStream a) => t -> a -> IO ()
sendmsg conn = Chunked.put conn . pack

recvmsg :: (Transport t, FromPackStream a) => t -> IO a
recvmsg conn = do
    ma <- Chunked.get conn unpack
    case ma of
        Left e  -> transportErr $ "Couldn't unpack received data: " <> T.pack e
        Right a -> return a
