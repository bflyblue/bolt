{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Transport (
  Transport (..),
  put,
  get,
  getE,
  handshake,
  BoltProtocol,
  OfferProtocols (..),
  noProto,
) where

import Data.ByteString qualified as BS
import Data.Monoid
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Text as T
import Data.Word

import Database.Bolt.Exception

-- Transport

class Transport t where
  send :: t -> BS.ByteString -> IO ()
  recv :: t -> Int -> IO BS.ByteString
  close :: t -> IO ()

  sendMany :: t -> [BS.ByteString] -> IO ()
  sendMany conn = mapM_ (send conn)

put :: (Transport t) => t -> Put -> IO ()
put conn = send conn . runPut

get :: (Transport t) => t -> Int -> Get a -> IO a
get conn n g = getE conn n g >>= either bad return
 where
  bad e = transportErr $ "Bad data received: " <> T.pack e

getE :: (Transport t) => t -> Int -> Get a -> IO (Either String a)
getE conn n g = runGet g <$> recv conn n

-- Protocol negotiation

type BoltProtocol = Word32

noProto :: BoltProtocol
noProto = 0

data OfferProtocols = OfferProtocols BoltProtocol BoltProtocol BoltProtocol BoltProtocol
  deriving (Show, Eq, Ord)

handshake :: (Transport t) => t -> OfferProtocols -> IO BoltProtocol
handshake conn offer = do
  put conn (gogobolt >> offerProtocols offer)
  get conn 4 agreedProtocol

gogobolt :: Put
gogobolt = mapM_ putWord8 [0x60, 0x60, 0xb0, 0x17]

offerProtocols :: Putter OfferProtocols
offerProtocols (OfferProtocols p1 p2 p3 p4) = mapM_ offerProtocol [p1, p2, p3, p4]
 where
  offerProtocol = putWord32be

agreedProtocol :: Get BoltProtocol
agreedProtocol = getWord32be
