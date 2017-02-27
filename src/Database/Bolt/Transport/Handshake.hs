{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Transport.Handshake
    ( BoltProtocol
    , OfferProtocols(..)
    , noProto
    , gogobolt
    , offerProtocols
    , agreedProtocol
    ) where

import           Data.Serialize.Get
import           Data.Serialize.Put
import           Data.Word

type BoltProtocol = Word32

noProto :: BoltProtocol
noProto = 0

data OfferProtocols = OfferProtocols BoltProtocol BoltProtocol BoltProtocol BoltProtocol
    deriving (Show, Eq, Ord)

gogobolt :: Put
gogobolt = mapM_ putWord8 [0x60, 0x60, 0xb0, 0x17]

offerProtocols :: Putter OfferProtocols
offerProtocols (OfferProtocols p1 p2 p3 p4) = mapM_ offerProtocol [p1, p2, p3, p4]
  where
    offerProtocol = putWord32be

agreedProtocol :: Get BoltProtocol
agreedProtocol = getWord32be
