{-# LANGUAGE OverloadedStrings #-}

module Bolt.Protocol.Ver1
 ( init
 , exec
 , AuthToken(..)
 , open
 ) where

import           Prelude                    hiding (init)
import           Control.Monad

import           Bolt.Exception
import           Bolt.Protocol.Ver1.Message (AuthToken(..))
import           Bolt.Protocol.Ver1.Request (init, exec)
import           Bolt.Protocol.Ver1.Types
import           Bolt.Transport

useragent :: UserAgent
useragent = "haskell-bolt/0.0"

open :: Transport t => (a -> IO t) -> a -> AuthToken -> IO t
open c a auth = do
    conn <- c a
    agreed <- handshake conn (OfferProtocols 1 noProto noProto noProto)
    unless (agreed == 1) $
        protocolErr "Can't negotiate protocol version"
    init conn useragent auth
    return conn
