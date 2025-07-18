{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Protocol.Ver1 (
  init,
  exec,
  AuthToken (..),
  open,
  Transaction,
  runTransaction,
  cypher,
  object,
  (#=),
  (#:),
  (#:?),
) where

import Control.Monad
import Prelude hiding (init)

import Database.Bolt.Exception
import Database.Bolt.Protocol.Ver1.Message (AuthToken (..))
import Database.Bolt.Protocol.Ver1.Request (exec, init)
import Database.Bolt.Protocol.Ver1.Transaction (Transaction, cypher, runTransaction)
import Database.Bolt.Protocol.Ver1.Types
import Database.Bolt.Transport

useragent :: UserAgent
useragent = "haskell-bolt/0.0"

open :: (Transport t) => (a -> IO t) -> a -> AuthToken -> IO t
open c a auth = do
  conn <- c a
  agreed <- handshake conn (OfferProtocols 1 noProto noProto noProto)
  unless (agreed == 1) $
    protocolErr "Can't negotiate protocol version"
  init conn useragent auth
  return conn
