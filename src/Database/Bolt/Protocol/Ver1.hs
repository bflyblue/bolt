{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Protocol.Ver1
 ( init
 , exec
 , AuthToken(..)
 -- , open
 -- , Transaction
 -- , runTransaction
 -- , cypher
 , useragent
 , transaction
 , object
 , (#=)
 , (#:)
 , (#:?)
 ) where

import           Prelude                                      hiding (init)

-- import           Database.Bolt.Exception
import           Database.Bolt.Protocol.Ver1.Message          (AuthToken (..))
import           Database.Bolt.Protocol.Ver1.Request          (exec, init)
-- import           Database.Bolt.Protocol.Ver1.Request.Pipeline as Pipeline
-- import           Database.Bolt.Protocol.Ver1.Request.Simple   as Simple
-- import           Database.Bolt.Protocol.Ver1.Transaction      (Transaction, cypher, runTransaction)
import           Database.Bolt.Protocol.Ver1.Transaction      (transaction)
import           Database.Bolt.Protocol.Ver1.Types
-- import           Database.Bolt.Transport

useragent :: UserAgent
useragent = "haskell-bolt/0.0"
