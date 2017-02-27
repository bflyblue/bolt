{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE FlexibleContexts                 #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Bolt.Protocol.Ver1.Transaction where
 -- ( Transaction
 -- , runTransaction
 -- , cypher
 -- ) where

-- import           Control.Exception
import           Control.Monad.Except
-- import           Control.Monad.IO.Class
-- import           Control.Monad.Trans.Reader
-- import           Data.Functor.Identity
import Data.String
import qualified Data.HashMap.Strict                 as HM
import           Database.Bolt.Protocol.Ver1.Request
import           Database.Bolt.Protocol.Ver1.Types
import           Database.Bolt.Protocol.Ver1.Request.Class
-- import           Database.Bolt.Protocol.Ver1.Types
-- import           Database.Bolt.Transport

transaction :: (Request m t, IsString e, MonadError (e, Metadata) m) => t -> (t -> m a) -> m a
transaction conn act = do
    exec_ conn "BEGIN" HM.empty
    a <- catchError (act conn) rollbackRethrow
    exec_ conn "COMMIT" HM.empty
    return a
  where
    rollbackRethrow e = do
       exec_ conn "ROLLBACK" HM.empty
       throwError e
