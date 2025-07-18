{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Database.Bolt.Protocol.Ver1.Transaction (
  Transaction,
  Tran (..),
  runTransaction,
  cypher,
) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.HashMap.Strict qualified as HM
import Database.Bolt.Protocol.Ver1.Request
import Database.Bolt.Protocol.Ver1.Types
import Database.Bolt.Transport

newtype Tran t a = Tran {unTran :: ReaderT t IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

type Transaction a = forall t. (Transport t) => Tran t a

runTransaction :: (Transport t) => t -> Transaction a -> IO a
runTransaction conn t = do
  _ <- exec conn "BEGIN" HM.empty
  r <- try $ runReaderT (unTran t) conn
  case r of
    Left (SomeException ex) -> do
      reset conn
      throwIO ex
    Right a -> do
      _ <- exec conn "COMMIT" HM.empty
      return a

getConn :: Tran t t
getConn = Tran ask

cypher :: Statement -> Parameters -> Transaction [Record]
cypher stmt params = do
  conn <- getConn
  liftIO $ exec conn stmt params
