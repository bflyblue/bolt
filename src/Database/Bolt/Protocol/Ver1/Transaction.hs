{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Bolt.Protocol.Ver1.Transaction
 ( Transaction
 , runTransaction
 , cypher
 ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
-- import           Database.Bolt.Exception
import qualified Data.HashMap.Strict                 as HM
import           Database.Bolt.Protocol.Ver1.Request
import           Database.Bolt.Protocol.Ver1.Types
import           Database.Bolt.Transport

newtype Transaction t a = Transaction { unTransaction :: ReaderT t IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

runTransaction :: Transport t => t -> Transaction t a -> IO a
runTransaction conn t = do
    _ <- exec conn "BEGIN" HM.empty
    r <- try $ runReaderT (unTransaction t) conn
    case r of
        Left (SomeException ex) -> do
            _ <- exec conn "ROLLBACK" HM.empty
            throwIO ex
        Right a -> do
            _ <- exec conn "COMMIT" HM.empty
            return a

getConn :: Transaction t t
getConn = Transaction ask

cypher :: Transport t => Statement -> Parameters -> Transaction t [Record]
cypher stmt params = do
    conn <- getConn
    liftIO $ exec conn stmt params
