{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Protocol.Ver1.Pipeline where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Database.Bolt.Exception
import           Database.Bolt.Protocol.Ver1.Message (Message)
import qualified Database.Bolt.Protocol.Ver1.Message as Msg
import           Database.Bolt.Protocol.Ver1.Types
import           Database.Bolt.Transport
import           Database.Bolt.Transport.Message

data Response = Success Metadata [Record]
              | Failed Metadata
              | Ignored Metadata

data Command = GetResponse (MVar Response)
             | Done

data Pipeline t = Pipeline t (MVar Command)

data Pipe a = PVal (IO a)
            | PVar (IO (MVar a))

instance Functor Pipe where
    fmap g a = PVal $ fmap g (runPipe a)
    -- fmap g (PVal a) = PVal (fmap g a)
    -- fmap g (PVar a) = PVal (fmap g (readMVar =<< a))

instance Applicative Pipe where
    pure = PVal . return
    -- f <*> a = PVal $ runPipe f <*> runPipe a
    PVal f <*> PVal a = PVal (f <*> a)
    PVal f <*> PVar a =
        PVal $ do
            a' <- a
            f <*> readMVar a'
    PVar f <*> PVal a =
        PVal $ do
            f' <- f
            readMVar f' <*> a
    PVar f <*> PVar a =
        PVal $ do
            f' <- f
            a' <- a
            readMVar f' <*> readMVar a'

instance Monad Pipe where
    return = pure
    a >>= f = PVal $ runPipe a >>= runPipe . f

instance MonadIO Pipe where
    liftIO = PVal

runPipe :: Pipe a -> IO a
runPipe (PVal a) = a
runPipe (PVar v) = readMVar =<< v

request :: Transport t => Pipeline t -> Message -> Pipe Response
request (Pipeline conn cvar) msg =
    PVar $ do
        rvar <- newEmptyMVar
        putMVar cvar (GetResponse rvar)
        print ("send", msg)
        sendmsg conn msg
        return rvar

newPipeline :: Transport t => t -> IO (Pipeline t)
newPipeline conn = do
    cvar <- newEmptyMVar
    _ <- forkIO (gatherer conn cvar)
    return $ Pipeline conn cvar

gatherer :: Transport t => t -> MVar Command -> IO ()
gatherer conn cvar = do
    cmd <- takeMVar cvar
    case cmd of
        Done -> return ()
        GetResponse rvar -> do
            reply <- gather []
            putMVar rvar reply
            gatherer conn cvar
  where
    gather vals = do
        reply <- recvmsg conn
        print ("recv", reply)
        case reply of
            Msg.Success meta -> return $ Success meta (reverse vals)
            Msg.Failure meta -> return $ Failed meta
            Msg.Ignored meta -> return $ Ignored meta
            Msg.Record  val   -> gather (val : vals)
            _                 -> protocolErr "Unexpected message in response"
