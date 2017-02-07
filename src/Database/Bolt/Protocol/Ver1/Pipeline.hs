{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Protocol.Ver1.Pipeline where

import           Control.Monad
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
    deriving (Show)

data Command = GetResponse (MVar Response)
             | Done

data Pipeline t = Pipeline t (MVar Command)

data Pipe a = forall b. PVal (b -> IO a) (IO b)

instance Functor Pipe where
    fmap g (PVal f a) = PVal (return . g <=< f) a

instance Applicative Pipe where
    pure a = PVal return (return a)
    PVal g1 a1 <*> PVal g2 a2 =
        PVal
          ( \(v1, v2) -> do
                r1 <- g1 v1
                r2 <- g2 v2
                return $ r1 r2
          )
          ( do
            v1 <- a1
            v2 <- a2
            return (v1, v2)
          )

instance Monad Pipe where
    return = pure
    a >>= f = PVal return (runPipe a >>= runPipe . f)

instance MonadIO Pipe where
    liftIO = PVal return

runPipe :: Pipe a -> IO a
runPipe (PVal f a) = a >>= f

request :: Transport t => Pipeline t -> Message -> Pipe Response
request (Pipeline conn cvar) msg =
    PVal go $ do
        rvar <- newEmptyMVar
        putMVar cvar (GetResponse rvar)
        print ("send", msg)
        sendmsg conn msg
        return rvar
  where
    go v = do
        a <- readMVar v
        print ("recv", a)
        return a

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
        case reply of
            Msg.Success meta -> return $ Success meta (reverse vals)
            Msg.Failure meta -> return $ Failed meta
            Msg.Ignored meta -> return $ Ignored meta
            Msg.Record  val   -> gather (val : vals)
            _                 -> protocolErr "Unexpected message in response"
