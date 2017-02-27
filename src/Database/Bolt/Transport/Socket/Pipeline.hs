{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Database.Bolt.Transport.Socket.Pipeline
    ( TS.ConnInfo
    , TS.hostserv
    , TS.uri

    , Connection
    , open
    , open'

    , Pipe

    ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

import qualified Data.ByteString.Lazy              as LBS
import           Database.Bolt.Transport.Class
import           Database.Bolt.Transport.Handshake
import qualified Database.Bolt.Transport.Socket    as TS

-------- Socket --------

data Command = GetResponse (MVar LBS.ByteString)
             | Done

data Connection = Connection TS.Connection (Chan Command)

data Pipe a = forall b. Pipe (IO b) (b -> IO a)

instance Functor Pipe where
    fmap g (Pipe a f) = Pipe a (return . g <=< f)

instance Applicative Pipe where
    pure a = Pipe (return a) return
    Pipe a1 g1 <*> Pipe a2 g2 =
        Pipe ((,) <$> a1 <*> a2)
          ( \(v1, v2) -> do
                r1 <- g1 v1
                r2 <- g2 v2
                return $ r1 r2
          )

instance Monad Pipe where
    return = pure
    a >>= f = Pipe (runPipe a >>= runPipe . f) return

instance MonadIO Pipe where
    liftIO a = Pipe a return

runPipe :: Pipe a -> IO a
runPipe (Pipe a f) = a >>= f

instance Transport Pipe Connection where
    message (Connection conn cmdchan) msg = Pipe sendmsg getreply
      where
        sendmsg = do
            rvar <- newEmptyMVar
            writeChan cmdchan (GetResponse rvar)
            print ("send" :: String, msg)
            TS.sendMessage conn msg
            return rvar

        getreply rvar = do
            a <- readMVar rvar
            print ("recv" :: String, a)
            return a

    close (Connection conn chan) = liftIO $ writeChan chan Done >> close conn

open :: MonadIO m => TS.ConnInfo -> BoltProtocol -> m (Maybe Connection)
open conninfo proto = do
    mconn <- TS.open conninfo proto
    case mconn of
        Just conn -> Just <$> startPipeline conn
        Nothing   -> return Nothing

open' :: MonadIO m => TS.ConnInfo -> [BoltProtocol] -> m (Connection, Maybe BoltProtocol)
open' conninfo protos = do
    (conn, proto) <- TS.open' conninfo protos
    pconn <- startPipeline conn
    return (pconn, proto)

startPipeline :: MonadIO m => TS.Connection -> m Connection
startPipeline conn = liftIO $ do
    cmdchan <- newChan
    let c = Connection conn cmdchan
    _ <- forkIO (gatherer c)
    return c

gatherer :: Connection -> IO ()
gatherer (Connection conn chan) = loop
  where
    loop = do
        cmd <- readChan chan
        case cmd of
            Done -> return ()
            GetResponse rvar -> do
                msg <- TS.recvMessage conn
                putMVar rvar msg
                loop
