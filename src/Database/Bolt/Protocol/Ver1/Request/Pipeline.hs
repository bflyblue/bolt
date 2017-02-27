{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Database.Bolt.Protocol.Ver1.Request.Pipeline
 ( Pipeline
 , Pipe
 , runPipe
 , newPipeline
 , donePipeline
 , prequest
 ) where

import           Prelude                                   hiding (init)

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Database.Bolt.Exception
import           Database.Bolt.Protocol.Ver1.Message       as Msg
import           Database.Bolt.Protocol.Ver1.Request.Class
import           Database.Bolt.Protocol.Ver1.Response      as Resp
import           Database.Bolt.Transport

data Pipeline where
    Pipeline :: Transport t => t -> Chan Command -> Pipeline

data Pipe a = forall b. Pipe (IO b) (b -> IO a)

data Command = GetResponse (MVar Response)
             | Done

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

newPipeline :: Transport t => t -> IO Pipeline
newPipeline conn = do
    cmdchan <- newChan
    _ <- forkIO (gatherer conn cmdchan)
    return $ Pipeline conn cmdchan

donePipeline :: Pipeline -> IO ()
donePipeline (Pipeline _ cmdchan) = writeChan cmdchan Done

gatherer :: Transport t => t -> Chan Command -> IO ()
gatherer conn cmdchan = do
    cmd <- readChan cmdchan
    case cmd of
        Done -> return ()
        GetResponse rvar -> do
            reply <- gather []
            putMVar rvar reply
            gatherer conn cmdchan
  where
    gather vals = do
        reply <- recvmsg conn
        case reply of
            Msg.Success meta -> return $ Resp.Success meta (reverse vals)
            Msg.Failure meta -> return $ Resp.Failed meta
            Msg.Ignored meta -> return $ Resp.Ignored meta
            Msg.Record  val  -> gather (val : vals)
            _                -> protocolErr "Unexpected message in response"

prequest :: Pipeline -> Message -> Pipe Response
prequest (Pipeline conn cmdchan) msg = Pipe sendreq getresp
  where
    sendreq = do
        rvar <- newEmptyMVar
        writeChan cmdchan (GetResponse rvar)
        print ("send" :: String, msg)
        sendmsg conn msg
        return rvar

    getresp rvar = do
        a <- readMVar rvar
        print ("recv" :: String, a)
        return a

instance Request Pipe where
    type ReqH Pipe = Pipeline
    request = prequest

{-
simple :: Transport t => Pipeline t -> Message -> Pipe ()
simple p msg = void $ request p msg
  -- where
  --   case resp of
  --       Success _    [] -> return ()
  --       Success meta _  -> liftIO $ reqFail meta "Request not expecting records"
  --       Failed  meta    -> liftIO $ reqFail meta "Request failed"
  --       Ignored meta    -> liftIO $ reqIgnore meta "Request ignored"

detail :: Transport t => Pipeline t -> Message -> Pipe [Record]
detail p msg = f <$> request p msg
  where
    f (Success _    rs) = rs
    f  _                = []
        -- Failed  meta    -> liftIO $ reqFail   meta "Request failed"
        -- Ignored meta    -> liftIO $ reqIgnore meta "Request ignored"

init :: Transport t => Pipeline t -> UserAgent -> AuthToken -> Pipe ()
init p agent auth = simple p $ Msg.Init agent auth

reset :: Transport t => Pipeline t -> Pipe ()
reset p = simple p Msg.Reset

discardAll :: Transport t => Pipeline t -> Pipe ()
discardAll p = simple p Msg.DiscardAll

pullAll :: Transport t => Pipeline t -> Pipe [Record]
pullAll p = detail p Msg.PullAll

run :: Transport t => Pipeline t -> Statement -> Parameters -> Pipe ()
run p stmt params = simple p $ Msg.Run stmt params

exec :: Transport t => Pipeline t -> Statement -> Parameters -> Pipe [Record]
exec p stmt params = run p stmt params *> pullAll p

{- NOTE: Using ApplicativeDo

exec p stmt params = do
    _ <- run p stmt params
    pullAll p

is not the same as

exec p stmt params = do
    _ <- run p stmt params
    x <- pullAll p
    return x

Also see: https://ghc.haskell.org/trac/ghc/ticket/10892

-}

-}
