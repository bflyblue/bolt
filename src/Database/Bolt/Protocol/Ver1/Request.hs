{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Database.Bolt.Protocol.Ver1.Request where
 -- ( init
 -- , reset
 -- , discardAll
 -- , pullAll
 -- , run
 -- , exec
 -- ) where

import           Prelude                             hiding (init)

import           Control.Monad.Except
import           Data.Monoid
import           Data.PackStream
import           Data.String
import           Database.Bolt.Protocol.Ver1.Message (AuthToken, Message)
import qualified Database.Bolt.Protocol.Ver1.Message as Msg
import           Database.Bolt.Protocol.Ver1.Response as Resp
-- import           Database.Bolt.Protocol.Ver1.Request.Class
import           Database.Bolt.Protocol.Ver1.Types
import           Database.Bolt.Transport.Class
import           Data.Serialize.Get
import           Data.Serialize.Put

request :: (Transport m t, MonadError (e, Metadata) m, IsString e) => t -> Message -> m Message -- Response
request conn msg = do
    let bs = runPutLazy (pack msg)
    rbs <- message conn bs
    let reply = runGetLazy unpack rbs
    case reply of
        Left  err  -> requestError "Unable to decode response" mempty
        Right rmsg -> case rmsg of
            Left  err  -> requestError "Unable to unpack response" mempty
            Right resp -> return resp
  where
    gather vals = do
        rbs <- message conn bs
        let reply = runGetLazy unpack rbs
        case reply of
            Msg.Success meta -> return $ Resp.Success meta (reverse vals)
            Msg.Failure meta -> return $ Resp.Failed meta
            Msg.Ignored meta -> return $ Resp.Ignored meta
            Msg.Record  val  -> gather (val : vals)
            _                -> protocolErr "Unexpected message in response"

requestError :: (IsString e, MonadError (e, Metadata) m) => e -> Metadata -> m a
requestError msg meta = throwError (msg, meta)

-- simple :: (IsString e, Request m t, MonadError (e, Metadata) m) => t -> Message -> m ()
simple conn msg = do
    resp <- request conn msg
    return resp
    -- case resp of
    --     Success _meta [] -> return ()
    --     Success meta  _  -> requestError "Request not expecting records" meta
    --     Failed  meta     -> requestError "Request failed" meta
    --     Ignored meta     -> requestError "Request ignored" meta

-- detail :: (IsString e, Request m t, MonadError (e, Metadata) m) => t -> Message -> m [Record]
-- detail conn msg = do
--     resp <- request conn msg
--     case resp of
--         Success _    recs -> return recs
--         Failed  meta      -> requestError "Request failed" meta
--         Ignored meta      -> requestError "Request ignored" meta
--
-- init :: (IsString e, MonadError (e, Metadata) m, Request m t) => t -> UserAgent -> AuthToken -> m ()
-- init conn agent auth = simple conn $ Msg.Init agent auth
--
-- reset :: (IsString e, MonadError (e, Metadata) m, Request m t) => t -> m ()
-- reset conn = simple conn Msg.Reset
--
-- discardAll :: (MonadError (e, Metadata) m, Request m t, IsString e) => t -> m ()
-- discardAll conn = simple conn Msg.DiscardAll
--
-- pullAll :: (MonadError (e, Metadata) m, Request m t, IsString e) => t -> m [Record]
-- pullAll conn = detail conn Msg.PullAll
--
-- run :: (MonadError (e, Metadata) m, Request m t, IsString e) => t -> Statement -> Parameters -> m ()
-- run conn stmt params = simple conn $ Msg.Run stmt params
--
-- exec :: (IsString e, Request f t, MonadError (e, Metadata) f) => t -> Statement -> Parameters -> f [Record]
-- exec conn stmt params = run conn stmt params *> pullAll conn
{-  We use the applicative here so if m ~ Pipe things proceed asynchronously

    NOTE: Using ApplicativeDo

    exec conn stmt params = do
        run p stmt params
        pullAll p

    does not currently do the right thing.

    See: https://ghc.haskell.org/trac/ghc/ticket/10892
-}

-- exec_ :: (IsString e, Request f t, MonadError (e, Metadata) f) => t -> Statement -> Parameters -> f ()
-- exec_ conn stmt params = run conn stmt params *> pullAll conn *> pure ()
