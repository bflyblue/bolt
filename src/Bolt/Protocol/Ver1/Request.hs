{-# LANGUAGE OverloadedStrings #-}

module Bolt.Protocol.Ver1.Request
 ( init
 , reset
 , discardAll
 , pullAll
 , run
 , exec
 ) where

import           Prelude                    hiding (init)

import           Bolt.Exception
import           Bolt.Protocol.Ver1.Message (AuthToken, Message)
import qualified Bolt.Protocol.Ver1.Message as Msg
import           Bolt.Protocol.Ver1.Types
import           Bolt.Transport
import           Bolt.Transport.Message

data Response = Success [Record]
              | Failed
              | Ignored

request :: Transport t => t -> Message -> IO Response
request conn msg = do
    sendmsg conn msg
    gather []
  where
    gather vals = do
        reply <- recvmsg conn
        case reply of
            Msg.Success _meta -> return $ Success (reverse vals)
            Msg.Failure _meta -> return Failed
            Msg.Ignored _meta -> return Ignored
            Msg.Record  val   -> gather (val : vals)
            _                 -> protocolErr "Unexpected message in response"

simple :: Transport t => t -> Message -> IO ()
simple conn msg = do
    resp <- request conn msg
    case resp of
        Success [] -> return ()
        Success _  -> reqFail   "Request not expecting records"
        Failed     -> reqFail   "Request failed"
        Ignored    -> reqIgnore "Request ignored"

detail :: Transport t => t -> Message -> IO [Record]
detail conn msg = do
    resp <- request conn msg
    case resp of
        Success rs -> return rs
        Failed     -> reqFail   "Request failed"
        Ignored    -> reqIgnore "Request ignored"

init :: Transport t => t -> UserAgent -> AuthToken -> IO ()
init conn agent auth = simple conn $ Msg.Init agent auth

reset :: Transport t => t -> IO ()
reset conn = simple conn Msg.Reset

discardAll :: Transport t => t -> IO ()
discardAll conn = simple conn Msg.DiscardAll

pullAll :: Transport t => t -> IO [Record]
pullAll conn = detail conn Msg.PullAll

run :: Transport t => t -> Statement -> Parameters -> IO ()
run conn stmt params = simple conn $ Msg.Run stmt params

exec :: Transport t => t -> Statement -> Parameters -> IO [Record]
exec conn stmt params = do
    run conn stmt params
    pullAll conn
