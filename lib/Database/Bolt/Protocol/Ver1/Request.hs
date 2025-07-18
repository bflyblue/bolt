{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Protocol.Ver1.Request (
  init,
  reset,
  discardAll,
  pullAll,
  run,
  exec,
) where

import Prelude hiding (init)

import Database.Bolt.Exception
import Database.Bolt.Protocol.Ver1.Message (AuthToken, Message)
import Database.Bolt.Protocol.Ver1.Message qualified as Msg
import Database.Bolt.Protocol.Ver1.Types
import Database.Bolt.Transport
import Database.Bolt.Transport.Message

data Response
  = Success !Metadata ![Record]
  | Failed !Metadata
  | Ignored !Metadata

request :: (Transport t) => t -> Message -> IO Response
request conn msg = do
  -- print ("send", msg)
  sendmsg conn msg
  gather []
 where
  gather vals = do
    reply <- recvmsg conn
    -- print ("recv", reply)
    case reply of
      Msg.Success meta -> return $ Success meta (reverse vals)
      Msg.Failure meta -> return $ Failed meta
      Msg.Ignored meta -> return $ Ignored meta
      Msg.Record val -> gather (val : vals)
      _other -> protocolErr "Unexpected message in response"

simple :: (Transport t) => t -> Message -> IO ()
simple conn msg = do
  resp <- request conn msg
  case resp of
    Success _ [] -> return ()
    Success meta _ -> reqFail meta "Request not expecting records"
    Failed meta -> reqFail meta "Request failed"
    Ignored meta -> reqIgnore meta "Request ignored"

detail :: (Transport t) => t -> Message -> IO [Record]
detail conn msg = do
  resp <- request conn msg
  case resp of
    Success _ rs -> return rs
    Failed meta -> reqFail meta "Request failed"
    Ignored meta -> reqIgnore meta "Request ignored"

init :: (Transport t) => t -> UserAgent -> AuthToken -> IO ()
init conn agent auth = simple conn $ Msg.Init agent auth

reset :: (Transport t) => t -> IO ()
reset conn = simple conn Msg.Reset

discardAll :: (Transport t) => t -> IO ()
discardAll conn = simple conn Msg.DiscardAll

pullAll :: (Transport t) => t -> IO [Record]
pullAll conn = detail conn Msg.PullAll

run :: (Transport t) => t -> Statement -> Parameters -> IO ()
run conn stmt params = simple conn $ Msg.Run stmt params

exec :: (Transport t) => t -> Statement -> Parameters -> IO [Record]
exec conn stmt params = do
  run conn stmt params
  pullAll conn
