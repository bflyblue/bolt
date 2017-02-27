{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Protocol.Ver1.Request.Simple
 ( Connection(..)
 , fromTransport
 , request
 ) where

import           Prelude                              hiding (init)

import           Database.Bolt.Exception
import           Database.Bolt.Protocol.Ver1.Message  as Msg
import           Database.Bolt.Protocol.Ver1.Response as Resp
import           Database.Bolt.Transport

data Connection where
    Connection :: Transport t => t -> Connection

fromTransport :: Transport t => t -> Connection
fromTransport = Connection

sendRequest :: Transport t => t -> Message -> IO ()
sendRequest =
    -- print ("send", msg)
    sendmsg

getResponse :: Transport t => t -> IO Response
getResponse conn = gather []
  where
    gather vals = do
        reply <- recvmsg conn
        -- print ("recv", reply)
        case reply of
            Msg.Success meta -> return $ Resp.Success meta (reverse vals)
            Msg.Failure meta -> return $ Resp.Failed meta
            Msg.Ignored meta -> return $ Resp.Ignored meta
            Msg.Record  val  -> gather (val : vals)
            _                -> protocolErr "Unexpected message in response"

request :: Transport t => t -> Message -> IO Response
request conn msg = do
    sendRequest conn msg
    getResponse conn
