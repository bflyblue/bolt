{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Bolt.Transport.Socket (
  ConnInfo,
  hostserv,
  uri,
  Connection,
  connect,
  close,
  send,
  sendMany,
  recv,
  put,
  get,
) where

import Control.Exception
import Control.Monad
import Data.Text qualified as T
import Network.Socket qualified as Net
import Network.Socket.ByteString qualified as NetBS
import Network.URI

import Database.Bolt.Exception
import Database.Bolt.Transport

-------- Socket --------

newtype Connection = Connection {connSocket :: Net.Socket}

instance Transport Connection where
  send = NetBS.sendAll . connSocket
  sendMany = NetBS.sendMany . connSocket
  recv = NetBS.recv . connSocket
  close = Net.close . connSocket

data ConnInfo = ConnInfo
  { connHost :: !Net.HostName
  , connService :: !Net.ServiceName
  }
  deriving (Show, Read, Eq, Ord)

-------- Connections --------

-- Create ConnInfo from a host and service name
hostserv :: String -> String -> ConnInfo
hostserv = ConnInfo

-- Create ConnInfo from a bolt URI
uri :: String -> Maybe ConnInfo
uri uristr = do
  uri' <- parseAbsoluteURI uristr
  guard $ uriScheme uri' == "bolt"
  auth <- uriAuthority uri'
  guard $ uriUserInfo auth == ""
  guard $ uriPath uri' == ""
  guard $ uriQuery uri' == ""
  guard $ uriFragment uri' == ""
  return
    ConnInfo
      { connHost = uriRegName auth
      , connService = uriPort auth
      }

-- Connect using ConnInfo and return a handle
connect :: ConnInfo -> IO Connection
connect ConnInfo{..} = do
  let hints = Net.defaultHints{Net.addrSocketType = Net.Stream}
  addrs <- Net.getAddrInfo (Just hints) (Just connHost) (Just connService)
  connectAny addrs
 where
  connectAny [] = transportErr $ "Unable to connect to " <> T.pack connHost <> ":" <> T.pack connService
  connectAny (addr : addrs) = doConnect addr `catch` (\(_ :: IOException) -> connectAny addrs)

  doConnect addr = do
    sock <- Net.socket (Net.addrFamily addr) (Net.addrSocketType addr) (Net.addrProtocol addr)
    Net.connect sock (Net.addrAddress addr) `onException` Net.close sock
    return $ Connection sock
