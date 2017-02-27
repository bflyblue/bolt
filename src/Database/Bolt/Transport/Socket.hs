{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Database.Bolt.Transport.Socket
    ( ConnInfo
    , hostserv
    , uri

    , Connection
    , open
    , open'

    , sendMessage
    , recvMessage

    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text                         as T
import qualified Network.Socket                    as Net
import qualified Network.Socket.ByteString         as NetBS
import           Network.URI

import           Database.Bolt.Exception
import           Database.Bolt.Transport.Class
import           Database.Bolt.Transport.Handshake

import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import           Data.Serialize.Get
import           Data.Serialize.Put

-------- Socket --------

newtype Connection = Connection { connSocket :: Net.Socket }

instance MonadIO m => Transport m Connection where
    message conn msg = sendMessage conn msg >> recvMessage conn
    close   conn     = liftIO $ Net.close $ connSocket conn

-------- Connections --------

-- Describes a database we want to connect to
-- NOTE: user and password are part of the protocol and not the transport layer
data ConnInfo = ConnInfo
    { connHost    :: Net.HostName
    , connService :: Net.ServiceName
    } deriving (Show, Read, Eq, Ord)

-- Create ConnInfo from a host and service name
hostserv :: String -> String -> ConnInfo
hostserv = ConnInfo

-- Create ConnInfo from a bolt URI
uri :: String -> Maybe ConnInfo
uri uristr = do
    uri' <- parseAbsoluteURI uristr
    guard $ uriScheme    uri' == "bolt"
    auth <- uriAuthority uri'
    guard $ uriUserInfo  auth == ""
    guard $ uriPath      uri' == ""
    guard $ uriQuery     uri' == ""
    guard $ uriFragment  uri' == ""
    return ConnInfo { connHost = uriRegName auth
                    , connService = uriPort auth }

-- Connect using ConnInfo and return a Connection handle
connect :: MonadIO m => ConnInfo -> m Connection
connect ConnInfo{..} = liftIO $ do
    let hints = Net.defaultHints { Net.addrSocketType = Net.Stream }
    addrs <- Net.getAddrInfo (Just hints) (Just connHost) (Just connService)
    connectAny addrs
  where
    connectAny []           = transportErr $ "Unable to connect to " <> T.pack connHost <> ":" <> T.pack connService
    connectAny (addr:addrs) = doConnect addr `catch` (\ (_ :: IOException) -> connectAny addrs)

    doConnect addr = do
        sock <- Net.socket (Net.addrFamily addr) (Net.addrSocketType addr) (Net.addrProtocol addr)
        Net.connect sock (Net.addrAddress addr) `onException` Net.close sock
        return $ Connection sock

-- Connect and perform protocol negotiation
open' :: MonadIO m => ConnInfo -> [BoltProtocol] -> m (Connection, Maybe BoltProtocol)
open' conninfo protos = do
    conn <- connect conninfo
    put conn (gogobolt >> offerProtocols offer)
    agreed <- get conn 4 agreedProtocol
    return (conn, supported agreed)
  where
    offer = let (p1:p2:p3:p4:_) = protos ++ repeat noProto
            in  OfferProtocols p1 p2 p3 p4
    supported p | p == noProto = Nothing
                | otherwise    = Just p

open :: MonadIO m => ConnInfo -> BoltProtocol -> m (Maybe Connection)
open conninfo proto = do
    (conn, proto') <- open' conninfo [proto]
    if Just proto == proto'
    then
        return $ Just conn
    else do
        close conn
        return Nothing

-------- Message I/O --------

sendMessage :: MonadIO m => Connection -> LBS.ByteString -> m ()
sendMessage conn msg = liftIO $ put conn chunked
  where
    chunked = chunks >> done

    chunks = mapM_ putChunks (LBS.toChunks msg)

    done = putWord16be 0

    putChunks bs = do
        let sz = BS.length bs
        if | sz == 0       -> return ()
           | sz > maxChunk -> split bs
           | otherwise     -> putChunk bs

    maxChunk = 0xffff

    split bs = do
        let (bs1, bs2) = BS.splitAt maxChunk bs
        putChunk bs1
        putChunks bs2

    putChunk bs = do
        putWord16be $ fromIntegral $ BS.length bs
        putByteString bs

recvMessage :: MonadIO m => Connection -> m LBS.ByteString
recvMessage conn = liftIO $ LBS.fromChunks <$> getChunks
  where
    getChunks = do
        size <- get conn 2 getWord16be
        case size of
            0   -> return []
            csz -> do bs  <- getChunk csz
                      bs' <- getChunks
                      return (bs ++ bs')

    getChunk n = do
        bs <- NetBS.recv (connSocket conn) (fromIntegral n)
        let sz = fromIntegral $ BS.length bs
        if | sz == 0   -> transportErr "Unexpected end of stream"
           | sz < n    -> do bs' <- getChunk (n - sz)
                             return (bs : bs')
           | otherwise -> return [bs]

put :: MonadIO m => Connection -> Put -> m ()
put conn p = liftIO $ NetBS.sendAll (connSocket conn) (runPut p)

get :: MonadIO m => Connection -> Int -> Get a -> m a
get conn n g = liftIO $ do
    bs <- NetBS.recv (connSocket conn) n
    case runGet g bs of
        Left err -> transportErr $ "Unexpected data received: " <> T.pack err
        Right a  -> return a
