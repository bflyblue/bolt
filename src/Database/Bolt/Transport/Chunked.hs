{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Transport.Chunked
    ( send
    , recv
    , put
    , get
    ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import           Data.Monoid
import           Data.Serialize.Get
import           Data.Serialize.Put
import qualified Data.Text               as T

import           Database.Bolt.Exception
import           Database.Bolt.Transport (Transport)
import qualified Database.Bolt.Transport as Trans

-------- Chunked I/O --------

send :: Transport t => t -> LBS.ByteString -> IO ()
send conn lbs = Trans.put conn $ mapM_ putChunks (LBS.toChunks lbs) >> done
  where
    maxChunk = 0xffff

    done = putWord16be 0

    putChunks bs = do
        let sz = BS.length bs
        if | sz == 0       -> return ()
           | sz > maxChunk -> split bs
           | otherwise     -> sendChunk bs

    split bs = do
        let (bs1, bs2) = BS.splitAt maxChunk bs
        sendChunk bs1
        putChunks bs2

    sendChunk bs = do
        putWord16be $ fromIntegral $ BS.length bs
        putByteString bs

recv :: Transport t => t -> IO LBS.ByteString
recv conn = LBS.fromChunks <$> getChunks
  where
    getChunks = do
        size <- Trans.get conn 2 getWord16be
        case size of
            0   -> return []
            csz -> do bs  <- getChunk csz
                      bs' <- getChunks
                      return (bs ++ bs')

    getChunk n = do
        bs <- Trans.recv conn (fromIntegral n)
        let sz = fromIntegral $ BS.length bs
        if | sz == 0   -> transportErr "Unexpected end of stream"
           | sz < n    -> do bs' <- getChunk (n - sz)
                             return (bs : bs')
           | otherwise -> return [bs]

put :: Transport t => t -> Put -> IO ()
put conn = send conn . runPutLazy

get :: Transport t => t -> Get a -> IO a
get conn g = do
    lbs <- recv conn
    case runGetLazy g lbs of
        Left err -> transportErr $ "Unexpected data received: " <> T.pack err
        Right a  -> return a
