{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (mapM, mapM_, sequence, sequence_, init)

import           Control.Monad (void)
import           Control.Monad.Except (ExceptT, runExceptT)
-- import           Data.Int
import           Database.Bolt.Protocol.Ver1
import           Database.Bolt.Protocol.Ver1.Types
import           Database.Bolt.Transport.Socket.Pipeline

-- -----------------------------------------------------------------------------
-- Applicative traversals

-- | We don't want the monadic 'mapM', because that doesn't do batching.
-- There doesn't seem to be a way to make 'Data.Traversable.mapM' have
-- the right behaviour when used with Haxl, so instead we define 'mapM'
-- to be 'traverse' in Haxl code.
mapM :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
mapM = traverse

forM :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
forM = flip mapM

-- | See 'mapM'.
mapM_ :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f ()
mapM_ f t = void $ traverse f t

forM_ :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f ()
forM_ = flip mapM_

-- | See 'mapM'.
sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence = sequenceA

-- | See 'mapM'.
sequence_ :: (Traversable t, Applicative f) => t (f a) -> f ()
sequence_ t = void $ sequenceA t

-- | See 'mapM'.
filterM :: (Applicative f) => (a -> f Bool) -> [a] -> f [a]
filterM predicate xs =
    filt <$> mapM predicate xs
  where
    filt bools = [ x | (x,True) <- zip xs bools ]

go :: Connection -> ExceptT (String, Metadata) Pipe ()
go conn = do
        init conn useragent (Basic "neo4j" "icecream")

main :: IO ()
main = do
    conn <- open (hostserv "bfly.blue" "7687") 1
    -- runExceptT $ do
        -- init conn useragent (Basic "neo4j" "icecream")
    -- r <- runPipe $ forM [1..10] $ \_ -> P.exec p "MATCH (n) RETURN COUNT (*)" mempty
    -- r <- runPipe $ forM [1..10] $ \i -> P.exec p "RETURN {i}" (object [ "i" #= (i :: Int64) ])
    -- print r
    undefined
