{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Database.Bolt.Transport.Class
    ( Transport(..)
    ) where

import           Data.ByteString.Lazy

-- Transport

class Monad m => Transport m t where
    message :: t -> ByteString -> m ByteString
    close   :: t -> m ()
