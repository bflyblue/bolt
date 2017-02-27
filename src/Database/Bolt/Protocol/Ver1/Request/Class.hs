{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Bolt.Protocol.Ver1.Request.Class
    ( Request(..)
    ) where

import           Database.Bolt.Protocol.Ver1.Message
import           Database.Bolt.Protocol.Ver1.Response
import           Database.Bolt.Transport.Class

class Transport m t => Request m t where
    request :: t -> Message -> m Response
