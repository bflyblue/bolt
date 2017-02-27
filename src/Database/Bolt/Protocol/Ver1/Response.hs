module Database.Bolt.Protocol.Ver1.Response
    ( Response(..)
    ) where

import           Database.Bolt.Protocol.Ver1.Types

data Response = Success Metadata [Record]
              | Failed Metadata
              | Ignored Metadata
    deriving (Eq, Show)
