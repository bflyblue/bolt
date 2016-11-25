module Bolt.Exception
    ( internalErr
    , transportErr
    , protocolErr
    , authFail
    , reqFail
    , reqIgnore
    ) where

import           Control.Exception
import           Data.Text         (Text)

data BoltException = AuthenticationFailure Text
                   | RequestFailure Text
                   | RequestIgnored Text
                   | InternalError Text
                   | TransportError Text
                   | ProtocolError Text
    deriving (Show)

instance Exception BoltException

authFail :: Text -> IO a
authFail = throwIO . AuthenticationFailure

reqFail :: Text -> IO a
reqFail = throwIO . RequestFailure

reqIgnore :: Text -> IO a
reqIgnore = throwIO . RequestIgnored

internalErr :: Text -> IO a
internalErr = throwIO . InternalError

transportErr :: Text -> IO a
transportErr = throwIO . TransportError

protocolErr :: Text -> IO a
protocolErr = throwIO . ProtocolError
