module Database.Bolt.Exception (
  internalErr,
  transportErr,
  protocolErr,
  authFail,
  reqFail,
  reqIgnore,
) where

import Control.Exception
import Data.Text (Text)
import Database.Bolt.Types

data BoltException
  = AuthenticationFailure Text
  | RequestFailure Object Text
  | RequestIgnored Object Text
  | InternalError Text
  | TransportError Text
  | ProtocolError Text
  deriving (Show)

instance Exception BoltException

authFail :: Text -> IO a
authFail = throwIO . AuthenticationFailure

reqFail :: Object -> Text -> IO a
reqFail meta = throwIO . RequestFailure meta

reqIgnore :: Object -> Text -> IO a
reqIgnore meta = throwIO . RequestIgnored meta

internalErr :: Text -> IO a
internalErr = throwIO . InternalError

transportErr :: Text -> IO a
transportErr = throwIO . TransportError

protocolErr :: Text -> IO a
protocolErr = throwIO . ProtocolError
