module Database.Bolt.Protocol.Ver1.Types (
  Object,
  object,
  UserAgent,
  Principal,
  Credentials,
  Statement,
  Label,
  Type,
  Identity,
  Parameters,
  Properties,
  Metadata,
  Record,
  (#=),
  (#:),
  (#:?),
) where

import Data.HashMap.Strict qualified as HM
import Data.Int
import Data.PackStream
import Data.Text (Text)
import Database.Bolt.Types (Object, object)

type UserAgent = Text
type Principal = Text
type Credentials = Text
type Statement = Text
type Label = Text
type Type = Text
type Identity = Int64

type Parameters = Object
type Properties = Object
type Metadata = Object
type Record = [PackStream]

(#=) :: (ToPackStream a) => Text -> a -> (Text, PackStream)
k #= v = (k, toPackStream v)

(#:) :: (FromPackStream a) => Object -> Text -> Parser a
m #: k = maybe (parsefail "Expected Key missing in object") parsePackStream (HM.lookup k m)

(#:?) :: (FromPackStream a) => Object -> Text -> Parser (Maybe a)
m #:? k = maybe (return Nothing) (fmap Just . parsePackStream) (HM.lookup k m)
