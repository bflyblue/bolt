module Database.Bolt.Protocol.Ver1.Types where

import           Data.Int
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.PackStream
import           Data.Text           (Text)

type UserAgent   = Text
type Principal   = Text
type Credentials = Text
type Statement   = Text
type Label       = Text
type Type        = Text
type Identity    = Int64

type Object      = HM.HashMap Text PackStream
type Parameters  = Object
type Properties  = Object
type Metadata    = Object
type Record      = [PackStream]

object :: (Eq k, Hashable k) => [(k, v)] -> HM.HashMap k v
object = HM.fromList

(#=) :: ToPackStream a => Text -> a -> (Text, PackStream)
k #= v = (k, toPackStream v)

(#:) :: FromPackStream a => Object -> Text -> Parser a
m #: k = maybe (parsefail "Expected Key missing in object") parsePackStream (HM.lookup k m)

(#:?) :: FromPackStream a => Object -> Text -> Parser (Maybe a)
m #:? k = maybe (return Nothing) (fmap Just . parsePackStream) (HM.lookup k m)
