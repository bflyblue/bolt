module Database.Bolt.Protocol.Ver1.Types where

import           Data.Int
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
