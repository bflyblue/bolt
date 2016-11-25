module Bolt.Protocol.Ver1.Types where

import qualified Data.HashMap.Strict as HM
import           Data.PackStream
import           Data.Text           (Text)

type UserAgent   = Text
type Principal   = Text
type Credentials = Text
type Statement   = Text

type Object      = HM.HashMap Text PackStream
type Parameters  = Object
type Metadata    = Object
type Record      = [PackStream]
