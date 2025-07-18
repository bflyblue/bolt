module Database.Bolt.Types (
  Object,
  object,
) where

import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.PackStream
import Data.Text (Text)

type Object = HM.HashMap Text PackStream

object :: (Eq k, Hashable k) => [(k, v)] -> HM.HashMap k v
object = HM.fromList
