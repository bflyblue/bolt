{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Protocol.Ver1.Pretty
 ( pretty
 , structName
 ) where

import           Data.PackStream (PackStream, genericStructName, prettyStruct)
import           Data.Text
import           Data.Word

pretty :: PackStream -> Text
pretty = prettyStruct structName

structName :: Word8 -> Text
structName 0x01 = "InitMessage"
structName 0x0e = "AckFailureMessage"
structName 0x0f = "ResetMessage"
structName 0x10 = "RunMessage"
structName 0x2f = "DiscardAllMessage"
structName 0x3f = "PullAllMessage"
structName 0x43 = "Node"
structName 0x50 = "Path"
structName 0x52 = "Relationship"
structName 0x70 = "SuccessMessage"
structName 0x71 = "RecordMessage"
structName 0x72 = "UnboundedRelationship"
structName 0x7e = "IgnoredMessage"
structName 0x7f = "FailureMessage"
structName n    = genericStructName n
