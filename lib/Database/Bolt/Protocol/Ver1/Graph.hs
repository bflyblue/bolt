{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Protocol.Ver1.Graph (
  Node (..),
  Relationship (..),
  Path (..),
  UnboundedRelationship (..),
)
where

import Data.Int
import Data.PackStream

import Database.Bolt.Protocol.Ver1.Types

data Node = Node
  { nodeIdentity :: !Identity
  , nodeLabels :: ![Label]
  , nodeProperties :: !Properties
  }
  deriving (Show, Eq)

data Relationship = Relationship
  { relIdentity :: !Identity
  , startNodeIdentity :: !Identity
  , endNodeIdentity :: !Identity
  , relType :: !Type
  , relProperties :: !Properties
  }
  deriving (Show, Eq)

data Path = Path
  { pathNodes :: ![Node]
  , pathRelationships :: ![UnboundedRelationship]
  , pathSequence :: ![Int64]
  }
  deriving (Show, Eq)

data UnboundedRelationship = UnboundedRelationship
  { urelIdentity :: !Identity
  , urelType :: !Type
  , urelProperties :: !Properties
  }
  deriving (Show, Eq)

instance ToPackStream Node where
  toPackStream (Node nodeid labels props) = Struct 0x4e [toPackStream nodeid, toPackStream labels, toPackStream props]

instance FromPackStream Node where
  parsePackStream s = do
    struct <- parsePackStream s
    case struct of
      (Struct 0x4e [nodeid, labels, props]) -> Node <$> parsePackStream nodeid <*> parsePackStream labels <*> parsePackStream props
      _other -> error "Invalid Node"

instance ToPackStream Relationship where
  toPackStream (Relationship relid start end ty props) =
    Struct
      0x52
      [ toPackStream relid
      , toPackStream start
      , toPackStream end
      , toPackStream ty
      , toPackStream props
      ]

instance FromPackStream Relationship where
  parsePackStream s = do
    struct <- parsePackStream s
    case struct of
      (Struct 0x52 [relid, start, end, ty, props]) ->
        Relationship
          <$> parsePackStream relid
          <*> parsePackStream start
          <*> parsePackStream end
          <*> parsePackStream ty
          <*> parsePackStream props
      _other -> error "Invalid Relationship"

instance ToPackStream Path where
  toPackStream (Path nodes rels seqn) = Struct 0x50 [toPackStream nodes, toPackStream rels, toPackStream seqn]

instance FromPackStream Path where
  parsePackStream s = do
    struct <- parsePackStream s
    case struct of
      (Struct 0x50 [nodes, rels, seqn]) -> Path <$> parsePackStream nodes <*> parsePackStream rels <*> parsePackStream seqn
      _other -> error "Invalid Path"

instance ToPackStream UnboundedRelationship where
  toPackStream (UnboundedRelationship relid ty props) = Struct 0x72 [toPackStream relid, toPackStream ty, toPackStream props]

instance FromPackStream UnboundedRelationship where
  parsePackStream s = do
    struct <- parsePackStream s
    case struct of
      (Struct 0x72 [relid, ty, props]) -> UnboundedRelationship <$> parsePackStream relid <*> parsePackStream ty <*> parsePackStream props
      _other -> error "Invalid UnboundedRelationship"
