{-# LANGUAGE OverloadedStrings #-}

module Database.Bolt.Protocol.Ver1.Message
 ( Message(..)
 , AuthToken(..)
 )
where

import qualified Data.HashMap.Strict               as HM
import           Data.PackStream
import           Data.Text                         (Text)

import           Database.Bolt.Protocol.Ver1.Types

data Message = Init UserAgent AuthToken
             | AckFailure
             | Reset
             | Run Statement Parameters
             | DiscardAll
             | PullAll
             | Success Metadata
             | Ignored Metadata
             | Failure Metadata
             | Record Record
    deriving (Show)

instance ToPackStream Message where
    toPackStream (Init useragent authtoken) = Struct 0x01 [toPackStream useragent, toPackStream authtoken]
    toPackStream AckFailure                 = Struct 0x0e []
    toPackStream Reset                      = Struct 0x0f []
    toPackStream (Run stmt params)          = Struct 0x10 [toPackStream stmt, toPackStream params]
    toPackStream DiscardAll                 = Struct 0x2f []
    toPackStream PullAll                    = Struct 0x3f []
    toPackStream (Success metadata)         = Struct 0x70 [toPackStream metadata]
    toPackStream (Ignored metadata)         = Struct 0x71 [toPackStream metadata]
    toPackStream (Failure metadata)         = Struct 0x7e [toPackStream metadata]
    toPackStream (Record  record)           = Struct 0x7f record

instance FromPackStream Message where
    parsePackStream s = do
        struct <- parsePackStream s
        case struct of
            (Struct 0x01 [useragent, authtoken]) -> Init <$> parsePackStream useragent <*> parsePackStream authtoken
            (Struct 0x0e [])                     -> return AckFailure
            (Struct 0x0f [])                     -> return Reset
            (Struct 0x10 [stmt, params])         -> Run <$> parsePackStream stmt <*> parsePackStream params
            (Struct 0x2f [])                     -> return DiscardAll
            (Struct 0x3f [])                     -> return PullAll
            (Struct 0x70 [metadata])             -> Success <$> parsePackStream metadata
            (Struct 0x71 value)                  -> return $ Record  value
            (Struct 0x7e [metadata])             -> Ignored <$> parsePackStream metadata
            (Struct 0x7f [metadata])             -> Failure <$> parsePackStream metadata
            _                                    -> error "Invalid message"

data AuthToken = NoAuth
               | Basic Principal Credentials
    deriving (Show)

instance ToPackStream AuthToken where
    toPackStream NoAuth            = Map $ HM.fromList [ "scheme"      .= ("none"  :: Text) ]
    toPackStream (Basic user pass) = Map $ HM.fromList [ "scheme"      .= ("basic" :: Text)
                                                       , "principal"   .= user
                                                       , "credentials" .= pass
                                                       ]

instance FromPackStream AuthToken where
    parsePackStream (Map m) = do
        scheme <- m .:? "scheme" .!= "none"
        parseScheme scheme
      where
        parseScheme :: Text -> Parser AuthToken
        parseScheme "none"  = return NoAuth
        parseScheme "basic" = Basic <$> m .: "principal"
                                    <*> m .: "credentials"
        parseScheme _       = error "Unknown authentication scheme"

    parsePackStream _ = error "AuthToken should be a map"
