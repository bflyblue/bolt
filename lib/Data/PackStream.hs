{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.PackStream (
  PackStream (..),
  ToPackStream (..),
  FromPackStream (..),
  Parser,
  parse,
  parsefail,
  parseEither,
  parseMaybe,
  pack,
  unpack,
  pretty,
  prettyStruct,
  genericStructName,
  (.=),
  (.:),
  (.:?),
  (.!=),
)
where

import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.Int
import Data.Map.Strict qualified as M
import Data.Scientific
import Data.Serialize.Get
import Data.Serialize.IEEE754
import Data.Serialize.Put
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Data.Vector.Instances ()
import Data.Word
import GHC.Generics (Generic)
import Text.Printf

data PackStream
  = Null
  | Bool !Bool
  | Int !Int64
  | Float !Double
  | Bytes !BS.ByteString
  | String !Text
  | List !(V.Vector PackStream)
  | Dict !(HM.HashMap PackStream PackStream)
  | Struct !Word8 !(V.Vector PackStream)
  deriving (Show, Eq, Generic, Hashable)

type Parser = Either String

parse :: (a -> Parser b) -> a -> Parser b
parse = parseEither

parseEither :: (a -> Parser b) -> a -> Either String b
parseEither f = f

parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe f = either (const Nothing) Just . parseEither f

parsefail :: a -> Either a b
parsefail = Left

class FromPackStream a where
  parsePackStream :: PackStream -> Parser a

instance FromPackStream PackStream where
  parsePackStream = return

instance FromPackStream Bool where
  parsePackStream (Bool b) = return b
  parsePackStream _ = parsefail "Expecting Bool"

instance FromPackStream Int64 where
  parsePackStream (Int i) = return i
  parsePackStream _ = parsefail "Expecting Int"

instance FromPackStream Double where
  parsePackStream (Float d) = return d
  parsePackStream _ = parsefail "Expecting Float"

instance FromPackStream Text where
  parsePackStream (String t) = return t
  parsePackStream _ = parsefail "Expecting String"

instance (FromPackStream a) => FromPackStream [a] where
  parsePackStream (List l) = mapM parsePackStream (V.toList l)
  parsePackStream _ = parsefail "Expecting List"

instance (FromPackStream a) => FromPackStream (V.Vector a) where
  parsePackStream (List l) = mapM parsePackStream l
  parsePackStream _ = parsefail "Expecting List"

instance (Eq a, Hashable a, FromPackStream a, FromPackStream b) => FromPackStream (HM.HashMap a b) where
  parsePackStream (Dict m) = do
    kvs <- mapM parseAssoc (HM.toList m)
    return $ HM.fromList kvs
   where
    parseAssoc (k, v) = (,) <$> parsePackStream k <*> parsePackStream v
  parsePackStream _ = parsefail "Expecting Dict"

instance (Ord a, FromPackStream a, FromPackStream b) => FromPackStream (M.Map a b) where
  parsePackStream (Dict m) = do
    kvs <- mapM parseAssoc (HM.toList m)
    return $ M.fromList kvs
   where
    parseAssoc (k, v) = (,) <$> parsePackStream k <*> parsePackStream v
  parsePackStream _ = parsefail "Expecting Dict"

instance FromPackStream Scientific where
  parsePackStream (Int i) = return $ fromIntegral i
  parsePackStream (Float f) = return $ fromFloatDigits f
  parsePackStream _ = parsefail "Expecting Int or Float"

class ToPackStream a where
  toPackStream :: a -> PackStream

instance ToPackStream PackStream where
  toPackStream = id

instance ToPackStream Bool where
  toPackStream = Bool

instance ToPackStream Int64 where
  toPackStream = Int

instance ToPackStream Double where
  toPackStream = Float

instance ToPackStream Text where
  toPackStream = String

instance (ToPackStream a) => ToPackStream [a] where
  toPackStream = List . V.fromList . fmap toPackStream

instance (ToPackStream a) => ToPackStream (V.Vector a) where
  toPackStream = List . fmap toPackStream

instance (ToPackStream a, ToPackStream b) => ToPackStream (M.Map a b) where
  toPackStream = Dict . HM.fromList . fmap (bimap toPackStream toPackStream) . M.toList

instance (ToPackStream a, ToPackStream b) => ToPackStream (HM.HashMap a b) where
  toPackStream = Dict . HM.fromList . fmap (bimap toPackStream toPackStream) . HM.toList

instance ToPackStream Scientific where
  toPackStream s =
    case Data.Scientific.floatingOrInteger s of
      Left rf -> Float rf
      Right i -> Int i

pack :: (ToPackStream a) => Putter a
pack = putPackStream . toPackStream

unpack :: (FromPackStream a) => Get (Parser a)
unpack = parsePackStream <$> getPackStream

getPackStream :: Get PackStream
getPackStream = do
  marker <- getWord8
  if
    | marker == 0xc0 -> return Null
    | marker == 0xc1 -> Float <$> getFloat64be
    | marker == 0xc2 -> return $ Bool False
    | marker == 0xc3 -> return $ Bool True
    | marker < 0x80 -> return $ Int (fromIntegral marker)
    | marker >= 0xf0 -> return $ Int (fromIntegral marker - 0x100)
    | marker == 0xc8 -> Int . fromIntegral <$> getInt8
    | marker == 0xc9 -> Int . fromIntegral <$> getInt16be
    | marker == 0xca -> Int . fromIntegral <$> getInt32be
    | marker == 0xcb -> Int . fromIntegral <$> getInt64be
    | marker == 0xcc -> fromIntegral <$> getWord8 >>= getBytes'
    | marker == 0xcd -> fromIntegral <$> getWord16be >>= getBytes'
    | marker == 0xce -> fromIntegral <$> getWord32be >>= getBytes'
    | 0x80 <= marker && marker < 0x90 ->
        getString (fromIntegral marker .&. 0x0f)
    | marker == 0xd0 -> fromIntegral <$> getWord8 >>= getString
    | marker == 0xd1 -> fromIntegral <$> getWord16be >>= getString
    | marker == 0xd2 -> fromIntegral <$> getWord32be >>= getString
    | 0x90 <= marker && marker < 0xa0 ->
        getList (fromIntegral marker .&. 0x0f)
    | marker == 0xd4 -> fromIntegral <$> getWord8 >>= getList
    | marker == 0xd5 -> fromIntegral <$> getWord16be >>= getList
    | marker == 0xd6 -> fromIntegral <$> getWord32be >>= getList
    | 0xa0 <= marker && marker < 0xb0 ->
        getDict (fromIntegral marker .&. 0x0f)
    | marker == 0xd8 -> fromIntegral <$> getWord8 >>= getDict
    | marker == 0xd9 -> fromIntegral <$> getWord16be >>= getDict
    | marker == 0xda -> fromIntegral <$> getWord32be >>= getDict
    | 0xb0 <= marker && marker < 0xc0 ->
        getStruct (fromIntegral marker .&. 0x0f)
    | marker == 0xdc -> fromIntegral <$> getWord8 >>= getStruct
    | marker == 0xdd -> fromIntegral <$> getWord16be >>= getStruct
    | otherwise -> fail $ "Unknown marker " ++ printf "0x%02x" marker

getBytes' :: Int -> Get PackStream
getBytes' n = Bytes . BS.pack <$> replicateM n getWord8

getString :: Int -> Get PackStream
getString n = String . T.decodeUtf8 <$> getByteString n

getList :: Int -> Get PackStream
getList n = List <$> V.replicateM n getPackStream

getDict :: Int -> Get PackStream
getDict n = Dict . HM.fromList <$> replicateM n getPair
 where
  getPair = (,) <$> getPackStream <*> getPackStream

getStruct :: Int -> Get PackStream
getStruct n = Struct <$> getWord8 <*> V.replicateM n getPackStream

putPackStream :: Putter PackStream
putPackStream Null = putWord8 0xc0
putPackStream (Float d) = putWord8 0xc1 >> putFloat64be d
putPackStream (Bool False) = putWord8 0xc2
putPackStream (Bool True) = putWord8 0xc3
putPackStream (Int i)
  | -0x10 <= i && i < 0x80 = putInt8 (fromIntegral i)
  | -0x80 <= i && i < 0x80 = putWord8 0xc8 >> putInt8 (fromIntegral i)
  | -0x8000 <= i && i < 0x8000 = putWord8 0xc9 >> putInt16be (fromIntegral i)
  | -0x80000000 <= i && i < 0x80000000 = putWord8 0xca >> putInt32be (fromIntegral i)
  | otherwise = putWord8 0xcb >> putInt64be (fromIntegral i)
putPackStream (Bytes bstr) = do
  let size = BS.length bstr
  if
    | size < 0x10 -> putWord8 (0x80 + fromIntegral size)
    | size < 0x100 -> putWord8 0xd0 >> putWord8 (fromIntegral size)
    | size < 0x10000 -> putWord8 0xd1 >> putWord16be (fromIntegral size)
    | size < 0x100000000 -> putWord8 0xd2 >> putWord32be (fromIntegral size)
    | otherwise -> error "Bytes too long"
  putByteString bstr
putPackStream (String t) = do
  let bstr = T.encodeUtf8 t
      size = BS.length bstr
  if
    | size < 0x10 -> putWord8 (0x80 + fromIntegral size)
    | size < 0x100 -> putWord8 0xd0 >> putWord8 (fromIntegral size)
    | size < 0x10000 -> putWord8 0xd1 >> putWord16be (fromIntegral size)
    | size < 0x100000000 -> putWord8 0xd2 >> putWord32be (fromIntegral size)
    | otherwise -> error "String too long"
  putByteString bstr
putPackStream (List xs) = do
  let size = V.length xs
  if
    | size < 0x10 -> putWord8 (0x90 + fromIntegral size)
    | size < 0x100 -> putWord8 0xd4 >> putWord8 (fromIntegral size)
    | size < 0x10000 -> putWord8 0xd5 >> putWord16be (fromIntegral size)
    | size < 0x100000000 -> putWord8 0xd6 >> putWord32be (fromIntegral size)
    | otherwise -> error "List too long"
  mapM_ putPackStream xs
putPackStream (Dict m) = do
  let size = HM.size m
  if
    | size < 0x10 -> putWord8 (0xa0 + fromIntegral size)
    | size < 0x100 -> putWord8 0xd8 >> putWord8 (fromIntegral size)
    | size < 0x10000 -> putWord8 0xd9 >> putWord16be (fromIntegral size)
    | size < 0x100000000 -> putWord8 0xda >> putWord32be (fromIntegral size)
    | otherwise -> error "Map too large"
  mapM_ (uncurry putPair) (HM.toList m)
 where
  putPair k v = putPackStream k >> putPackStream v
putPackStream (Struct sig fs) = do
  let size = length fs
  if
    | size < 0x10 -> putWord8 (0xb0 + fromIntegral size)
    | size < 0x100 -> putWord8 0xdc >> putWord8 (fromIntegral size)
    | size < 0x10000 -> putWord8 0xdd >> putWord16be (fromIntegral size)
    | otherwise -> error "Structure too big"
  putWord8 sig
  mapM_ putPackStream fs

pretty :: PackStream -> Text
pretty = prettyStruct genericStructName

prettyStruct :: (Word8 -> Text) -> PackStream -> Text
prettyStruct _ Null = "null"
prettyStruct _ (Bool True) = "true"
prettyStruct _ (Bool False) = "false"
prettyStruct _ (Int i) = T.pack (show i)
prettyStruct _ (Float d) = T.pack (show d)
prettyStruct _ (Bytes bstr) = "b\"" <> T.pack (show (BS.length bstr)) <> "\""
prettyStruct _ (String t) = "\"" <> t <> "\""
prettyStruct _ (List xs) = "[" <> T.intercalate ", " (pretty <$> V.toList xs) <> "]"
prettyStruct _ (Dict ps) = "{" <> T.intercalate ", " (fmap (\(k, v) -> pretty k <> ": " <> pretty v) (HM.toList ps)) <> "}"
prettyStruct sn (Struct s fs) = sn s <> "{" <> T.intercalate ", " (pretty <$> V.toList fs) <> "}"

genericStructName :: Word8 -> Text
genericStructName n = "Struct(signature=" <> T.pack (printf "0x%02x" n) <> ")"

infixr 8 .=
(.=) :: (ToPackStream a) => Text -> a -> (PackStream, PackStream)
k .= v = (String k, toPackStream v)

infixl 9 .:
(.:) :: (FromPackStream a) => HM.HashMap PackStream PackStream -> Text -> Parser a
m .: k = maybe (parsefail "Expected Key missing in map") parsePackStream (HM.lookup (String k) m)

infixl 9 .:?
(.:?) :: (FromPackStream a) => HM.HashMap PackStream PackStream -> Text -> Parser (Maybe a)
m .:? k = maybe (return Nothing) (fmap Just . parsePackStream) (HM.lookup (String k) m)

infixl 9 .!=
(.!=) :: Parser (Maybe a) -> a -> Parser a
p .!= d = do
  ma <- p
  maybe (return d) return ma
