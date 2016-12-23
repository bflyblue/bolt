module Main where

import           Data.ByteString     (ByteString)
import           Data.PackStream
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as Text
import qualified Data.Vector         as Vec
import           Data.Serialize.Get
import           Data.Serialize.Put

import           Test.Tasty
import           Test.Tasty.QuickCheck

instance Arbitrary PackStream where
    arbitrary =
      oneof [ return Null
            , Bool   <$> arbitrary
            , Int    <$> arbitrary
            , Float  <$> arbitrary
            , String <$> (Text.pack    <$> arbitrary)
            , List   <$> (Vec.fromList <$> scale' (listOf arbitrary))
            , Map    <$> (HM.fromList  <$> scale' (listOf arbitrary))
            , Struct <$> arbitrary     <*> scale' (listOf arbitrary)
            ]
      where
         scale' = scale (`div` 2)

    shrink Null          = []
    shrink (Bool _)      = [Null]
    shrink (Int _)       = [Null]
    shrink (Float _)     = [Null]
    shrink (String s)    = Null : ((String . Text.pack)  <$> shrink (Text.unpack s))
    shrink (List xs)
        | Vec.length xs == 0 = [Null]
        | Vec.length xs == 1 = [Vec.head xs]
        | otherwise          = Null : ((List . Vec.fromList) <$> shrink (Vec.toList xs))
    shrink (Map xs)
        | HM.size xs == 0 = [Null]
        | HM.size xs == 1 = let [(k, v)] = HM.toList xs in [k, v]
        | otherwise       = Null : ((Map . HM.fromList)   <$> shrink (HM.toList xs))
    shrink (Struct s xs) = Null : List (Vec.fromList xs) : (Struct s <$> shrink xs)

prop_decode_encode :: PackStream -> Property
prop_decode_encode a = (decode . encode) a === a
  where
    encode :: PackStream -> ByteString
    encode = runPut . pack
    decode :: ByteString -> PackStream
    decode x = case runGet unpack x of
                 Left err -> error err
                 Right (Left err) -> error err
                 Right (Right ps) -> ps


main :: IO ()
main = defaultMain $
  testGroup "Tests"
    [ testGroup "(checked by QuickCheck)"
        [ testProperty "id == decode . encode" prop_decode_encode
        ]
    ]
