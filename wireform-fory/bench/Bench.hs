{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | criterion micro-benchmark for the wireform-fory encoder /
-- decoder. Mirrors the payload set in @bench/bench.py@ so the
-- two implementations can be compared head-to-head.
--
-- The bench has /two/ matching pipelines for each shape:
--
-- * @encode\/decode@ uses the dynamic 'Fory.Value.Value' AST.
-- * @encode-typed\/decode-typed@ uses 'Fory.Direct', no
--   'Value' allocation in the inner loop.
module Main (main) where

import Control.DeepSeq (NFData)
import Criterion.Main
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Text as T

import qualified Fory.Decode as D
import qualified Fory.Direct as FD
import qualified Fory.Encode as E
import qualified Fory.Options as O
import qualified Fory.Struct as ST
import qualified Fory.TypeId as TI
import qualified Fory.Value as VV

-- ---------------------------------------------------------------------------
-- Payloads
-- ---------------------------------------------------------------------------

vInt :: VV.Value
vInt = VV.VarInt64Val 1234567890

vFloat :: VV.Value
vFloat = VV.Float64Val 3.141592653589793

vSmallStr :: VV.Value
vSmallStr = VV.StringVal (T.pack (replicate 12 'a'))

vLongStr :: VV.Value
vLongStr = VV.StringVal (T.pack (replicate 1024 'a'))

vBytes1k :: VV.Value
vBytes1k = VV.BinaryVal (BS.replicate 1024 0x42)

vListOfInt :: VV.Value
vListOfInt = VV.ListVal (V.fromList [VV.VarInt64Val (fromIntegral i)
                                     | i <- [0 .. 99 :: Int]])

vListOfString :: VV.Value
vListOfString = VV.ListVal
  (V.fromList [VV.StringVal (T.pack (replicate 8 'x'))
               | _ <- [1 .. 100 :: Int]])

vMapStrInt :: VV.Value
vMapStrInt =
  VV.MapVal
    (V.fromList
       [ (VV.StringVal (T.pack ("k" ++ show i)), VV.VarInt64Val (fromIntegral i))
       | i <- [0 .. 49 :: Int]
       ])

vInt32Array1k :: VV.Value
vInt32Array1k =
  VV.Int32ArrayVal
    (VS.fromList [fromIntegral i | i <- [0 .. 1023 :: Int]])

vFloat64Array1k :: VV.Value
vFloat64Array1k =
  VV.Float64ArrayVal
    (VS.fromList [fromIntegral i * 0.5 | i <- [0 .. 1023 :: Int]])

personSchema :: ST.StructSchema
personSchema = ST.mkSchema "example" "Person"
  [("name", TI.STRING), ("age", TI.VARINT64)]

personOpts :: O.EncodeOptions
personOpts = O.defaultEncodeOptions
  { O.eoStructRegistry = O.registerStruct personSchema
                           O.emptyStructRegistry }

personDopts :: O.DecodeOptions
personDopts = O.defaultDecodeOptions
  { O.doStructRegistry = O.registerStruct personSchema
                           O.emptyStructRegistry }

mkPerson :: T.Text -> Int -> VV.Value
mkPerson n a = VV.RegisteredStructVal "example" "Person"
  (V.fromList [("name", VV.StringVal n), ("age", VV.VarInt64Val (fromIntegral a))])

vPerson :: VV.Value
vPerson = mkPerson "alice" 30

vListOfPerson :: VV.Value
vListOfPerson =
  VV.ListVal (V.fromList
    [mkPerson (T.pack ("user" ++ show i)) i | i <- [0 .. 99 :: Int]])

-- ---------------------------------------------------------------------------
-- Benchmark group
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Typed payloads (Fory.Direct, no Value AST)
-- ---------------------------------------------------------------------------

tInt :: Int
tInt = 1234567890

tFloat :: Double
tFloat = 3.141592653589793

tSmallStr :: T.Text
tSmallStr = T.pack (replicate 12 'a')

tLongStr :: T.Text
tLongStr = T.pack (replicate 1024 'a')

tBytes1k :: BS.ByteString
tBytes1k = BS.replicate 1024 0x42

tListOfInt :: [Int]
tListOfInt = [0 .. 99]

tVecOfInt :: V.Vector Int
tVecOfInt = V.fromList [0 .. 99]

tStorableInt64s :: VS.Vector Int64
tStorableInt64s =
  VS.fromList [fromIntegral i | i <- [0 .. 99 :: Int]]

tListOfString :: [T.Text]
tListOfString = [T.pack (replicate 8 'x') | _ <- [1 .. 100 :: Int]]

tVecOfString :: V.Vector T.Text
tVecOfString = V.fromList tListOfString

tMapStrInt :: M.Map T.Text Int
tMapStrInt =
  M.fromList [(T.pack ("k" ++ show i), i) | i <- [0 .. 49 :: Int]]

tInt32Array1k :: VS.Vector Int32
tInt32Array1k =
  VS.fromList [fromIntegral i | i <- [0 .. 1023 :: Int]]

tFloat64Array1k :: VS.Vector Double
tFloat64Array1k =
  VS.fromList [fromIntegral i * 0.5 | i <- [0 .. 1023 :: Int]]

main :: IO ()
main = defaultMain
  [ bgroup "encode"
      [ bench "int"               $ nf E.encode vInt
      , bench "float"             $ nf E.encode vFloat
      , bench "small string"      $ nf E.encode vSmallStr
      , bench "long string 1k"    $ nf E.encode vLongStr
      , bench "bytes 1k"          $ nf E.encode vBytes1k
      , bench "list-of-int 100"   $ nf E.encode vListOfInt
      , bench "list-of-string 100" $ nf E.encode vListOfString
      , bench "map str/int 50"    $ nf E.encode vMapStrInt
      , bench "int32-array 1k"    $ nf E.encode vInt32Array1k
      , bench "float64-array 1k"  $ nf E.encode vFloat64Array1k
      , bench "struct Person"     $ nf (E.encodeWith personOpts) vPerson
      , bench "list-of-struct 100" $ nf (E.encodeWith personOpts) vListOfPerson
      ]
  , bgroup "decode"
      [ benchDecode "int"               vInt
      , benchDecode "float"             vFloat
      , benchDecode "small string"      vSmallStr
      , benchDecode "long string 1k"    vLongStr
      , benchDecode "bytes 1k"          vBytes1k
      , benchDecode "list-of-int 100"   vListOfInt
      , benchDecode "list-of-string 100" vListOfString
      , benchDecode "map str/int 50"    vMapStrInt
      , benchDecode "int32-array 1k"    vInt32Array1k
      , benchDecode "float64-array 1k"  vFloat64Array1k
      , benchDecodeStruct "struct Person" vPerson
      , benchDecodeStruct "list-of-struct 100" vListOfPerson
      ]

    -- Typed pipelines (Fory.Direct, no Value AST)
  , bgroup "encode-typed"
      [ bench "int"               $ nf FD.encodeDirect tInt
      , bench "float"             $ nf FD.encodeDirect tFloat
      , bench "small string"      $ nf FD.encodeDirect tSmallStr
      , bench "long string 1k"    $ nf FD.encodeDirect tLongStr
      , bench "bytes 1k"          $ nf FD.encodeDirect tBytes1k
      , bench "list-of-int 100"   $ nf FD.encodeDirect tListOfInt
      , bench "vec-of-int 100"    $ nf FD.encodeDirect tVecOfInt
      , bench "vecS-of-int64 100" $ nf FD.encodeDirect tStorableInt64s
      , bench "list-of-string 100" $ nf FD.encodeDirect tListOfString
      , bench "vec-of-string 100"  $ nf FD.encodeDirect tVecOfString
      , bench "map str/int 50"    $ nf FD.encodeDirect tMapStrInt
      , bench "int32-array 1k"    $ nf FD.encodeDirect tInt32Array1k
      , bench "float64-array 1k"  $ nf FD.encodeDirect tFloat64Array1k
      ]
  , bgroup "decode-typed"
      [ benchDecodeT "int"               tInt
      , benchDecodeT "float"             tFloat
      , benchDecodeT "small string"      tSmallStr
      , benchDecodeT "long string 1k"    tLongStr
      , benchDecodeT "bytes 1k"          tBytes1k
      , benchDecodeT "list-of-int 100"   tListOfInt
      , benchDecodeT "vec-of-int 100"    tVecOfInt
      , benchDecodeT "vecS-of-int64 100" tStorableInt64s
      , benchDecodeT "list-of-string 100" tListOfString
      , benchDecodeT "vec-of-string 100"  tVecOfString
      , benchDecodeT "map str/int 50"    tMapStrInt
      , benchDecodeT "int32-array 1k"    tInt32Array1k
      , benchDecodeT "float64-array 1k"  tFloat64Array1k
      ]
  ]
  where
    benchDecode :: String -> VV.Value -> Benchmark
    benchDecode name v =
      let !bytes = E.encode v
      in bench name $ nf D.decode bytes

    benchDecodeStruct :: String -> VV.Value -> Benchmark
    benchDecodeStruct name v =
      let !bytes = E.encodeWith personOpts v
      in bench name $ nf (D.decodeWith personDopts) bytes

    benchDecodeT
      :: forall a.
         (FD.EncodeDirect a, FD.DecodeDirect a, NFData a)
      => String -> a -> Benchmark
    benchDecodeT name x =
      let !bytes = FD.encodeDirect x
      in bench name $ nf (FD.decodeDirect @a) bytes
