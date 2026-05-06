{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | criterion micro-benchmark for the wireform-fory encoder /
-- decoder. Mirrors the payload set in @bench/bench.py@ so the
-- two implementations can be compared head-to-head:
--
-- * @int@                 — a single 'VarInt64Val'
-- * @float@               — a single 'Float64Val'
-- * @small string@        — 12-char ASCII
-- * @long string@         — 1024-char ASCII
-- * @bytes 1k@            — 1024 random bytes
-- * @list-of-int@         — homogeneous list of 100 'VarInt64Val'
-- * @list-of-string@      — homogeneous list of 100 small strings
-- * @map-of-string-int@   — 50-entry map @str -> int@
-- * @int32-array 1k@      — primitive int32 array of 1024 elements
-- * @float64-array 1k@    — primitive float64 array of 1024 elements
-- * @struct@              — registered 'Person' with hash header
-- * @list-of-struct@      — 100 'Person' records in a same-type
--                           list (exercises the once-only ns+tn
--                           element-type path)
module Main (main) where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Text as T

import qualified Fory.Decode as D
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
