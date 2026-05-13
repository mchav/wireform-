{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Avro.Decode (decodeAvro)
import Avro.Encode (encodeAvro)
import Avro.Schema (AvroField (..), AvroSchema (..), AvroType (..))
import Avro.Value qualified as AV
import BSON.Decode qualified as BD
import BSON.Encode qualified as BE
import BSON.Value qualified as BV
import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Value qualified as CV
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64)
import EDN.Decode qualified as ED
import EDN.Encode qualified as EE
import EDN.Value qualified as EV
import Ion.Decode qualified as ID
import Ion.Encode qualified as IE
import Ion.Value qualified as IV
import MsgPack.Decode qualified as MPD
import MsgPack.Encode qualified as MPE
import MsgPack.Value qualified as MV
import Proto.Decode (DecodeError, MessageDecode (..), decodeMessage)
import Proto.Encode (MessageEncode (..), MessageSize (..), encodeFieldDouble, encodeFieldString, encodeFieldVarint, encodeMessage)
import Proto.Internal.Wire (Tag (..))
import Proto.Internal.Wire.Decode (getDouble, getText, getVarint, skipField)
import Proto.Internal.Wire.Decode qualified as WD
import Proto.Internal.Wire.Encode (fieldDoubleSize, fieldTextSize, fieldVarintSize)
import System.CPUTime
import Text.Printf (printf)
import Thrift.Decode (decodeBinary, decodeCompact)
import Thrift.Encode (encodeBinary, encodeCompact)
import Thrift.Value qualified as TV
import Thrift.Wire ()


iterations :: Int
iterations = 100000


data BenchResult = BenchResult
  { brFormat :: !String
  , brEncodeNs :: !Integer
  , brDecodeNs :: !Integer
  , brSizeBytes :: !Int
  }


main :: IO ()
main = do
  putStrLn "All-Format Benchmark (100k iterations each)"
  putStrLn (replicate 68 '=')
  putStrLn ""

  results <-
    sequence
      [ benchProtobuf
      , benchAvro
      , benchThriftBinary
      , benchThriftCompact
      , benchMsgPack
      , benchCBOR
      , benchBSON
      , benchIon
      , benchEDN
      ]

  putStrLn ""
  printf "%-20s %12s %12s %12s\n" ("Format" :: String) ("Encode(ns)" :: String) ("Decode(ns)" :: String) ("Size(bytes)" :: String)
  putStrLn (replicate 68 '-')
  mapM_ printResult results


printResult :: BenchResult -> IO ()
printResult r =
  printf "%-20s %12d %12d %12d\n" (brFormat r) (brEncodeNs r) (brDecodeNs r) (brSizeBytes r)


nsPerIter :: Integer -> Integer
nsPerIter pico = pico `div` (fromIntegral iterations * 1000)


--------------------------------------------------------------------------------
-- Protobuf
--------------------------------------------------------------------------------

data PersonPB = PersonPB
  { pbName :: !T.Text
  , pbAge :: {-# UNPACK #-} !Word64
  , pbEmail :: !T.Text
  , pbScore :: {-# UNPACK #-} !Double
  }
  deriving stock (Show, Eq)


instance MessageEncode PersonPB where
  buildMessage msg =
    (if pbName msg /= "" then encodeFieldString 1 (pbName msg) else mempty)
      <> (if pbAge msg /= 0 then encodeFieldVarint 2 (pbAge msg) else mempty)
      <> (if pbEmail msg /= "" then encodeFieldString 3 (pbEmail msg) else mempty)
      <> (if pbScore msg /= 0 then encodeFieldDouble 4 (pbScore msg) else mempty)


instance MessageSize PersonPB where
  messageSize msg =
    (if pbName msg /= "" then fieldTextSize 1 (pbName msg) else 0)
      + (if pbAge msg /= 0 then fieldVarintSize 2 (pbAge msg) else 0)
      + (if pbEmail msg /= "" then fieldTextSize 3 (pbEmail msg) else 0)
      + (if pbScore msg /= 0 then fieldDoubleSize 4 else 0)


instance MessageDecode PersonPB where
  messageDecoder = loop "" 0 "" 0
    where
      loop !name !age !email !score = do
        mt <- WD.getTagOrU
        case mt of
          WD.UNothing -> pure (PersonPB name age email score)
          WD.UJust (Tag fn wt) -> case fn of
            1 -> getText >>= \v -> loop v age email score
            2 -> getVarint >>= \v -> loop name v email score
            3 -> getText >>= \v -> loop name age v score
            4 -> getDouble >>= \v -> loop name age email v
            _ -> skipField wt >> loop name age email score


personPB :: PersonPB
personPB = PersonPB "John Doe" 30 "john@example.com" 95.5


benchProtobuf :: IO BenchResult
benchProtobuf = do
  let encoded = encodeMessage personPB
      sz = BS.length encoded
  encT <- timeEncode iterations (\_ -> encodeMessage personPB)
  decT <- timeDecodeE iterations encoded (\e -> decodeMessage e :: Either DecodeError PersonPB)
  putStrLn $ "  Protobuf:       enc=" ++ show (nsPerIter encT) ++ "ns dec=" ++ show (nsPerIter decT) ++ "ns size=" ++ show sz
  pure $ BenchResult "Protobuf" (nsPerIter encT) (nsPerIter decT) sz


--------------------------------------------------------------------------------
-- Avro
--------------------------------------------------------------------------------

personAvroSchema :: AvroType
personAvroSchema =
  AvroRecord
    { avroRecordName = "Person"
    , avroRecordNamespace = Nothing
    , avroRecordDoc = Nothing
    , avroRecordAliases = V.empty
    , avroRecordFields =
        V.fromList
          [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "age" (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "email" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "score" (AvroPrimitive AvroDouble) Nothing Nothing V.empty Nothing Map.empty
          ]
    , avroRecordProps = Map.empty
    }


personAvro :: AV.Value
personAvro =
  AV.Record $
    V.fromList
      [ AV.String "John Doe"
      , AV.Int 30
      , AV.String "john@example.com"
      , AV.Double 95.5
      ]


benchAvro :: IO BenchResult
benchAvro = do
  let encoded = encodeAvro personAvroSchema personAvro
      sz = BS.length encoded
  encT <- timeEncode iterations (\_ -> encodeAvro personAvroSchema personAvro)
  decT <- timeDecode iterations encoded (decodeAvro personAvroSchema)
  putStrLn $ "  Avro:           enc=" ++ show (nsPerIter encT) ++ "ns dec=" ++ show (nsPerIter decT) ++ "ns size=" ++ show sz
  pure $ BenchResult "Avro" (nsPerIter encT) (nsPerIter decT) sz


--------------------------------------------------------------------------------
-- Thrift Binary
--------------------------------------------------------------------------------

personThrift :: TV.Value
personThrift =
  TV.Struct $
    V.fromList
      [ (1, TV.String "John Doe")
      , (2, TV.I32 30)
      , (3, TV.String "john@example.com")
      , (4, TV.Double 95.5)
      ]


benchThriftBinary :: IO BenchResult
benchThriftBinary = do
  let encoded = encodeBinary personThrift
      sz = BS.length encoded
  encT <- timeEncode iterations (\_ -> encodeBinary personThrift)
  decT <- timeDecode iterations encoded decodeBinary
  putStrLn $ "  Thrift Binary:  enc=" ++ show (nsPerIter encT) ++ "ns dec=" ++ show (nsPerIter decT) ++ "ns size=" ++ show sz
  pure $ BenchResult "Thrift Binary" (nsPerIter encT) (nsPerIter decT) sz


--------------------------------------------------------------------------------
-- Thrift Compact
--------------------------------------------------------------------------------

benchThriftCompact :: IO BenchResult
benchThriftCompact = do
  let encoded = encodeCompact personThrift
      sz = BS.length encoded
  encT <- timeEncode iterations (\_ -> encodeCompact personThrift)
  decT <- timeDecode iterations encoded decodeCompact
  putStrLn $ "  Thrift Compact: enc=" ++ show (nsPerIter encT) ++ "ns dec=" ++ show (nsPerIter decT) ++ "ns size=" ++ show sz
  pure $ BenchResult "Thrift Compact" (nsPerIter encT) (nsPerIter decT) sz


--------------------------------------------------------------------------------
-- MsgPack
--------------------------------------------------------------------------------

personMsgPack :: MV.Value
personMsgPack =
  MV.Map $
    V.fromList
      [ (MV.String "name", MV.String "John Doe")
      , (MV.String "age", MV.Int 30)
      , (MV.String "email", MV.String "john@example.com")
      , (MV.String "score", MV.Double 95.5)
      ]


benchMsgPack :: IO BenchResult
benchMsgPack = do
  let encoded = MPE.encode personMsgPack
      sz = BS.length encoded
  encT <- timeEncode iterations (\_ -> MPE.encode personMsgPack)
  decT <- timeDecode iterations encoded MPD.decode
  putStrLn $ "  MsgPack:        enc=" ++ show (nsPerIter encT) ++ "ns dec=" ++ show (nsPerIter decT) ++ "ns size=" ++ show sz
  pure $ BenchResult "MsgPack" (nsPerIter encT) (nsPerIter decT) sz


--------------------------------------------------------------------------------
-- CBOR
--------------------------------------------------------------------------------

personCBOR :: CV.Value
personCBOR =
  CV.Map $
    V.fromList
      [ (CV.TextString "name", CV.TextString "John Doe")
      , (CV.TextString "age", CV.UInt 30)
      , (CV.TextString "email", CV.TextString "john@example.com")
      , (CV.TextString "score", CV.Float64 95.5)
      ]


benchCBOR :: IO BenchResult
benchCBOR = do
  let encoded = CE.encode personCBOR
      sz = BS.length encoded
  encT <- timeEncode iterations (\_ -> CE.encode personCBOR)
  decT <- timeDecode iterations encoded CD.decode
  putStrLn $ "  CBOR:           enc=" ++ show (nsPerIter encT) ++ "ns dec=" ++ show (nsPerIter decT) ++ "ns size=" ++ show sz
  pure $ BenchResult "CBOR" (nsPerIter encT) (nsPerIter decT) sz


--------------------------------------------------------------------------------
-- BSON
--------------------------------------------------------------------------------

personBSON :: BV.Value
personBSON =
  BV.Document $
    V.fromList
      [ ("name", BV.String "John Doe")
      , ("age", BV.Int32 30)
      , ("email", BV.String "john@example.com")
      , ("score", BV.Double 95.5)
      ]


benchBSON :: IO BenchResult
benchBSON = do
  let encoded = BE.encode personBSON
      sz = BS.length encoded
  encT <- timeEncode iterations (\_ -> BE.encode personBSON)
  decT <- timeDecode iterations encoded BD.decode
  putStrLn $ "  BSON:           enc=" ++ show (nsPerIter encT) ++ "ns dec=" ++ show (nsPerIter decT) ++ "ns size=" ++ show sz
  pure $ BenchResult "BSON" (nsPerIter encT) (nsPerIter decT) sz


--------------------------------------------------------------------------------
-- Ion
--------------------------------------------------------------------------------

personIon :: IV.Value
personIon =
  IV.Struct $
    V.fromList
      [ ("name", IV.String "John Doe")
      , ("age", IV.Int 30)
      , ("email", IV.String "john@example.com")
      , ("score", IV.Float 95.5)
      ]


benchIon :: IO BenchResult
benchIon = do
  let encoded = IE.encode personIon
      sz = BS.length encoded
  encT <- timeEncode iterations (\_ -> IE.encode personIon)
  decT <- timeDecode iterations encoded ID.decode
  putStrLn $ "  Ion:            enc=" ++ show (nsPerIter encT) ++ "ns dec=" ++ show (nsPerIter decT) ++ "ns size=" ++ show sz
  pure $ BenchResult "Ion" (nsPerIter encT) (nsPerIter decT) sz


--------------------------------------------------------------------------------
-- EDN
--------------------------------------------------------------------------------

personEDN :: EV.Value
personEDN =
  EV.Map $
    V.fromList
      [ (EV.Keyword Nothing "name", EV.String "John Doe")
      , (EV.Keyword Nothing "age", EV.Integer 30)
      , (EV.Keyword Nothing "email", EV.String "john@example.com")
      , (EV.Keyword Nothing "score", EV.Float 95.5)
      ]


benchEDN :: IO BenchResult
benchEDN = do
  let encoded = EE.encodeBS personEDN
      sz = BS.length encoded
  encT <- timeEncode iterations (\_ -> EE.encodeBS personEDN)
  decT <- timeDecode iterations encoded ED.decodeBS
  putStrLn $ "  EDN:            enc=" ++ show (nsPerIter encT) ++ "ns dec=" ++ show (nsPerIter decT) ++ "ns size=" ++ show sz
  pure $ BenchResult "EDN" (nsPerIter encT) (nsPerIter decT) sz


--------------------------------------------------------------------------------
-- Timing helpers
--------------------------------------------------------------------------------

timeEncode :: Int -> (Int -> BS.ByteString) -> IO Integer
timeEncode n f = do
  t1 <- getCPUTime
  let !_ = goEnc n 0
  t2 <- getCPUTime
  pure (t2 - t1)
  where
    goEnc 0 !acc = acc
    goEnc !i !acc = goEnc (i - 1) (acc + BS.length (f i))


timeDecode :: Int -> BS.ByteString -> (BS.ByteString -> Either String a) -> IO Integer
timeDecode n enc f = do
  t1 <- getCPUTime
  let !_ = goDec n (0 :: Int)
  t2 <- getCPUTime
  pure (t2 - t1)
  where
    goDec 0 !acc = acc
    goDec !i !acc = case f enc of
      Right _ -> goDec (i - 1) (acc + 1)
      Left _ -> goDec (i - 1) acc


timeDecodeE :: Int -> BS.ByteString -> (BS.ByteString -> Either e a) -> IO Integer
timeDecodeE n enc f = do
  t1 <- getCPUTime
  let !_ = goDec n (0 :: Int)
  t2 <- getCPUTime
  pure (t2 - t1)
  where
    goDec 0 !acc = acc
    goDec !i !acc = case f enc of
      Right _ -> goDec (i - 1) (acc + 1)
      Left _ -> goDec (i - 1) acc
