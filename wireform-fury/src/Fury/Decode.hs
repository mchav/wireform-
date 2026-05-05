{-# LANGUAGE BangPatterns #-}
-- | Apache Fory xlang value decoder.
--
-- Mirrors 'Fury.Encode.encode'. See that module's haddock for the
-- exact subset of the spec we round-trip through.
module Fury.Decode
  ( decode
  , decodeValue
  ) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Fury.Encoding as E
import qualified Fury.MetaString as MS
import qualified Fury.TypeId as T
import qualified Fury.Value as VV

-- | Parse a fory-encoded byte string back to a 'Value'.
decode :: ByteString -> Either String VV.Value
decode !bs = do
  (hdr, off1) <- E.readForyHeader bs 0
  if hdr .&. 0x01 /= 0
    then if off1 == BS.length bs
           then Right VV.NoneVal
           else Left "Fury.Decode.decode: trailing bytes after null header"
    else if hdr .&. 0x02 == 0
      then Left ("Fury.Decode.decode: missing xlang flag in header byte "
                 ++ show hdr)
      else do
        (flag, off2) <- E.readByte bs off1
        case flag of
          f | f == E.refFlagNull ->
                if off2 == BS.length bs
                  then Right VV.NoneVal
                  else Left "Fury.Decode.decode: trailing bytes after NULL flag"
            | f == E.refFlagNotNullValue -> do
                (val, off3) <- decodeValue bs off2
                if off3 == BS.length bs
                  then Right val
                  else Left $ "Fury.Decode.decode: " ++ show (BS.length bs - off3)
                              ++ " trailing bytes"
            | otherwise ->
                Left $ "Fury.Decode.decode: unsupported ref flag "
                       ++ show flag
                       ++ " (only NULL/NOT_NULL_VALUE are emitted by Fury.Encode)"

-- | Read a single value (type tag + payload) at the given offset.
-- Returns the value and the next offset.
decodeValue :: ByteString -> Int -> Either String (VV.Value, Int)
decodeValue bs off = do
  (tagW, off1) <- E.readByte bs off
  let !tag = T.TypeId tagW
  case tag of
    T.NONE     -> Right (VV.NoneVal, off1)
    T.BOOL     -> do
      (b, off2) <- E.readByte bs off1
      Right (VV.BoolVal (b /= 0), off2)
    T.INT8     -> do
      (b, off2) <- E.readByte bs off1
      Right (VV.Int8Val (fromIntegral b), off2)
    T.INT16    -> do
      (n, off2) <- E.readInt16LE bs off1
      Right (VV.Int16Val n, off2)
    T.INT32    -> do
      (n, off2) <- E.readInt32LE bs off1
      Right (VV.Int32Val n, off2)
    T.INT64    -> do
      (n, off2) <- E.readInt64LE bs off1
      Right (VV.Int64Val n, off2)
    T.UINT8    -> do
      (b, off2) <- E.readByte bs off1
      Right (VV.Uint8Val b, off2)
    T.UINT16   -> do
      (n, off2) <- E.readWord16LE bs off1
      Right (VV.Uint16Val n, off2)
    T.UINT32   -> do
      (n, off2) <- E.readWord32LE bs off1
      Right (VV.Uint32Val n, off2)
    T.UINT64   -> do
      (n, off2) <- E.readWord64LE bs off1
      Right (VV.Uint64Val n, off2)
    T.FLOAT32  -> do
      (f, off2) <- E.readFloat32LE bs off1
      Right (VV.Float32Val f, off2)
    T.FLOAT64  -> do
      (d, off2) <- E.readFloat64LE bs off1
      Right (VV.Float64Val d, off2)
    T.STRING   -> do
      (t, off2) <- E.readUtf8String bs off1
      Right (VV.StringVal t, off2)
    T.BINARY   -> do
      (n,   off2) <- E.readVaruint32 bs off1
      (raw, off3) <- E.readBytes (fromIntegral n) bs off2
      Right (VV.BinaryVal raw, off3)
    T.LIST     -> readCollection VV.ListVal bs off1
    T.SET      -> readCollection VV.SetVal  bs off1
    T.MAP      -> readMap bs off1
    T.NAMED_STRUCT -> do
      (ns,     off2) <- MS.readMetaString bs off1
      (typeNm, off3) <- MS.readMetaString bs off2
      (fields, off4) <- readStructFields bs off3
      Right (VV.StructVal ns typeNm fields, off4)
    _ -> Left $ "Fury.Decode.decodeValue: unsupported type tag " ++ show tagW

readCollection
  :: (V.Vector VV.Value -> VV.Value)
  -> ByteString
  -> Int
  -> Either String (VV.Value, Int)
readCollection con bs off = do
  (n, off1) <- E.readVaruint32 bs off
  loop (fromIntegral n) [] off1
  where
    loop :: Int -> [VV.Value] -> Int -> Either String (VV.Value, Int)
    loop 0 acc o = Right (con (V.fromListN (length acc) (reverse acc)), o)
    loop k acc o = do
      (v, o') <- decodeValue bs o
      loop (k - 1) (v : acc) o'

readMap :: ByteString -> Int -> Either String (VV.Value, Int)
readMap bs off = do
  (n, off1) <- E.readVaruint32 bs off
  loop (fromIntegral n) [] off1
  where
    loop :: Int -> [(VV.Value, VV.Value)] -> Int -> Either String (VV.Value, Int)
    loop 0 acc o = Right (VV.MapVal (V.fromListN (length acc) (reverse acc)), o)
    loop k acc o = do
      (kv, o1) <- decodeValue bs o
      (vv, o2) <- decodeValue bs o1
      loop (k - 1) ((kv, vv) : acc) o2

readStructFields :: ByteString -> Int -> Either String (VV.StructFields, Int)
readStructFields bs off = do
  (n, off1) <- E.readVaruint32 bs off
  loop (fromIntegral n :: Int) [] off1
  where
    loop :: Int -> [(T.Text, VV.Value)] -> Int -> Either String (VV.StructFields, Int)
    loop 0 acc o = Right (V.fromListN (length acc) (reverse acc), o)
    loop k acc o = do
      (name, o1) <- MS.readMetaString bs o
      (val,  o2) <- decodeValue bs o1
      loop (k - 1) ((name, val) : acc) o2
