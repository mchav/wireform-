{-# LANGUAGE BangPatterns #-}
-- | BSON binary decoding.
--
-- Decodes a BSON wire-format 'ByteString' into a 'BSON.Value.Value'.
-- Uses unsafe indexing for performance on validated input. Supports
-- all standard BSON element types.
module BSON.Decode
  ( decode
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Data.Word (Word8, Word32, Word64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Float (castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified BSON.Value as B

decode :: ByteString -> Either String B.Value
decode !bs
  | BS.length bs < 5 = Left "BSON.Decode: input too short"
  | otherwise = case decodeDocument bs 0 of
      Left err -> Left err
      Right (val, _) -> Right val

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

withBSPtrOff :: ByteString -> Int -> (Ptr Word8 -> IO a) -> a
withBSPtrOff (BSI.BS fp _) off f = unsafeDupablePerformIO $
  withForeignPtr fp $ \p -> f (castPtr p `plusPtr` off)
{-# INLINE withBSPtrOff #-}

readLE32 :: ByteString -> Int -> Word32
readLE32 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE32 #-}

readLE64 :: ByteString -> Int -> Word64
readLE64 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE64 #-}

ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off + n > BS.length bs = Left "BSON.Decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}

readCString :: ByteString -> Int -> Either String (Text, Int)
readCString bs off = go off
  where
    !bsLen = BS.length bs
    go !i
      | i >= bsLen = Left "BSON.Decode: unterminated cstring"
      | rdByte bs i == 0x00 =
          let !raw = BSU.unsafeTake (i - off) (BSU.unsafeDrop off bs)
          in case TE.decodeUtf8' raw of
               Left _  -> Left "BSON.Decode: invalid UTF-8 in cstring"
               Right t -> Right (t, i + 1)
      | otherwise = go (i + 1)

readBSONString :: ByteString -> Int -> Either String (Text, Int)
readBSONString bs off = do
  ensure bs off 4
  let !len = fromIntegral (readLE32 bs off) :: Int
  ensure bs (off + 4) len
  let !raw = BSU.unsafeTake (len - 1) (BSU.unsafeDrop (off + 4) bs)
  case TE.decodeUtf8' raw of
    Left _  -> Left "BSON.Decode: invalid UTF-8 in string"
    Right t -> Right (t, off + 4 + len)

decodeDocument :: ByteString -> Int -> Either String (B.Value, Int)
decodeDocument bs off = do
  ensure bs off 4
  let !docSz = fromIntegral (readLE32 bs off) :: Int
      !docEnd = off + docSz
  ensure bs off docSz
  if rdByte bs (docEnd - 1) /= 0x00
    then Left "BSON.Decode: document missing terminator"
    else do
      (elems, _) <- readElements bs (off + 4) (docEnd - 1)
      Right (B.Document (V.fromList elems), docEnd)

readElements :: ByteString -> Int -> Int -> Either String ([(Text, B.Value)], Int)
readElements bs off end
  | off >= end = Right ([], off)
  | otherwise = do
      ensure bs off 1
      let !tag = rdByte bs off
      (key, off1) <- readCString bs (off + 1)
      (val, off2) <- readValue bs off1 tag
      (rest, off3) <- readElements bs off2 end
      Right ((key, val) : rest, off3)

readValue :: ByteString -> Int -> Word8 -> Either String (B.Value, Int)
readValue bs off tag = case tag of
  0x01 -> do
    ensure bs off 8
    let !w = readLE64 bs off
    Right (B.Double (castWord64ToDouble w), off + 8)
  0x02 -> do
    (t, off1) <- readBSONString bs off
    Right (B.String t, off1)
  0x03 -> decodeDocument bs off
  0x04 -> do
    result <- decodeDocument bs off
    case result of
      (B.Document fields, off1) -> Right (B.Array (V.map snd fields), off1)
      (_, _off1) -> Left "BSON.Decode: expected document for array"
  0x05 -> do
    ensure bs off 5
    let !len = fromIntegral (readLE32 bs off) :: Int
        !off1 = off + 4 + 1
    ensure bs off1 len
    let !dat = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
    Right (B.Binary dat, off1 + len)
  0x07 -> do
    ensure bs off 12
    let !dat = BSU.unsafeTake 12 (BSU.unsafeDrop off bs)
    Right (B.ObjectId dat, off + 12)
  0x08 -> do
    ensure bs off 1
    let !b = rdByte bs off /= 0x00
    Right (B.Bool b, off + 1)
  0x09 -> do
    ensure bs off 8
    let !ms = fromIntegral (readLE64 bs off) :: Int64
    Right (B.DateTime ms, off + 8)
  0x0A -> Right (B.Null, off)
  0x0B -> do
    (pat, off1) <- readCString bs off
    (opts, off2) <- readCString bs off1
    Right (B.Regex pat opts, off2)
  0x0E -> do
    (t, off1) <- readBSONString bs off
    Right (B.String t, off1)
  0x10 -> do
    ensure bs off 4
    let !n = fromIntegral (readLE32 bs off) :: Int32
    Right (B.Int32 n, off + 4)
  0x12 -> do
    ensure bs off 8
    let !n = fromIntegral (readLE64 bs off) :: Int64
    Right (B.Int64 n, off + 8)
  _ -> Left $ "BSON.Decode: unknown type tag: " ++ show tag
