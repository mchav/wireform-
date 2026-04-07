{-# LANGUAGE BangPatterns #-}
-- | BSON binary encoding.
--
-- Encodes a 'BSON.Value.Value' to its BSON wire format using direct
-- buffer writes via 'Proto.Encode.Direct.directEncode'. Integers are
-- little-endian, strings are UTF-8 with null terminators, and documents
-- include a leading 4-byte length prefix.
module BSON.Encode
  ( encode
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word32, Word64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castDoubleToWord64)

import Proto.Encode.Direct (directEncode)
import qualified BSON.Value as B

encode :: B.Value -> ByteString
encode val = case val of
  B.Document fields -> encodeDocument fields
  _ -> encodeDocument (V.singleton (T.pack "0", val))

encodeDocument :: V.Vector (T.Text, B.Value) -> ByteString
encodeDocument !fields =
  let !sz = docSize fields
  in directEncode sz (\p off -> writeDocument p off fields)
{-# NOINLINE encodeDocument #-}

docSize :: V.Vector (T.Text, B.Value) -> Int
docSize fields = 4 + V.foldl' (\acc (k, v) -> acc + elementSize k v) 0 fields + 1

elementSize :: T.Text -> B.Value -> Int
elementSize key val = 1 + cstringSize key + valuePayloadSize val

cstringSize :: T.Text -> Int
cstringSize t = BS.length (TE.encodeUtf8 t) + 1

valuePayloadSize :: B.Value -> Int
valuePayloadSize = \case
  B.Double _       -> 8
  B.String t       -> 4 + BS.length (TE.encodeUtf8 t) + 1
  B.Document fs    -> docSize fs
  B.Array vs       -> arrayDocSize vs
  B.Binary bs      -> 4 + 1 + BS.length bs
  B.Bool _         -> 1
  B.DateTime _     -> 8
  B.Null           -> 0
  B.Int32 _        -> 4
  B.Int64 _        -> 8
  B.ObjectId _     -> 12
  B.Regex p o      -> cstringSize p + cstringSize o

arrayDocSize :: V.Vector B.Value -> Int
arrayDocSize vs = 4 + V.ifoldl' (\acc i v -> acc + elementSize (T.pack (show i)) v) 0 vs + 1

writeDocument :: Ptr Word8 -> Int -> V.Vector (T.Text, B.Value) -> IO Int
writeDocument p off fields = do
  let !sz = docSize fields
  off1 <- writeLE32 p off (fromIntegral sz)
  off2 <- V.foldM' (\o (k, v) -> writeElement p o k v) off1 fields
  pokeByteOff p off2 (0x00 :: Word8)
  pure $! off2 + 1

writeElement :: Ptr Word8 -> Int -> T.Text -> B.Value -> IO Int
writeElement p off key val = do
  off1 <- pokeByteOff p off (typeTag val) >> pure (off + 1)
  off2 <- writeCString p off1 key
  writeValuePayload p off2 val

typeTag :: B.Value -> Word8
typeTag = \case
  B.Double _    -> 0x01
  B.String _    -> 0x02
  B.Document _  -> 0x03
  B.Array _     -> 0x04
  B.Binary _    -> 0x05
  B.Bool _      -> 0x08
  B.DateTime _  -> 0x09
  B.Null        -> 0x0A
  B.Int32 _     -> 0x10
  B.Int64 _     -> 0x12
  B.ObjectId _  -> 0x07
  B.Regex _ _   -> 0x0B

writeCString :: Ptr Word8 -> Int -> T.Text -> IO Int
writeCString p off t = do
  let !bs = TE.encodeUtf8 t
  off1 <- writeRawBytes p off bs
  pokeByteOff p off1 (0x00 :: Word8)
  pure $! off1 + 1

writeRawBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRawBytes !p !off (BSI.BS fp len) = do
  withForeignPtr fp $ \src ->
    copyBytes (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRawBytes #-}

writeLE32 :: Ptr Word8 -> Int -> Word32 -> IO Int
writeLE32 p off w = do
  pokeByteOff p off       (fromIntegral (w .&. 0xFF) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral ((w `shiftR` 24) .&. 0xFF) :: Word8)
  pure $! off + 4
{-# INLINE writeLE32 #-}

writeLE64 :: Ptr Word8 -> Int -> Word64 -> IO Int
writeLE64 p off w = do
  pokeByteOff p off       (fromIntegral (w .&. 0xFF) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral ((w `shiftR` 24) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 4) (fromIntegral ((w `shiftR` 32) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 5) (fromIntegral ((w `shiftR` 40) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 6) (fromIntegral ((w `shiftR` 48) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 7) (fromIntegral ((w `shiftR` 56) .&. 0xFF) :: Word8)
  pure $! off + 8
{-# INLINE writeLE64 #-}

writeValuePayload :: Ptr Word8 -> Int -> B.Value -> IO Int
writeValuePayload p off = \case
  B.Double d -> writeLE64 p off (castDoubleToWord64 d)
  B.String t -> do
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs + 1
    off1 <- writeLE32 p off (fromIntegral len)
    off2 <- writeRawBytes p off1 bs
    pokeByteOff p off2 (0x00 :: Word8)
    pure $! off2 + 1
  B.Document fields -> writeDocument p off fields
  B.Array vs -> do
    let arrayFields = V.imap (\i v -> (T.pack (show i), v)) vs
    writeDocument p off arrayFields
  B.Binary bs -> do
    let !len = BS.length bs
    off1 <- writeLE32 p off (fromIntegral len)
    pokeByteOff p off1 (0x00 :: Word8)
    writeRawBytes p (off1 + 1) bs
  B.Bool b -> do
    pokeByteOff p off (if b then 0x01 else 0x00 :: Word8)
    pure $! off + 1
  B.DateTime ms -> writeLE64 p off (fromIntegral ms)
  B.Null -> pure off
  B.Int32 n -> writeLE32 p off (fromIntegral n)
  B.Int64 n -> writeLE64 p off (fromIntegral n)
  B.ObjectId bs -> writeRawBytes p off (BS.take 12 (bs <> BS.replicate 12 0))
  B.Regex pat opts -> do
    off1 <- writeCString p off pat
    writeCString p off1 opts
