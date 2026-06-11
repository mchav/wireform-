{-# LANGUAGE BangPatterns #-}

-- | Apache Arrow IPC file and stream readers.
module Arrow.File (
  ArrowFile (..),
  ArrowStream (..),
  readArrowFile,
  readArrowFileColumns,
  readArrowStream,
  readIPCMessage,
) where

import Arrow.Column (ColumnArray, materializeRecordBatch)
import Arrow.IPC (decodeIPCMessage)
import Arrow.Types
import Data.Bits (complement, shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int64)
import Data.Vector qualified as V
import Data.Word (Word32, Word64)


data ArrowFile = ArrowFile
  { afSchema :: !Schema
  , afBatches :: !(V.Vector (RecordBatchDef, ByteString))
  }
  deriving stock (Show, Eq)


data ArrowStream = ArrowStream
  { asSchema :: !Schema
  , asBatches :: !(V.Vector (RecordBatchDef, ByteString))
  }
  deriving stock (Show, Eq)


arrowMagic :: ByteString
arrowMagic = "ARROW1"


{- | Read a single IPC message at the given byte offset.
Returns (message, body, next_offset).
-}
readIPCMessage :: ByteString -> Int -> Either String (Message, ByteString, Int)
readIPCMessage bs !off = do
  ensureBytes bs off 8
  let !cont = getLE32 bs off
  if cont /= 0xFFFFFFFF
    then Left "Arrow.File: missing continuation marker"
    else do
      let !metaLen = fromIntegral (getLE32 bs (off + 4)) :: Int
      if metaLen == 0
        then Left "Arrow.File: end of stream (EOS marker)"
        else do
          -- metaLen as stored already includes padding from the encoder
          ensureBytes bs (off + 8) metaLen
          let !msgSlice = BS.take (8 + metaLen) (BS.drop off bs)
          msg <- decodeIPCMessage msgSlice
          let !rawMeta = BS.take metaLen (BS.drop (off + 8) bs)
              !bodyLen = extractBodyLength rawMeta
              !bodyStart = off + 8 + metaLen
              !paddedBody = alignUp8 (fromIntegral bodyLen)
          if bodyLen > 0
            then do
              ensureBytes bs bodyStart paddedBody
              let !bodyBs = BS.take (fromIntegral bodyLen) (BS.drop bodyStart bs)
              Right (msg, bodyBs, bodyStart + paddedBody)
            else Right (msg, BS.empty, off + 8 + metaLen)


-- | Check for EOS marker (continuation 0xFFFFFFFF + metadata length 0) at offset.
isEOS :: ByteString -> Int -> Bool
isEOS bs off =
  off + 8 <= BS.length bs
    && getLE32 bs off == 0xFFFFFFFF
    && getLE32 bs (off + 4) == 0


-- | Read an Arrow IPC stream from a ByteString.
readArrowStream :: ByteString -> Either String ArrowStream
readArrowStream bs = do
  (msg0, _, off0) <- readIPCMessage bs 0
  schema <- case msg0 of
    SchemaMessage s -> Right s
    _ -> Left "Arrow.File: first stream message is not a schema"
  batches <- readStreamBatches bs off0
  Right ArrowStream {asSchema = schema, asBatches = V.fromList batches}


readStreamBatches :: ByteString -> Int -> Either String [(RecordBatchDef, ByteString)]
readStreamBatches bs !off
  | off >= BS.length bs = Right []
  | isEOS bs off = Right []
  | otherwise = do
      (msg, body, off') <- readIPCMessage bs off
      case msg of
        RecordBatch rb -> do
          rest <- readStreamBatches bs off'
          Right ((rb, body) : rest)
        DictionaryBatch -> readStreamBatches bs off'
        _ -> Left "Arrow.File: unexpected message type in stream"


-- | Read an Arrow IPC file from a ByteString.
readArrowFile :: ByteString -> Either String ArrowFile
readArrowFile bs = do
  let !len = BS.length bs
  if len < 18
    then Left "Arrow.File: file too small"
    else do
      if BS.take 6 bs /= arrowMagic
        then Left "Arrow.File: missing ARROW1 header magic"
        else
          if BS.take 6 (BS.drop (len - 6) bs) /= arrowMagic
            then Left "Arrow.File: missing ARROW1 footer magic"
            else do
              let !footerLen = fromIntegral (getLE32 bs (len - 10)) :: Int
              if footerLen <= 0 || len - 10 - footerLen < 8
                then Left "Arrow.File: invalid footer length"
                else do
                  let !footerOff = len - 10 - footerLen
                      !footerBs = BS.take footerLen (BS.drop footerOff bs)
                  (schema, offsets) <- decodeFooter footerBs
                  batches <-
                    mapM
                      ( \bOff -> do
                          let !msgOff = fromIntegral bOff :: Int
                          (msg, body, _) <- readIPCMessage bs msgOff
                          case msg of
                            RecordBatch rb -> Right (rb, body)
                            _ -> Left "Arrow.File: block is not a RecordBatch"
                      )
                      offsets
                  Right ArrowFile {afSchema = schema, afBatches = V.fromList batches}


-- | Read an Arrow IPC file and materialize all record batches into columns.
readArrowFileColumns :: ByteString -> Either String (Schema, V.Vector (V.Vector ColumnArray))
readArrowFileColumns bs = do
  af <- readArrowFile bs
  let schema = afSchema af
  cols <- V.mapM (\(rb, body) -> materializeRecordBatch schema rb body) (afBatches af)
  Right (schema, cols)


-- * Footer


decodeFooter :: ByteString -> Either String (Schema, [Int64])
decodeFooter bs = do
  ensureBytes bs 0 4
  let !schemaLen = fromIntegral (getLE32 bs 0) :: Int
  ensureBytes bs 4 schemaLen
  let !schemaBs = BS.take schemaLen (BS.drop 4 bs)
  msg <- decodeIPCMessage schemaBs
  schema <- case msg of
    SchemaMessage s -> Right s
    _ -> Left "Arrow.File: footer does not contain schema"
  let !off = 4 + schemaLen
  ensureBytes bs off 4
  let !nBlocks = fromIntegral (getLE32 bs off) :: Int
      !off1 = off + 4
  ensureBytes bs off1 (nBlocks * 8)
  let offsets =
        map
          ( \i ->
              fromIntegral (getLE64 bs (off1 + i * 8)) :: Int64
          )
          [0 .. nBlocks - 1]
  Right (schema, offsets)


{- | Extract body length from raw metadata bytes.
Format: version(2) | headerType(1) | headerLen(4) | header(n) | bodyLength(8)
-}
extractBodyLength :: ByteString -> Int64
extractBodyLength bs
  | BS.length bs < 7 = 0
  | otherwise =
      let !headerLen = fromIntegral (getLE32 bs 3) :: Int
          !off = 7 + headerLen
      in if off + 8 > BS.length bs
           then 0
           else fromIntegral (getLE64 bs off)


-- * Primitives


alignUp8 :: Int -> Int
alignUp8 n = (n + 7) .&. complement 7


ensureBytes :: ByteString -> Int -> Int -> Either String ()
ensureBytes bs off n
  | off + n > BS.length bs = Left "Arrow.File: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensureBytes #-}


getLE32 :: ByteString -> Int -> Word32
getLE32 bs off =
  let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word32
      !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word32
      !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word32
      !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)


getLE64 :: ByteString -> Int -> Word64
getLE64 bs off =
  let rd i = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word64
  in rd 0
       .|. (rd 1 `shiftL` 8)
       .|. (rd 2 `shiftL` 16)
       .|. (rd 3 `shiftL` 24)
       .|. (rd 4 `shiftL` 32)
       .|. (rd 5 `shiftL` 40)
       .|. (rd 6 `shiftL` 48)
       .|. (rd 7 `shiftL` 56)
