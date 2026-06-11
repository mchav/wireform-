{-# LANGUAGE BangPatterns #-}

{- | Thrift RPC message header encoding\/decoding for Binary and Compact protocols.

A Thrift RPC message wraps a struct payload with a header containing the
method name, message type (call\/reply\/exception\/oneway), and a sequence ID.
-}
module Thrift.Message (
  ThriftMessageType (..),
  ThriftMessage (..),
  encodeMessageBinary,
  decodeMessageBinary,
  encodeMessageCompact,
  decodeMessageCompact,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import Thrift.Decode (decodeBinary, decodeCompact)
import Thrift.Encode (encodeBinary, encodeCompact)
import Thrift.Value qualified as TV
import Wireform.Builder qualified as B


-- | Thrift RPC message types.
data ThriftMessageType
  = TMsgCall
  | TMsgReply
  | TMsgException
  | TMsgOneway
  deriving stock (Show, Eq, Ord, Enum, Bounded)


-- | A complete Thrift RPC message: header + payload.
data ThriftMessage = ThriftMessage
  { tmsgName :: !Text
  , tmsgType :: !ThriftMessageType
  , tmsgSeqId :: {-# UNPACK #-} !Int32
  , tmsgPayload :: !TV.Value
  }
  deriving stock (Show, Eq)


msgTypeToWord8 :: ThriftMessageType -> Word8
msgTypeToWord8 TMsgCall = 1
msgTypeToWord8 TMsgReply = 2
msgTypeToWord8 TMsgException = 3
msgTypeToWord8 TMsgOneway = 4
{-# INLINE msgTypeToWord8 #-}


msgTypeFromWord8 :: Word8 -> Maybe ThriftMessageType
msgTypeFromWord8 1 = Just TMsgCall
msgTypeFromWord8 2 = Just TMsgReply
msgTypeFromWord8 3 = Just TMsgException
msgTypeFromWord8 4 = Just TMsgOneway
msgTypeFromWord8 _ = Nothing
{-# INLINE msgTypeFromWord8 #-}


--------------------------------------------------------------------------------
-- Binary Protocol
--------------------------------------------------------------------------------

binaryVersion :: Word32
binaryVersion = 0x80010000


encodeMessageBinary :: ThriftMessage -> ByteString
encodeMessageBinary (ThriftMessage name mtype seqid payload) =
  BL.toStrict $
    B.toLazyByteString $
      let !nameBytes = TE.encodeUtf8 name
          !version = binaryVersion .|. fromIntegral (msgTypeToWord8 mtype)
      in putBE32 version
           <> putBE32 (fromIntegral (BS.length nameBytes))
           <> B.byteString nameBytes
           <> putBE32i (fromIntegral seqid)
           <> B.byteString (encodeBinary payload)


decodeMessageBinary :: ByteString -> Either String ThriftMessage
decodeMessageBinary !bs
  | BS.length bs < 4 = Left "decodeMessageBinary: insufficient data"
  | otherwise =
      let !w = getBE32 bs 0
      in if w .&. 0x80000000 /= 0
           then decodeStrictBinary bs w
           else decodeOldBinary bs


decodeStrictBinary :: ByteString -> Word32 -> Either String ThriftMessage
decodeStrictBinary !bs !versionWord =
  let !mtypeByte = fromIntegral (versionWord .&. 0xFF) :: Word8
  in case msgTypeFromWord8 mtypeByte of
       Nothing -> Left $ "decodeMessageBinary: invalid message type: " ++ show mtypeByte
       Just !mtype -> do
         (nameBytes, off1) <- getStr bs 4
         case TE.decodeUtf8' nameBytes of
           Left _ -> Left "decodeMessageBinary: invalid UTF-8 in method name"
           Right name -> do
             seqid <- getI32At bs off1
             let !payloadBs = BS.drop (off1 + 4) bs
             payload <- decodeBinary payloadBs
             Right (ThriftMessage name mtype seqid payload)


decodeOldBinary :: ByteString -> Either String ThriftMessage
decodeOldBinary !bs = do
  (nameBytes, off1) <- getStr bs 0
  case TE.decodeUtf8' nameBytes of
    Left _ -> Left "decodeMessageBinary: invalid UTF-8 in method name"
    Right name -> do
      if off1 >= BS.length bs
        then Left "decodeMessageBinary: missing message type byte"
        else do
          let !mtypeByte = BSU.unsafeIndex bs off1
          case msgTypeFromWord8 mtypeByte of
            Nothing -> Left $ "decodeMessageBinary: invalid message type: " ++ show mtypeByte
            Just !mtype -> do
              seqid <- getI32At bs (off1 + 1)
              let !payloadBs = BS.drop (off1 + 5) bs
              payload <- decodeBinary payloadBs
              Right (ThriftMessage name mtype seqid payload)


--------------------------------------------------------------------------------
-- Compact Protocol
--------------------------------------------------------------------------------

compactProtocolId :: Word8
compactProtocolId = 0x82


compactVersion :: Word8
compactVersion = 1


encodeMessageCompact :: ThriftMessage -> ByteString
encodeMessageCompact (ThriftMessage name mtype seqid payload) =
  BL.toStrict $
    B.toLazyByteString $
      let !nameBytes = TE.encodeUtf8 name
          !vtByte = (compactVersion .&. 0x1F) .|. (msgTypeToWord8 mtype `shiftL` 5)
      in B.word8 compactProtocolId
           <> B.word8 vtByte
           <> putVarint (fromIntegral (fromIntegral seqid :: Word32))
           <> putVarint (fromIntegral (BS.length nameBytes))
           <> B.byteString nameBytes
           <> B.byteString (encodeCompact payload)


decodeMessageCompact :: ByteString -> Either String ThriftMessage
decodeMessageCompact !bs
  | BS.length bs < 2 = Left "decodeMessageCompact: insufficient data"
  | BSU.unsafeIndex bs 0 /= compactProtocolId =
      Left $ "decodeMessageCompact: invalid protocol ID: " ++ show (BSU.unsafeIndex bs 0)
  | otherwise =
      let !vtByte = BSU.unsafeIndex bs 1
          !ver = vtByte .&. 0x1F
          !mtypeBits = (vtByte `shiftR` 5) .&. 0x07
      in if ver /= compactVersion
           then Left $ "decodeMessageCompact: unsupported version: " ++ show ver
           else case msgTypeFromWord8 mtypeBits of
             Nothing -> Left $ "decodeMessageCompact: invalid message type: " ++ show mtypeBits
             Just !mtype -> do
               (seqidW, off1) <-
                 maybeToEither
                   "decodeMessageCompact: failed to read seqid varint"
                   (getVarint bs 2)
               let !seqid = fromIntegral seqidW :: Int32
               (nameLen, off2) <-
                 maybeToEither
                   "decodeMessageCompact: failed to read name length varint"
                   (getVarint bs off1)
               let !nLen = fromIntegral nameLen :: Int
               if off2 + nLen > BS.length bs
                 then Left "decodeMessageCompact: name extends past end of data"
                 else do
                   let !nameBytes = BS.take nLen (BS.drop off2 bs)
                   case TE.decodeUtf8' nameBytes of
                     Left _ -> Left "decodeMessageCompact: invalid UTF-8 in method name"
                     Right name -> do
                       let !payloadBs = BS.drop (off2 + nLen) bs
                       payload <- decodeCompact payloadBs
                       Right (ThriftMessage name mtype seqid payload)


--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither msg Nothing = Left msg
maybeToEither _ (Just a) = Right a
{-# INLINE maybeToEither #-}


putBE32 :: Word32 -> B.Builder
putBE32 !w =
  B.word8 (fromIntegral (w `shiftR` 24))
    <> B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
    <> B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
    <> B.word8 (fromIntegral (w .&. 0xFF))
{-# INLINE putBE32 #-}


putBE32i :: Word32 -> B.Builder
putBE32i = putBE32
{-# INLINE putBE32i #-}


getBE32 :: ByteString -> Int -> Word32
getBE32 !bs !off =
  let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word32
      !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word32
      !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word32
      !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word32
  in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
{-# INLINE getBE32 #-}


getStr :: ByteString -> Int -> Either String (ByteString, Int)
getStr !bs !off
  | off + 4 > BS.length bs = Left "unexpected end of data reading string length"
  | otherwise =
      let !len = fromIntegral (getBE32 bs off) :: Int
          !start = off + 4
      in if len < 0 || start + len > BS.length bs
           then Left "string length exceeds available data"
           else Right (BS.take len (BS.drop start bs), start + len)


getI32At :: ByteString -> Int -> Either String Int32
getI32At !bs !off
  | off + 4 > BS.length bs = Left "unexpected end of data reading i32"
  | otherwise = Right (fromIntegral (getBE32 bs off))


putVarint :: Word64 -> B.Builder
putVarint !n
  | n < 0x80 = B.word8 (fromIntegral n)
  | otherwise = B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <> putVarint (n `shiftR` 7)
{-# INLINE putVarint #-}


getVarint :: ByteString -> Int -> Maybe (Word64, Int)
getVarint !bs !off = go off 0 0
  where
    go !i !acc !shift
      | i >= BS.length bs = Nothing
      | shift >= 64 = Nothing
      | otherwise =
          let !b = fromIntegral (BSU.unsafeIndex bs i) :: Word64
              !acc' = acc .|. ((b .&. 0x7F) `shiftL` shift)
          in if b < 0x80
               then Just (acc', i + 1)
               else go (i + 1) acc' (shift + 7)
{-# INLINE getVarint #-}
