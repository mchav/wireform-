{-# LANGUAGE BangPatterns #-}
-- | Incremental\/streaming decode for MessagePack values.
--
-- MessagePack values are self-delimiting, so the streaming decoder
-- reads one complete value at a time. When the input is incomplete,
-- it returns 'Partial' requesting more bytes.
module MsgPack.Stream
  ( DecodeStep(..)
  , streamDecode
  , feedMore
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word32, Word64)
import qualified Data.Vector as V
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import qualified MsgPack.Value as MV

-- | Result of an incremental decode step.
data DecodeStep a
  = Done !a !ByteString
  | Partial (ByteString -> DecodeStep a)
  | Fail !String

instance Show a => Show (DecodeStep a) where
  show (Done a bs) = "Done " ++ show a ++ " (" ++ show (BS.length bs) ++ " leftover)"
  show (Partial _) = "Partial _"
  show (Fail e)    = "Fail " ++ show e

-- | Begin streaming decode of a single MessagePack value.
streamDecode :: ByteString -> DecodeStep MV.Value
streamDecode = tryDecode BS.empty

-- | Feed more bytes into a 'Partial' continuation.
feedMore :: DecodeStep a -> ByteString -> DecodeStep a
feedMore (Partial k) bs = k bs
feedMore step _         = step

tryDecode :: ByteString -> ByteString -> DecodeStep MV.Value
tryDecode !accum !new =
  let !buf = if BS.null accum then new else accum <> new
  in if BS.null buf
     then Partial $ \more ->
            if BS.null more
            then Fail "MsgPack.Stream: unexpected end of input"
            else tryDecode buf more
     else case decodeOneWithLeftover buf of
            Right (val, leftover) -> Done val leftover
            Left _ -> Partial $ \more ->
              if BS.null more
              then Fail "MsgPack.Stream: incomplete MessagePack value"
              else tryDecode buf more

decodeOneWithLeftover :: ByteString -> Either String (MV.Value, ByteString)
decodeOneWithLeftover bs
  | BS.null bs = Left "empty input"
  | otherwise = case decodeOneValue bs 0 of
      Left e -> Left e
      Right (val, off) -> Right (val, BS.drop off bs)

decodeOneValue :: ByteString -> Int -> Either String (MV.Value, Int)
decodeOneValue !bs !off
  | off >= BS.length bs = Left "unexpected end of input"
  | otherwise =
      let !b = BS.index bs off
          !off1 = off + 1
      in dispatchDecode bs off1 b

dispatchDecode :: ByteString -> Int -> Word8 -> Either String (MV.Value, Int)
dispatchDecode bs off b
  | b <= 0x7f = Right (MV.Word (fromIntegral b), off)
  | b >= 0x80 && b <= 0x8f = decodeMapN bs off (fromIntegral (b .&. 0x0f))
  | b >= 0x90 && b <= 0x9f = decodeArrayN bs off (fromIntegral (b .&. 0x0f))
  | b >= 0xa0 && b <= 0xbf = decodeStr bs off (fromIntegral (b .&. 0x1f))
  | b == 0xc0 = Right (MV.Nil, off)
  | b == 0xc1 = Left "reserved byte 0xc1"
  | b == 0xc2 = Right (MV.Bool False, off)
  | b == 0xc3 = Right (MV.Bool True, off)
  | b == 0xc4 = requireN bs off 1 $ decodeBin bs (off + 1) (fromIntegral (BS.index bs off))
  | b == 0xc5 = requireN bs off 2 $ decodeBin bs (off + 2) (fromIntegral (readBE16' bs off))
  | b == 0xc6 = requireN bs off 4 $ decodeBin bs (off + 4) (fromIntegral (readBE32' bs off))
  | b == 0xc7 = decodeExtVariant bs off 0xc7
  | b == 0xc8 = decodeExtVariant bs off 0xc8
  | b == 0xc9 = decodeExtVariant bs off 0xc9
  | b == 0xca = requireN bs off 4 $
      let !w = readBE32' bs off
      in Right (MV.Float (castWord32ToFloat (fromIntegral w)), off + 4)
  | b == 0xcb = requireN bs off 8 $
      let !w = readBE64' bs off
      in Right (MV.Double (castWord64ToDouble w), off + 8)
  | b == 0xcc = requireN bs off 1 $ Right (MV.Word (fromIntegral (BS.index bs off)), off + 1)
  | b == 0xcd = requireN bs off 2 $ Right (MV.Word (readBE16' bs off), off + 2)
  | b == 0xce = requireN bs off 4 $ Right (MV.Word (readBE32' bs off), off + 4)
  | b == 0xcf = requireN bs off 8 $ Right (MV.Word (readBE64' bs off), off + 8)
  | b == 0xd0 = requireN bs off 1 $
      Right (MV.Int (fromIntegral (fromIntegral (BS.index bs off) :: Int8)), off + 1)
  | b == 0xd1 = requireN bs off 2 $
      Right (MV.Int (fromIntegral (fromIntegral (readBE16' bs off) :: Int16)), off + 2)
  | b == 0xd2 = requireN bs off 4 $
      Right (MV.Int (fromIntegral (fromIntegral (readBE32' bs off) :: Int32)), off + 4)
  | b == 0xd3 = requireN bs off 8 $
      Right (MV.Int (fromIntegral (readBE64' bs off) :: Int64), off + 8)
  | b == 0xd4 = decodeFixExt bs off 1
  | b == 0xd5 = decodeFixExt bs off 2
  | b == 0xd6 = decodeFixExt bs off 4
  | b == 0xd7 = decodeFixExt bs off 8
  | b == 0xd8 = decodeFixExt bs off 16
  | b == 0xd9 = requireN bs off 1 $ decodeStr bs (off + 1) (fromIntegral (BS.index bs off))
  | b == 0xda = requireN bs off 2 $ decodeStr bs (off + 2) (fromIntegral (readBE16' bs off))
  | b == 0xdb = requireN bs off 4 $ decodeStr bs (off + 4) (fromIntegral (readBE32' bs off))
  | b == 0xdc = requireN bs off 2 $ decodeArrayN bs (off + 2) (fromIntegral (readBE16' bs off))
  | b == 0xdd = requireN bs off 4 $ decodeArrayN bs (off + 4) (fromIntegral (readBE32' bs off))
  | b == 0xde = requireN bs off 2 $ decodeMapN bs (off + 2) (fromIntegral (readBE16' bs off))
  | b == 0xdf = requireN bs off 4 $ decodeMapN bs (off + 4) (fromIntegral (readBE32' bs off))
  | otherwise = Right (MV.Int (fromIntegral (fromIntegral b :: Int8)), off)

decodeExtVariant :: ByteString -> Int -> Word8 -> Either String (MV.Value, Int)
decodeExtVariant bs off variant
  | variant == 0xc7 = requireN bs off 2 $ do
      let !elen = fromIntegral (BS.index bs off)
          !ty   = fromIntegral (BS.index bs (off + 1)) :: Int8
      requireN bs (off + 2) elen $
        if ty == -1
        then decodeTimestamp (off + 2) elen bs
        else let !dat = BS.take elen (BS.drop (off + 2) bs)
             in Right (MV.Ext ty dat, off + 2 + elen)
  | variant == 0xc8 = requireN bs off 3 $ do
      let !elen = fromIntegral (readBE16' bs off)
          !ty   = fromIntegral (BS.index bs (off + 2)) :: Int8
      requireN bs (off + 3) elen $
        if ty == -1
        then decodeTimestamp (off + 3) elen bs
        else let !dat = BS.take elen (BS.drop (off + 3) bs)
             in Right (MV.Ext ty dat, off + 3 + elen)
  | variant == 0xc9 = requireN bs off 5 $ do
      let !elen = fromIntegral (readBE32' bs off)
          !ty   = fromIntegral (BS.index bs (off + 4)) :: Int8
      requireN bs (off + 5) elen $
        if ty == -1
        then decodeTimestamp (off + 5) elen bs
        else let !dat = BS.take elen (BS.drop (off + 5) bs)
             in Right (MV.Ext ty dat, off + 5 + elen)
  | otherwise = Left "unexpected ext variant"

decodeFixExt :: ByteString -> Int -> Int -> Either String (MV.Value, Int)
decodeFixExt bs off elen = requireN bs off (1 + elen) $ do
  let !ty = fromIntegral (BS.index bs off) :: Int8
  if ty == -1
    then decodeTimestamp (off + 1) elen bs
    else let !dat = BS.take elen (BS.drop (off + 1) bs)
         in Right (MV.Ext ty dat, off + 1 + elen)

decodeTimestamp :: Int -> Int -> ByteString -> Either String (MV.Value, Int)
decodeTimestamp off elen bs = case elen of
  4 -> requireN bs off 4 $
    let !secs = readBE32' bs off
    in Right (MV.Timestamp (fromIntegral secs) 0, off + 4)
  8 -> requireN bs off 8 $
    let !w64 = readBE64' bs off
        !nsAdj = w64 `shiftR` 34
        !secs  = w64 .&. 0x3FFFFFFFF
    in Right (MV.Timestamp (fromIntegral secs) (fromIntegral nsAdj), off + 8)
  12 -> requireN bs off 12 $
    let !ns32 = readBE32' bs off
        !sec64 = readBE64' bs (off + 4)
    in Right (MV.Timestamp (fromIntegral sec64 :: Int64) (fromIntegral ns32 :: Word32), off + 12)
  _ -> Left $ "invalid timestamp ext size: " ++ show elen

decodeStr :: ByteString -> Int -> Int -> Either String (MV.Value, Int)
decodeStr bs off slen = requireN bs off slen $
  let !slice = BS.take slen (BS.drop off bs)
  in case TE.decodeUtf8' slice of
       Left _  -> Left "invalid UTF-8 in string"
       Right t -> Right (MV.String t, off + slen)

decodeBin :: ByteString -> Int -> Int -> Either String (MV.Value, Int)
decodeBin bs off blen = requireN bs off blen $
  Right (MV.Binary (BS.take blen (BS.drop off bs)), off + blen)

decodeArrayN :: ByteString -> Int -> Int -> Either String (MV.Value, Int)
decodeArrayN bs off0 cnt = go off0 cnt []
  where
    go !off 0 !acc = Right (MV.Array (V.fromList (reverse acc)), off)
    go !off n !acc = do
      (v, o) <- decodeOneValue bs off
      go o (n - 1) (v : acc)

decodeMapN :: ByteString -> Int -> Int -> Either String (MV.Value, Int)
decodeMapN bs off0 cnt = go off0 cnt []
  where
    go !off 0 !acc = Right (MV.Map (V.fromList (reverse acc)), off)
    go !off n !acc = do
      (k, o1) <- decodeOneValue bs off
      (v, o2) <- decodeOneValue bs o1
      go o2 (n - 1) ((k, v) : acc)

requireN :: ByteString -> Int -> Int -> Either String a -> Either String a
requireN bs off n action
  | off + n > BS.length bs = Left "unexpected end of input"
  | otherwise = action
{-# INLINE requireN #-}

readBE16' :: ByteString -> Int -> Word64
readBE16' bs off =
  (fromIntegral (BS.index bs off) `shiftL` 8) .|.
  fromIntegral (BS.index bs (off + 1))

readBE32' :: ByteString -> Int -> Word64
readBE32' bs off =
  (fromIntegral (BS.index bs off) `shiftL` 24) .|.
  (fromIntegral (BS.index bs (off + 1)) `shiftL` 16) .|.
  (fromIntegral (BS.index bs (off + 2)) `shiftL` 8) .|.
  fromIntegral (BS.index bs (off + 3))

readBE64' :: ByteString -> Int -> Word64
readBE64' bs off =
  (fromIntegral (BS.index bs off) `shiftL` 56) .|.
  (fromIntegral (BS.index bs (off + 1)) `shiftL` 48) .|.
  (fromIntegral (BS.index bs (off + 2)) `shiftL` 40) .|.
  (fromIntegral (BS.index bs (off + 3)) `shiftL` 32) .|.
  (fromIntegral (BS.index bs (off + 4)) `shiftL` 24) .|.
  (fromIntegral (BS.index bs (off + 5)) `shiftL` 16) .|.
  (fromIntegral (BS.index bs (off + 6)) `shiftL` 8) .|.
  fromIntegral (BS.index bs (off + 7))
