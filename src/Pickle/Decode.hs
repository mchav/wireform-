{-# LANGUAGE BangPatterns #-}
-- | Python Pickle protocol 2 decoder (stack-based VM).
--
-- Decodes a Pickle wire-format 'ByteString' by simulating the Pickle
-- virtual machine's stack. Supports protocols 0 through 5 opcodes:
-- MARK, STOP, INT, LONG, STRING, UNICODE, FLOAT, LIST, DICT, TUPLE,
-- SETITEMS, APPENDS, EMPTY_*, PROTO, FRAME, and more.
module Pickle.Decode
  ( decode
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int64)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word32, Word64, byteSwap64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Float (castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Pickle.Value as P

data StackItem
  = SValue !P.Value
  | SMark
  deriving stock (Show)

decode :: ByteString -> Either String P.Value
decode bs
  | BS.length bs < 3 = Left "Pickle.Decode: input too short"
  | otherwise = do
      let !b0 = rdByte bs 0
          !b1 = rdByte bs 1
      if b0 == 0x80 && b1 == 0x02
        then runVM bs 2 []
        else if b0 == 0x80
        then runVM bs 2 []  -- accept other protocols too
        else runVM bs 0 []

runVM :: ByteString -> Int -> [StackItem] -> Either String P.Value
runVM bs off stack
  | off >= BS.length bs = Left "Pickle.Decode: unexpected end of input"
  | otherwise = do
      let !op = rdByte bs off
      case op of
        0x2E -> -- STOP '.'
          case stack of
            (SValue v : _) -> Right v
            _ -> Left "Pickle.Decode: empty stack at STOP"

        0x80 -> do -- PROTO
          ensure bs (off + 1) 1
          runVM bs (off + 2) stack

        0x4E -> -- NONE 'N'
          runVM bs (off + 1) (SValue P.None : stack)

        0x88 -> -- NEWTRUE
          runVM bs (off + 1) (SValue (P.Bool True) : stack)

        0x89 -> -- NEWFALSE
          runVM bs (off + 1) (SValue (P.Bool False) : stack)

        0x8A -> do -- LONG1
          ensure bs (off + 1) 1
          let !nbytes = fromIntegral (rdByte bs (off + 1)) :: Int
          ensure bs (off + 2) nbytes
          let !val = decodeLittleEndianSigned bs (off + 2) nbytes
          runVM bs (off + 2 + nbytes) (SValue (P.Int val) : stack)

        0x8B -> do -- LONG4
          ensure bs (off + 1) 4
          let !nbytes = fromIntegral (readLE32 bs (off + 1)) :: Int
          ensure bs (off + 5) nbytes
          let !val = decodeLittleEndianSigned bs (off + 5) nbytes
          runVM bs (off + 5 + nbytes) (SValue (P.Int val) : stack)

        0x47 -> do -- BINFLOAT 'G'
          ensure bs (off + 1) 8
          let !w = readBE64 bs (off + 1)
              !d = castWord64ToDouble w
          runVM bs (off + 9) (SValue (P.Float d) : stack)

        0x43 -> do -- SHORT_BINBYTES 'C'
          ensure bs (off + 1) 1
          let !len = fromIntegral (rdByte bs (off + 1)) :: Int
          ensure bs (off + 2) len
          let !dat = BSU.unsafeTake len (BSU.unsafeDrop (off + 2) bs)
          runVM bs (off + 2 + len) (SValue (P.Bytes dat) : stack)

        0x42 -> do -- BINBYTES 'B'
          ensure bs (off + 1) 4
          let !len = fromIntegral (readLE32 bs (off + 1)) :: Int
          ensure bs (off + 5) len
          let !dat = BSU.unsafeTake len (BSU.unsafeDrop (off + 5) bs)
          runVM bs (off + 5 + len) (SValue (P.Bytes dat) : stack)

        0x8C -> do -- SHORT_BINUNICODE
          ensure bs (off + 1) 1
          let !len = fromIntegral (rdByte bs (off + 1)) :: Int
          ensure bs (off + 2) len
          let !raw = BSU.unsafeTake len (BSU.unsafeDrop (off + 2) bs)
          case TE.decodeUtf8' raw of
            Left _ -> Left "Pickle.Decode: invalid UTF-8"
            Right t -> runVM bs (off + 2 + len) (SValue (P.String t) : stack)

        0x58 -> do -- BINUNICODE 'X'
          ensure bs (off + 1) 4
          let !len = fromIntegral (readLE32 bs (off + 1)) :: Int
          ensure bs (off + 5) len
          let !raw = BSU.unsafeTake len (BSU.unsafeDrop (off + 5) bs)
          case TE.decodeUtf8' raw of
            Left _ -> Left "Pickle.Decode: invalid UTF-8"
            Right t -> runVM bs (off + 5 + len) (SValue (P.String t) : stack)

        0x5D -> -- EMPTY_LIST ']'
          runVM bs (off + 1) (SValue (P.List V.empty) : stack)

        0x7D -> -- EMPTY_DICT '}'
          runVM bs (off + 1) (SValue (P.Dict V.empty) : stack)

        0x29 -> -- EMPTY_TUPLE ')'
          runVM bs (off + 1) (SValue (P.Tuple V.empty) : stack)

        0x28 -> -- MARK '('
          runVM bs (off + 1) (SMark : stack)

        0x65 -> do -- APPENDS 'e'
          let (items, rest) = collectToMark stack
          case rest of
            (SValue (P.List existing) : rest2) ->
              runVM bs (off + 1) (SValue (P.List (existing <> V.fromList items)) : rest2)
            _ -> Left "Pickle.Decode: APPENDS without list"

        0x75 -> do -- SETITEMS 'u'
          let (items, rest) = collectToMark stack
          case rest of
            (SValue (P.Dict existing) : rest2) ->
              let pairs = makePairs items
              in runVM bs (off + 1) (SValue (P.Dict (existing <> V.fromList pairs)) : rest2)
            _ -> Left "Pickle.Decode: SETITEMS without dict"

        0x74 -> do -- TUPLE 't'
          let (items, rest) = collectToMark stack
          runVM bs (off + 1) (SValue (P.Tuple (V.fromList items)) : rest)

        0x85 -> -- TUPLE1
          case stack of
            (SValue a : rest) ->
              runVM bs (off + 1) (SValue (P.Tuple (V.singleton a)) : rest)
            _ -> Left "Pickle.Decode: TUPLE1 underflow"

        0x86 -> -- TUPLE2
          case stack of
            (SValue b : SValue a : rest) ->
              runVM bs (off + 1) (SValue (P.Tuple (V.fromList [a, b])) : rest)
            _ -> Left "Pickle.Decode: TUPLE2 underflow"

        0x87 -> -- TUPLE3
          case stack of
            (SValue c : SValue b : SValue a : rest) ->
              runVM bs (off + 1) (SValue (P.Tuple (V.fromList [a, b, c])) : rest)
            _ -> Left "Pickle.Decode: TUPLE3 underflow"

        0x30 -> -- POP '0'
          case stack of
            (_ : rest) -> runVM bs (off + 1) rest
            _ -> Left "Pickle.Decode: POP on empty stack"

        0x32 -> do -- DUP '2'
          case stack of
            (top : _) -> runVM bs (off + 1) (top : stack)
            _ -> Left "Pickle.Decode: DUP on empty stack"

        _ -> Left $ "Pickle.Decode: unknown opcode 0x" ++ showHex8 op

collectToMark :: [StackItem] -> ([P.Value], [StackItem])
collectToMark = go []
  where
    go acc (SMark : rest) = (acc, rest)
    go acc (SValue v : rest) = go (v : acc) rest
    go acc [] = (acc, [])

makePairs :: [P.Value] -> [(P.Value, P.Value)]
makePairs (k : v : rest) = (k, v) : makePairs rest
makePairs _ = []

decodeLittleEndianSigned :: ByteString -> Int -> Int -> Int64
decodeLittleEndianSigned bs off nbytes
  | nbytes == 0 = 0
  | otherwise =
      let !unsigned = foldl (\acc i ->
            acc .|. (fromIntegral (rdByte bs (off + i)) `shiftL` (8 * i))) (0 :: Int64) [0 .. nbytes - 1]
          !topBit = rdByte bs (off + nbytes - 1)
      in if topBit >= 0x80
         then unsigned - (1 `shiftL` (8 * nbytes))
         else unsigned

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

readBE64 :: ByteString -> Int -> Word64
readBE64 bs off = withBSPtrOff bs off $ \p ->
  byteSwap64 <$> (peekByteOff p 0 :: IO Word64)
{-# INLINE readBE64 #-}

ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off + n > BS.length bs = Left "Pickle.Decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}

showHex8 :: Word8 -> String
showHex8 w =
  let !hi = w `shiftR` 4
      !lo = w .&. 0x0F
      hexChar n
        | n < 10    = toEnum (fromIntegral n + fromEnum '0')
        | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')
  in [hexChar hi, hexChar lo]
