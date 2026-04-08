{-# LANGUAGE BangPatterns #-}
-- | Python Pickle protocol 2 decoder (stack-based VM).
--
-- Decodes a Pickle wire-format 'ByteString' by simulating the Pickle
-- virtual machine's stack. Supports protocols 0 through 5 opcodes:
-- MARK, STOP, INT, LONG, STRING, UNICODE, FLOAT, LIST, DICT, TUPLE,
-- SETITEMS, APPENDS, EMPTY_*, PROTO, FRAME, and more.
--
-- Uses mutable vectors for collecting items between MARK and closing
-- opcodes (APPENDS, SETITEMS, TUPLE).
module Pickle.Decode
  ( decode
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word32, Word64)
import qualified Data.Vector as V
import GHC.Float (castWord64ToDouble)

import qualified Pickle.Value as P

data StackItem
  = SValue !P.Value
  | SMark
  deriving stock (Show)

type Memo = [(Int, P.Value)]

decode :: ByteString -> Either String P.Value
decode bs
  | BS.length bs < 3 = Left "Pickle.Decode: input too short"
  | otherwise = do
      let !b0 = rdByte bs 0
          !b1 = rdByte bs 1
      if b0 == 0x80 && b1 == 0x02
        then runVM bs 2 [] []
        else if b0 == 0x80
        then runVM bs 2 [] []
        else runVM bs 0 [] []

memoGet :: Int -> Memo -> Maybe P.Value
memoGet _ [] = Nothing
memoGet k ((k', v) : rest)
  | k == k'   = Just v
  | otherwise  = memoGet k rest

memoPut :: Int -> P.Value -> Memo -> Memo
memoPut k v memo = (k, v) : memo

runVM :: ByteString -> Int -> [StackItem] -> Memo -> Either String P.Value
runVM bs off stack memo
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
          runVM bs (off + 2) stack memo

        0x95 -> do -- FRAME (protocol 4+): 8-byte length, skip
          ensure bs (off + 1) 8
          runVM bs (off + 9) stack memo

        0x4E -> -- NONE 'N'
          runVM bs (off + 1) (SValue P.None : stack) memo

        0x88 -> -- NEWTRUE
          runVM bs (off + 1) (SValue (P.Bool True) : stack) memo

        0x89 -> -- NEWFALSE
          runVM bs (off + 1) (SValue (P.Bool False) : stack) memo

        0x4A -> do -- BININT 'J': push 4-byte signed int (LE)
          ensure bs (off + 1) 4
          let !w = readLE32 bs (off + 1)
              !i = fromIntegral (fromIntegral w :: Int32) :: Int64
          runVM bs (off + 5) (SValue (P.Int i) : stack) memo

        0x4B -> do -- BININT1 'K': push 1-byte unsigned int
          ensure bs (off + 1) 1
          let !v = fromIntegral (rdByte bs (off + 1)) :: Int64
          runVM bs (off + 2) (SValue (P.Int v) : stack) memo

        0x4D -> do -- BININT2 'M': push 2-byte unsigned int (LE)
          ensure bs (off + 1) 2
          let !b0' = fromIntegral (rdByte bs (off + 1)) :: Int64
              !b1' = fromIntegral (rdByte bs (off + 2)) :: Int64
              !v = b0' .|. (b1' `shiftL` 8)
          runVM bs (off + 3) (SValue (P.Int v) : stack) memo

        0x8A -> do -- LONG1
          ensure bs (off + 1) 1
          let !nbytes = fromIntegral (rdByte bs (off + 1)) :: Int
          ensure bs (off + 2) nbytes
          let !val = decodeLittleEndianSigned bs (off + 2) nbytes
          runVM bs (off + 2 + nbytes) (SValue (P.Int val) : stack) memo

        0x8B -> do -- LONG4
          ensure bs (off + 1) 4
          let !nbytes = fromIntegral (readLE32 bs (off + 1)) :: Int
          ensure bs (off + 5) nbytes
          let !val = decodeLittleEndianSigned bs (off + 5) nbytes
          runVM bs (off + 5 + nbytes) (SValue (P.Int val) : stack) memo

        0x47 -> do -- BINFLOAT 'G'
          ensure bs (off + 1) 8
          let !w = readBE64 bs (off + 1)
              !d = castWord64ToDouble w
          runVM bs (off + 9) (SValue (P.Float d) : stack) memo

        0x43 -> do -- SHORT_BINBYTES 'C'
          ensure bs (off + 1) 1
          let !len = fromIntegral (rdByte bs (off + 1)) :: Int
          ensure bs (off + 2) len
          let !dat = BSU.unsafeTake len (BSU.unsafeDrop (off + 2) bs)
          runVM bs (off + 2 + len) (SValue (P.Bytes dat) : stack) memo

        0x42 -> do -- BINBYTES 'B'
          ensure bs (off + 1) 4
          let !len = fromIntegral (readLE32 bs (off + 1)) :: Int
          ensure bs (off + 5) len
          let !dat = BSU.unsafeTake len (BSU.unsafeDrop (off + 5) bs)
          runVM bs (off + 5 + len) (SValue (P.Bytes dat) : stack) memo

        0x8C -> do -- SHORT_BINUNICODE
          ensure bs (off + 1) 1
          let !len = fromIntegral (rdByte bs (off + 1)) :: Int
          ensure bs (off + 2) len
          let !raw = BSU.unsafeTake len (BSU.unsafeDrop (off + 2) bs)
          case TE.decodeUtf8' raw of
            Left _ -> Left "Pickle.Decode: invalid UTF-8"
            Right t -> runVM bs (off + 2 + len) (SValue (P.String t) : stack) memo

        0x58 -> do -- BINUNICODE 'X'
          ensure bs (off + 1) 4
          let !len = fromIntegral (readLE32 bs (off + 1)) :: Int
          ensure bs (off + 5) len
          let !raw = BSU.unsafeTake len (BSU.unsafeDrop (off + 5) bs)
          case TE.decodeUtf8' raw of
            Left _ -> Left "Pickle.Decode: invalid UTF-8"
            Right t -> runVM bs (off + 5 + len) (SValue (P.String t) : stack) memo

        0x5D -> -- EMPTY_LIST ']'
          runVM bs (off + 1) (SValue (P.List V.empty) : stack) memo

        0x7D -> -- EMPTY_DICT '}'
          runVM bs (off + 1) (SValue (P.Dict V.empty) : stack) memo

        0x29 -> -- EMPTY_TUPLE ')'
          runVM bs (off + 1) (SValue (P.Tuple V.empty) : stack) memo

        0x28 -> -- MARK '('
          runVM bs (off + 1) (SMark : stack) memo

        0x65 -> do -- APPENDS 'e'
          let (items, rest) = collectToMark stack
          case rest of
            (SValue (P.List existing) : rest2) ->
              runVM bs (off + 1) (SValue (P.List (existing <> V.fromList items)) : rest2) memo
            _ -> Left "Pickle.Decode: APPENDS without list"

        0x61 -> do -- APPEND 'a': append TOS to list below it
          case stack of
            (SValue item : SValue (P.List existing) : rest) ->
              runVM bs (off + 1) (SValue (P.List (V.snoc existing item)) : rest) memo
            _ -> Left "Pickle.Decode: APPEND without list"

        0x75 -> do -- SETITEMS 'u'
          let (items, rest) = collectToMark stack
          case rest of
            (SValue (P.Dict existing) : rest2) ->
              let pairs = makePairs items
              in runVM bs (off + 1) (SValue (P.Dict (existing <> V.fromList pairs)) : rest2) memo
            _ -> Left "Pickle.Decode: SETITEMS without dict"

        0x73 -> do -- SETITEM 's': key value dict -> dict with key=value
          case stack of
            (SValue val : SValue key : SValue (P.Dict existing) : rest) ->
              runVM bs (off + 1) (SValue (P.Dict (V.snoc existing (key, val))) : rest) memo
            _ -> Left "Pickle.Decode: SETITEM without dict"

        0x74 -> do -- TUPLE 't'
          let (items, rest) = collectToMark stack
          runVM bs (off + 1) (SValue (P.Tuple (V.fromList items)) : rest) memo

        0x85 -> -- TUPLE1
          case stack of
            (SValue a : rest) ->
              runVM bs (off + 1) (SValue (P.Tuple (V.singleton a)) : rest) memo
            _ -> Left "Pickle.Decode: TUPLE1 underflow"

        0x86 -> -- TUPLE2
          case stack of
            (SValue b : SValue a : rest) ->
              runVM bs (off + 1) (SValue (P.Tuple (V.fromList [a, b])) : rest) memo
            _ -> Left "Pickle.Decode: TUPLE2 underflow"

        0x87 -> -- TUPLE3
          case stack of
            (SValue c : SValue b : SValue a : rest) ->
              runVM bs (off + 1) (SValue (P.Tuple (V.fromList [a, b, c])) : rest) memo
            _ -> Left "Pickle.Decode: TUPLE3 underflow"

        0x71 -> do -- BINPUT 'q': store TOS in memo[1-byte key]
          ensure bs (off + 1) 1
          let !key = fromIntegral (rdByte bs (off + 1)) :: Int
          case stack of
            (SValue v : _) -> runVM bs (off + 2) stack (memoPut key v memo)
            _ -> runVM bs (off + 2) stack memo

        0x72 -> do -- LONG_BINPUT 'r': store TOS in memo[4-byte key]
          ensure bs (off + 1) 4
          let !key = fromIntegral (readLE32 bs (off + 1)) :: Int
          case stack of
            (SValue v : _) -> runVM bs (off + 5) stack (memoPut key v memo)
            _ -> runVM bs (off + 5) stack memo

        0x94 -> do -- MEMOIZE (protocol 4): store TOS in memo[next_idx]
          let !key = length memo
          case stack of
            (SValue v : _) -> runVM bs (off + 1) stack (memoPut key v memo)
            _ -> runVM bs (off + 1) stack memo

        0x68 -> do -- BINGET 'h': push memo[1-byte key]
          ensure bs (off + 1) 1
          let !key = fromIntegral (rdByte bs (off + 1)) :: Int
          case memoGet key memo of
            Just v  -> runVM bs (off + 2) (SValue v : stack) memo
            Nothing -> Left $ "Pickle.Decode: BINGET key not in memo: " ++ show key

        0x6A -> do -- LONG_BINGET 'j': push memo[4-byte key]
          ensure bs (off + 1) 4
          let !key = fromIntegral (readLE32 bs (off + 1)) :: Int
          case memoGet key memo of
            Just v  -> runVM bs (off + 5) (SValue v : stack) memo
            Nothing -> Left $ "Pickle.Decode: LONG_BINGET key not in memo: " ++ show key

        0x30 -> -- POP '0'
          case stack of
            (_ : rest) -> runVM bs (off + 1) rest memo
            _ -> Left "Pickle.Decode: POP on empty stack"

        0x32 -> do -- DUP '2'
          case stack of
            (top : _) -> runVM bs (off + 1) (top : stack) memo
            _ -> Left "Pickle.Decode: DUP on empty stack"

        0x8F -> -- EMPTY_SET (protocol 4)
          runVM bs (off + 1) (SValue (P.Set V.empty) : stack) memo

        0x90 -> do -- ADDITEMS (protocol 4): add items from MARK to set
          let (items, rest) = collectToMark stack
          case rest of
            (SValue (P.Set existing) : rest2) ->
              runVM bs (off + 1) (SValue (P.Set (existing <> V.fromList items)) : rest2) memo
            _ -> Left "Pickle.Decode: ADDITEMS without set"

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

readLE32 :: ByteString -> Int -> Word32
readLE32 bs off =
  let !b0 = fromIntegral (rdByte bs off) :: Word32
      !b1 = fromIntegral (rdByte bs (off + 1)) :: Word32
      !b2 = fromIntegral (rdByte bs (off + 2)) :: Word32
      !b3 = fromIntegral (rdByte bs (off + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

readBE64 :: ByteString -> Int -> Word64
readBE64 bs off =
  let rd i = fromIntegral (rdByte bs (off + i)) :: Word64
  in (rd 0 `shiftL` 56) .|. (rd 1 `shiftL` 48) .|. (rd 2 `shiftL` 40) .|. (rd 3 `shiftL` 32)
     .|. (rd 4 `shiftL` 24) .|. (rd 5 `shiftL` 16) .|. (rd 6 `shiftL` 8) .|. rd 7
rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

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
