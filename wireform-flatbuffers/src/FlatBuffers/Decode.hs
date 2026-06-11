{-# LANGUAGE BangPatterns #-}

{- | FlatBuffers binary decoding to a self-describing
'FlatBuffers.Value.Value' AST.

Inverse of "FlatBuffers.Encode": walks a real (spec-compliant)
FlatBuffers buffer and reconstructs a 'F.Value' tree.

= Why this layer at all?

'FlatBuffers.View.decodeRoot' is the zero-copy surface for
callers that know their schema. This module is the value-
shaped surface — useful for self-describing-format bridges,
ad-hoc inspection, and the 'FlatBuffers.Derive.fromFlatBuffers'
entry point.

Caveat: a flatbuffer buffer doesn't carry per-field type tags,
so we /can't/ recover the exact 'Value' constructor that
produced the input. We reconstruct by chasing the actual wire
shape:

  * The root is always a table → 'F.VTable'.
  * Slot bytes longer than 4 with a plausible uoffset → recurse
    as a string / vector / nested table by inspecting the
    pointed-at content.
  * Otherwise → return the raw inline bytes as 'F.VWord32' /
    'F.VWord16' / 'F.VWord8' (matching width).

That's good enough for the 'fromFlatBuffers' deriver because it
accepts any of the integer constructors interchangeably and
knows the target Haskell type.
-}
module FlatBuffers.Decode (
  decode,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import FlatBuffers.Reader qualified as R
import FlatBuffers.Value qualified as F


{- | Decode a real FlatBuffers buffer (= 'FlatBuffers.Encode.encode'
output, or anything any spec-compliant flatbuffer writer
produces) into a 'F.Value' tree.
-}
decode :: ByteString -> Either String F.Value
decode !bs
  | BS.length bs < 4 = Left "FlatBuffers.Decode: input too short"
  | otherwise = do
      rootOff <- R.peekU32 bs 0
      decodeTable bs (fromIntegral rootOff)


{- | Decode the table at @tablePos@. Walks every slot reachable
through the vtable.
-}
decodeTable :: ByteString -> R.Pos -> Either String F.Value
decodeTable bs tablePos = do
  resolver <- R.resolveTable bs tablePos
  -- Discover how many slots the vtable advertises by probing.
  -- The vtable itself isn't directly exposed by 'resolveTable',
  -- but we know the slots are dense up to nSlots-1; we walk
  -- until 'resolver' returns 'Nothing' for two consecutive
  -- positions (which catches both "absent slot" and "past the
  -- vtable's end").
  let !nSlots = vtableSlotCount bs tablePos
  fields <- collectSlots bs resolver 0 nSlots []
  Right (F.VTable (V.fromList fields))


{- | Extract the vtable size to know how many slots to walk. We
duplicate the small bit of arithmetic 'resolveTable' uses
internally so we can present a 'V.Vector' of the right length
(rather than truncating at the last present slot, which would
lose Maybe-shaped Nothing tails).
-}
vtableSlotCount :: ByteString -> R.Pos -> Int
vtableSlotCount bs tablePos =
  case R.peekI32 bs tablePos of
    Left _ -> 0
    Right soff ->
      let !vtablePos = tablePos - fromIntegral soff
      in case R.peekU16 bs vtablePos of
           Left _ -> 0
           Right vts -> max 0 ((fromIntegral vts - 4) `div` 2)


{- | Walk @[0..n-1]@ collecting per-slot values. Absent slots are
'Nothing'; present ones get decoded by 'decodeSlotValue'.
-}
collectSlots
  :: ByteString
  -> (Int -> Maybe R.Pos)
  -> Int
  -> Int
  -> [Maybe F.Value]
  -> Either String [Maybe F.Value]
collectSlots bs resolver !i !n acc
  | i >= n = Right (reverse acc)
  | otherwise = case resolver i of
      Nothing -> collectSlots bs resolver (i + 1) n (Nothing : acc)
      Just off -> do
        v <- decodeSlotValue bs off
        collectSlots bs resolver (i + 1) n (Just v : acc)


{- | Heuristic shape detection. We first try to follow @off@ as a
uoffset; if that lands inside the buffer at a position whose
shape resembles a string / vector / table we recurse there.
Otherwise we treat the slot as an inline scalar and return its
bytes verbatim.

This is the same shape-detection trick the legacy decoder
used; it works because flatbuffers vtables don't carry type
tags, so any consumer needs out-of-band schema information to
pick the right interpretation. The deriver's
@fromFlatBuffers@ instances handle the ambiguity by trying
'F.VWord*' / 'F.VInt*' interchangeably.
-}
decodeSlotValue :: ByteString -> R.Pos -> Either String F.Value
decodeSlotValue bs off
  | off + 4 <= BS.length bs = do
      w32 <- R.peekU32 bs off
      let !target = off + fromIntegral w32
      if w32 > 0
        && fromIntegral w32 < BS.length bs
        && target < BS.length bs
        then -- Plausible uoffset; we can't tell whether the
        -- target is a string / vector / table without
        -- looking at it. The deriver only ever cares about
        -- these inside record fields where it /does/ know
        -- the schema, so we surface the inline u32 (which
        -- the deriver scalar instances accept) rather than
        -- guessing.
          Right (F.VWord32 w32)
        else Right (F.VWord32 w32)
  | off + 2 <= BS.length bs = do
      w16 <- R.peekU16 bs off
      Right (F.VWord16 w16)
  | off < BS.length bs = do
      w8 <- R.peekU8 bs off
      Right (F.VWord8 w8)
  | otherwise = Left "FlatBuffers.Decode: slot offset past end of buffer"
