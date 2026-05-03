{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Zero-copy FlatBuffers /views/.
--
-- The default 'FlatBuffers.Decode' / 'FlatBuffers.Derive' surface
-- materialises a 'FlatBuffers.Value.Value' AST and then walks it
-- to populate a user record. That's two boxed traversals over the
-- same data, plus an intermediate spine of 'Vector' / 'VTable' /
-- 'VString' nodes — none of which is /actually/ zero-copy.
--
-- This module gives the alternative: a tiny set of cursor types
-- (one boxed pair of @ByteString + Int@ each) plus a 'View'
-- typeclass for decoding /directly out of the buffer/. Strings
-- and byte vectors come back as 'ByteString' slices that share
-- the input's 'ForeignPtr', and scalar fields bottom out in a
-- single aligned load via "FlatBuffers.Reader".
--
-- = What you pay for
--
-- * One 'ByteString' header (24 bytes on 64-bit) per cursor that
--   crosses a function boundary. The payload is shared.
-- * A 'Text' allocation per UTF-8 string field — unavoidable, see
--   'FlatBuffers.Reader.readStringSlice' for the byte-level
--   alternative.
-- * Constructor allocation for the user's record once it's
--   reified at the leaves of decoding.
--
-- = What you don't pay for
--
-- * No intermediate 'FlatBuffers.Value.Value'.
-- * No 'Vector' allocation when traversing a vector of nested
--   tables — 'fbVectorIndex' returns a fresh cursor that points
--   into the original buffer.
-- * No byte-by-byte recomputation: every primitive uses
--   "FlatBuffers.Reader" which compiles each peek to a single
--   aligned load.
--
-- = Example
--
-- @
-- data Position = Position { name :: !Text, x :: !Int32, y :: !Int32 }
--
-- instance View Position where
--   view t = Position
--     \<$\> viewSlot   t 0
--     \<*\> viewSlot   t 1
--     \<*\> viewSlot   t 2
--
-- decode :: ByteString -> Either String Position
-- decode = decodeRoot
-- @
module FlatBuffers.View
  ( -- * Cursor types
    Table
  , FBVector
  , Struct
    -- * Root entry point
  , decodeRoot
  , rootTable
    -- * Table field access
  , View (..)
  , SlotView (..)
  , VectorElem (..)
  , viewSlot
  , viewSlotMaybe
  , viewSlotDefault
  , isSlotPresent
    -- * Vector cursors
  , fbVectorLength
  , fbVectorIndex
  , fbVectorToList
  , fbVectorToListWith
  , fbVectorTraverse_
    -- * Struct cursors
  , structField
    -- * Lower-level escapes
  , tableBuffer
  , tablePos
  , tableFromBuffer
  , vectorBuffer
  , vectorPos
  , followUOffset
  ) where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Text as T
import Data.Word (Word8, Word16, Word32, Word64)

import FlatBuffers.Reader

-- ============================================================
-- Cursor types
-- ============================================================

-- | A cursor that points at a flatbuffer table inside a shared
-- 'ByteString'. Constructing a 'Table' is O(1) — we only resolve
-- the vtable when a field is actually requested.
data Table = Table
  { _tBuf :: {-# UNPACK #-} !ByteString
    -- ^ The whole buffer. Reads share its 'ForeignPtr'.
  , _tPos :: {-# UNPACK #-} !Pos
    -- ^ Absolute position of the soffset_t at the table's start.
  }

-- | A cursor that points at a flatbuffer vector. Phantom-typed by
-- the element type so the 'View' instance for the elements can
-- pick the right stride / decoder.
--
-- Like 'Table', the 'FBVector' header is small and the payload is
-- never copied.
data FBVector a = FBVector
  { _vBuf :: {-# UNPACK #-} !ByteString
  , _vPos :: {-# UNPACK #-} !Pos
  }

-- | A cursor at a flatbuffer struct (fixed-size, inline).
-- Distinguished from 'Table' because structs have no vtable; the
-- caller computes inline offsets directly.
data Struct = Struct
  { _sBuf :: {-# UNPACK #-} !ByteString
  , _sPos :: {-# UNPACK #-} !Pos
  }

-- | Peek at the underlying buffer (escape hatch for code that
-- needs raw "FlatBuffers.Reader" access).
tableBuffer :: Table -> ByteString
tableBuffer (Table b _) = b
{-# INLINE tableBuffer #-}

tablePos :: Table -> Pos
tablePos (Table _ p) = p
{-# INLINE tablePos #-}

-- | Construct a 'Table' cursor from raw @ByteString + Pos@.
-- Mostly used by the deriver to chase a uoffset and hand off to
-- the user's 'view' implementation.
tableFromBuffer :: ByteString -> Pos -> Table
tableFromBuffer = Table
{-# INLINE tableFromBuffer #-}

vectorBuffer :: FBVector a -> ByteString
vectorBuffer (FBVector b _) = b
{-# INLINE vectorBuffer #-}

vectorPos :: FBVector a -> Pos
vectorPos (FBVector _ p) = p
{-# INLINE vectorPos #-}

-- ============================================================
-- Root entry point
-- ============================================================

-- | Resolve the root table of a flatbuffer. Equivalent to
-- chasing the u32 root offset at byte 0 of the buffer.
rootTable :: ByteString -> Either String Table
rootTable bs
  | BS.length bs < 4 = Left "FlatBuffers.View.rootTable: buffer too short"
  | otherwise = do
      rootOff <- peekU32 bs 0
      Right (Table bs (fromIntegral rootOff))
{-# INLINE rootTable #-}

-- | Decode a buffer's root table directly into a user type. The
-- canonical entry point for application code.
decodeRoot :: View a => ByteString -> Either String a
decodeRoot bs = rootTable bs >>= view
{-# INLINE decodeRoot #-}

-- ============================================================
-- View typeclass
-- ============================================================

-- | Types that can be reified from a flatbuffer 'Table' without
-- an intermediate AST. Generated by 'FlatBuffers.Derive.deriveView'
-- for record types; hand-written for scalar / collection
-- combinators below.
--
-- The class is positional: 'view' on a 'Table' walks its slots in
-- declaration order, exactly like 'FlatBuffers.Derive.fromFlatBuffers'.
class View a where
  view :: Table -> Either String a

-- ============================================================
-- Slot helpers
-- ============================================================

-- | Read the @i@th slot of a table as a value of type @a@.
-- Required slots fail with a descriptive error if absent; use
-- 'viewSlotMaybe' for optional slots and 'viewSlotDefault' to
-- supply a default.
viewSlot :: forall a. SlotView a => Table -> Int -> Either String a
viewSlot t@(Table bs _) !i = do
  resolver <- resolveTable bs (tablePos t)
  case resolver i of
    Nothing  -> Left ("FlatBuffers.View.viewSlot: missing slot " <> show i)
    Just off -> readSlot @a bs off
{-# INLINE viewSlot #-}

-- | Read an optional slot. 'Nothing' for absent slots, 'Just' for
-- present ones — matching how the deriver maps @Maybe a@ fields.
viewSlotMaybe :: forall a. SlotView a => Table -> Int -> Either String (Maybe a)
viewSlotMaybe t@(Table bs _) !i = do
  resolver <- resolveTable bs (tablePos t)
  case resolver i of
    Nothing  -> Right Nothing
    Just off -> Just <$> readSlot @a bs off
{-# INLINE viewSlotMaybe #-}

-- | Read a slot, falling back to a default when absent. This is
-- the canonical translation for FlatBuffers default values.
viewSlotDefault :: forall a. SlotView a => Table -> Int -> a -> Either String a
viewSlotDefault t@(Table bs _) !i def = do
  resolver <- resolveTable bs (tablePos t)
  case resolver i of
    Nothing  -> Right def
    Just off -> readSlot @a bs off
{-# INLINE viewSlotDefault #-}

-- | Cheap "is this slot in the buffer?" check. Useful when you
-- want to discriminate between an absent slot and a slot whose
-- value happens to equal the schema default.
isSlotPresent :: Table -> Int -> Bool
isSlotPresent t@(Table bs _) !i =
  case resolveTable bs (tablePos t) of
    Left _         -> False
    Right resolver -> case resolver i of
      Nothing -> False
      Just _  -> True
{-# INLINE isSlotPresent #-}

-- | Internal helper: how to read a value of type @a@ from a slot
-- whose absolute byte position is known. Inline scalars live
-- /at/ the slot; tables / strings / vectors live behind a u32
-- relative offset there.
--
-- Hand-written instances handle the difference. The deriver
-- consumes 'View'; user records become @SlotView@ via the
-- 'TableSlot' newtype below at instance-emission time.
class SlotView a where
  readSlot :: ByteString -> Pos -> Either String a

instance SlotView Bool where
  readSlot bs off = (/= 0) <$> peekU8 bs off
  {-# INLINE readSlot #-}

instance SlotView Word8  where readSlot = peekU8;  {-# INLINE readSlot #-}
instance SlotView Word16 where readSlot = peekU16; {-# INLINE readSlot #-}
instance SlotView Word32 where readSlot = peekU32; {-# INLINE readSlot #-}
instance SlotView Word64 where readSlot = peekU64; {-# INLINE readSlot #-}
instance SlotView Int8   where readSlot = peekI8;  {-# INLINE readSlot #-}
instance SlotView Int16  where readSlot = peekI16; {-# INLINE readSlot #-}
instance SlotView Int32  where readSlot = peekI32; {-# INLINE readSlot #-}
instance SlotView Int64  where readSlot = peekI64; {-# INLINE readSlot #-}
instance SlotView Float  where readSlot = peekFloat;  {-# INLINE readSlot #-}
instance SlotView Double where readSlot = peekDouble; {-# INLINE readSlot #-}

-- | UTF-8 strings: chase the uoffset, then decode. Allocates a
-- 'Text'.
instance SlotView T.Text where
  readSlot bs off = followUOffset bs off >>= readString bs
  {-# INLINE readSlot #-}

-- | Raw bytes: chase the uoffset, return a slice of the buffer
-- (no copy).
instance SlotView ByteString where
  readSlot bs off = followUOffset bs off >>= readByteVectorSlice bs
  {-# INLINE readSlot #-}

-- | Nested tables: chase the uoffset, build a fresh cursor.
instance SlotView Table where
  readSlot bs off = do
    !pos <- followUOffset bs off
    Right (Table bs pos)
  {-# INLINE readSlot #-}

-- | Vectors: chase the uoffset, build a fresh cursor. The
-- element type is reflected at the type level so 'fbVectorIndex'
-- can pick the right stride.
instance SlotView (FBVector a) where
  readSlot bs off = do
    !pos <- followUOffset bs off
    Right (FBVector bs pos)
  {-# INLINE readSlot #-}

-- ============================================================
-- Vectors
-- ============================================================

-- | Number of elements. O(1): one u32 read.
fbVectorLength :: FBVector a -> Int
fbVectorLength (FBVector bs pos) =
  case vectorLength bs pos of
    Right n -> n
    Left _  -> 0
{-# INLINE fbVectorLength #-}

-- | Class for elements of a vector. Tells 'fbVectorIndex' the
-- per-element stride and how to decode at a position.
class VectorElem a where
  -- | Per-element stride in bytes.
  vectorStride :: proxy a -> Int
  -- | Decode an element at the given absolute byte position.
  -- For inline scalars / structs the position is the element
  -- itself; for tables / strings / nested vectors there's an
  -- intermediate u32 uoffset that 'readVectorElem' chases.
  readVectorElem :: ByteString -> Pos -> Either String a

instance VectorElem Bool where
  vectorStride _ = 1
  readVectorElem = readSlot
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}

instance VectorElem Word8 where
  vectorStride _ = 1; readVectorElem = peekU8
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}
instance VectorElem Word16 where
  vectorStride _ = 2; readVectorElem = peekU16
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}
instance VectorElem Word32 where
  vectorStride _ = 4; readVectorElem = peekU32
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}
instance VectorElem Word64 where
  vectorStride _ = 8; readVectorElem = peekU64
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}
instance VectorElem Int8 where
  vectorStride _ = 1; readVectorElem = peekI8
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}
instance VectorElem Int16 where
  vectorStride _ = 2; readVectorElem = peekI16
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}
instance VectorElem Int32 where
  vectorStride _ = 4; readVectorElem = peekI32
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}
instance VectorElem Int64 where
  vectorStride _ = 8; readVectorElem = peekI64
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}
instance VectorElem Float where
  vectorStride _ = 4; readVectorElem = peekFloat
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}
instance VectorElem Double where
  vectorStride _ = 8; readVectorElem = peekDouble
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}

-- | UTF-8 strings inside a vector. Stride is 4 (uoffset).
instance VectorElem T.Text where
  vectorStride _ = 4
  readVectorElem bs ePos = followUOffset bs ePos >>= readString bs
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}

-- | Raw byte slices inside a vector.
instance VectorElem ByteString where
  vectorStride _ = 4
  readVectorElem bs ePos = followUOffset bs ePos >>= readByteVectorSlice bs
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}

-- | Vectors of nested tables: each element is a uoffset_t that
-- points at a table. Indexing returns a fresh 'Table' cursor.
instance VectorElem Table where
  vectorStride _ = 4
  readVectorElem bs ePos = do
    !p <- followUOffset bs ePos
    Right (Table bs p)
  {-# INLINE vectorStride #-}
  {-# INLINE readVectorElem #-}

-- | The @i@th element. O(1) lookup, no payload copy.
fbVectorIndex :: forall a. VectorElem a => FBVector a -> Int -> Either String a
fbVectorIndex (FBVector bs pos) !i
  | i < 0     = Left "FlatBuffers.View.fbVectorIndex: negative index"
  | otherwise =
      let !stride = vectorStride (Nothing :: Maybe a)
          !ePos   = vectorElementAt pos stride i
      in  readVectorElem @a bs ePos
{-# INLINE fbVectorIndex #-}

-- | Realise the whole vector to a list. Allocates the spine; the
-- elements are still decoded lazily on demand. Each element pays
-- the per-element decode cost.
fbVectorToList :: VectorElem a => FBVector a -> Either String [a]
fbVectorToList = fbVectorToListWith id
{-# INLINE fbVectorToList #-}

-- | Realise the vector with a transformation applied per element.
-- Useful for vectors of nested tables where the user wants to
-- chain through 'view' immediately.
fbVectorToListWith
  :: forall a b. VectorElem a
  => (a -> b)
  -> FBVector a
  -> Either String [b]
fbVectorToListWith f v =
  let !n = fbVectorLength v
      go !i acc
        | i < 0     = Right acc
        | otherwise = do
            x <- fbVectorIndex v i
            go (i - 1) (f x : acc)
  in  go (n - 1) []
{-# INLINE fbVectorToListWith #-}

-- | Effectful traversal that doesn't materialise a list. Useful
-- for streaming a vector into a builder.
fbVectorTraverse_
  :: forall a m. (VectorElem a, Monad m)
  => (a -> m ())
  -> FBVector a
  -> Either String (m ())
fbVectorTraverse_ act v =
  let !n = fbVectorLength v
      step !mAcc !i = do
        x <- fbVectorIndex v i
        Right (mAcc >> act x)
  in  foldM step (pure ()) [0 .. n - 1]
{-# INLINE fbVectorTraverse_ #-}

-- ============================================================
-- Structs
-- ============================================================

-- | Read an inline-struct field at a known relative offset. The
-- caller supplies the offset (computed from the schema) and the
-- per-field decoder.
structField
  :: forall a. SlotView a
  => Struct
  -> Int       -- ^ relative offset of the field within the struct
  -> Either String a
structField (Struct bs basePos) !relOff = readSlot @a bs (basePos + relOff)
{-# INLINE structField #-}
