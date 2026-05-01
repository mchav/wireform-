{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Proto2 extensions runtime support.
--
-- Proto2 @extend Foo { optional int32 bar = 123; }@ declarations
-- expose typed accessors for fields whose numbers live in a message's
-- declared extension ranges. At the Haskell level we preserve those
-- fields through the existing unknown-field machinery in
-- "Proto.Decode" — each extension becomes a typed @'Extension' msg a@
-- descriptor that knows how to read the corresponding entry out of a
-- message's unknown-fields list and how to write it back in.
--
-- Typical generated shape:
--
-- @
-- -- In @Foo.hs@:
-- data Foo = Foo { fooUnknownFields :: ![UnknownField], ... }
--
-- -- In the module that contains the @extend Foo@ block:
-- barExt :: Extension Foo Int32
-- barExt = Extension
--   { extNumber = 123
--   , extType   = ExtInt32
--   }
-- @
--
-- Callers then use 'getExtension' / 'setExtension' / 'clearExtension'
-- to read, write, and remove the value.
module Proto.Extension
  ( -- * Extension descriptors
    Extension (..)
  , ExtensionType (..)
    -- * Accessing extension values on a message
  , HasExtensions (..)
  , hasExtension
  , getExtension
  , setExtension
  , clearExtension
  ) where

import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import GHC.Float
  ( castDoubleToWord64
  , castFloatToWord32
  , castWord32ToFloat
  , castWord64ToDouble
  )

import Proto.Decode (UnknownField (..))

-- ============================================================
-- Types
-- ============================================================

-- | A typed extension descriptor. Carries the wire-level field
-- number plus the type information needed to decode/encode the
-- value and tie it to the right 'UnknownField' constructor.
data Extension msg a = Extension
  { extNumber :: !Int
    -- ^ The field number declared in the @extend@ block.
  , extType   :: !(ExtensionType a)
    -- ^ Payload type information — drives the decoder + encoder.
  }

deriving stock instance Show (ExtensionType a) => Show (Extension msg a)

-- | Payload types supported by extensions today. Covers every
-- singular scalar proto2 supports plus an embedded message escape
-- hatch that round-trips raw bytes. Repeated / packed extensions
-- aren't covered here; the generated 'Extension' for those would
-- need a small additional wrapper.
data ExtensionType a where
  ExtInt32    :: ExtensionType Int32
  ExtInt64    :: ExtensionType Int64
  ExtUInt32   :: ExtensionType Word32
  ExtUInt64   :: ExtensionType Word64
  ExtSInt32   :: ExtensionType Int32
  ExtSInt64   :: ExtensionType Int64
  ExtFixed32  :: ExtensionType Word32
  ExtFixed64  :: ExtensionType Word64
  ExtSFixed32 :: ExtensionType Int32
  ExtSFixed64 :: ExtensionType Int64
  ExtFloat    :: ExtensionType Float
  ExtDouble   :: ExtensionType Double
  ExtBool     :: ExtensionType Bool
  ExtString   :: ExtensionType Text
  ExtBytes    :: ExtensionType ByteString
  ExtMessage  :: ExtensionType ByteString
    -- ^ Sub-message stored as its raw length-delimited payload;
    -- callers use 'Proto.Decode.decodeMessage' to re-project.

deriving stock instance Show (ExtensionType a)

-- ============================================================
-- Accessors
-- ============================================================

-- | Lens-like access to the unknown-fields list on a message.
-- Generated message types that carry extensions provide an instance
-- of this class; the three combinators below are written once in
-- terms of the instance.
class HasExtensions msg where
  messageUnknownFields    :: msg -> [UnknownField]
  setMessageUnknownFields :: [UnknownField] -> msg -> msg

-- | 'True' when the message carries a value for this extension.
hasExtension :: HasExtensions msg => Extension msg a -> msg -> Bool
hasExtension ext msg =
  any (\uf -> unknownFieldNumber uf == extNumber ext)
      (messageUnknownFields msg)

-- | Retrieve an extension value. Returns 'Nothing' when the field
-- is absent or the stored bytes don't fit the declared payload
-- type (corruption or aliasing — real deployments treat the
-- missing case as "use the extension's proto default", which
-- callers can layer on top).
getExtension :: HasExtensions msg => Extension msg a -> msg -> Maybe a
getExtension ext msg = do
  uf <- lookupField (extNumber ext) (messageUnknownFields msg)
  decodeExtensionValue (extType ext) uf

-- | Attach (or overwrite) an extension value. The underlying
-- unknown-fields list has any prior entries for the same field
-- number removed before the new one is appended, matching
-- protobuf's "last one wins" semantics for singular fields.
setExtension
  :: HasExtensions msg => Extension msg a -> a -> msg -> msg
setExtension ext value msg =
  let !fresh = encodeExtensionValue (extNumber ext) (extType ext) value
      !rest  = filter (\uf -> unknownFieldNumber uf /= extNumber ext)
                      (messageUnknownFields msg)
  in setMessageUnknownFields (rest ++ [fresh]) msg

-- | Remove an extension value. Leaves the message unchanged when
-- the extension wasn't set.
clearExtension
  :: HasExtensions msg => Extension msg a -> msg -> msg
clearExtension ext msg =
  setMessageUnknownFields
    (filter (\uf -> unknownFieldNumber uf /= extNumber ext)
            (messageUnknownFields msg))
    msg

-- ============================================================
-- Internal
-- ============================================================

unknownFieldNumber :: UnknownField -> Int
unknownFieldNumber = \case
  UnknownVarint n _      -> n
  UnknownFixed64 n _     -> n
  UnknownLenDelim n _ -> n
  UnknownFixed32 n _     -> n

-- The proto spec says later occurrences of a singular field
-- override earlier ones, so we walk from the tail.
lookupField :: Int -> [UnknownField] -> Maybe UnknownField
lookupField fn = go . reverse
  where
    go [] = Nothing
    go (uf:rest)
      | unknownFieldNumber uf == fn = Just uf
      | otherwise                   = go rest

decodeExtensionValue :: ExtensionType a -> UnknownField -> Maybe a
decodeExtensionValue ty uf = case (ty, uf) of
  (ExtInt32,    UnknownVarint _ v)      -> Just (fromIntegral v)
  (ExtInt64,    UnknownVarint _ v)      -> Just (fromIntegral v)
  (ExtUInt32,   UnknownVarint _ v)      -> Just (fromIntegral v)
  (ExtUInt64,   UnknownVarint _ v)      -> Just v
  (ExtSInt32,   UnknownVarint _ v)      -> Just (zigzagDecode32 v)
  (ExtSInt64,   UnknownVarint _ v)      -> Just (zigzagDecode64 v)
  (ExtBool,     UnknownVarint _ v)      -> Just (v /= 0)
  (ExtFixed32,  UnknownFixed32 _ v)     -> Just v
  (ExtSFixed32, UnknownFixed32 _ v)     -> Just (fromIntegral v)
  (ExtFloat,    UnknownFixed32 _ v)     -> Just (castWord32ToFloat v)
  (ExtFixed64,  UnknownFixed64 _ v)     -> Just v
  (ExtSFixed64, UnknownFixed64 _ v)     -> Just (fromIntegral v)
  (ExtDouble,   UnknownFixed64 _ v)     -> Just (castWord64ToDouble v)
  (ExtString,   UnknownLenDelim _ b) -> case TE.decodeUtf8' b of
    Right t -> Just t
    Left _  -> Nothing
  (ExtBytes,    UnknownLenDelim _ b) -> Just b
  (ExtMessage,  UnknownLenDelim _ b) -> Just b
  _ -> Nothing

encodeExtensionValue :: Int -> ExtensionType a -> a -> UnknownField
encodeExtensionValue fn ty value = case ty of
  ExtInt32    -> UnknownVarint      fn (fromIntegral value)
  ExtInt64    -> UnknownVarint      fn (fromIntegral value)
  ExtUInt32   -> UnknownVarint      fn (fromIntegral value)
  ExtUInt64   -> UnknownVarint      fn value
  ExtSInt32   -> UnknownVarint      fn (zigzagEncode32 value)
  ExtSInt64   -> UnknownVarint      fn (zigzagEncode64 value)
  ExtBool     -> UnknownVarint      fn (if value then 1 else 0)
  ExtFixed32  -> UnknownFixed32     fn value
  ExtSFixed32 -> UnknownFixed32     fn (fromIntegral value)
  ExtFloat    -> UnknownFixed32     fn (castFloatToWord32 value)
  ExtFixed64  -> UnknownFixed64     fn value
  ExtSFixed64 -> UnknownFixed64     fn (fromIntegral value)
  ExtDouble   -> UnknownFixed64     fn (castDoubleToWord64 value)
  ExtString   -> UnknownLenDelim fn (TE.encodeUtf8 value)
  ExtBytes    -> UnknownLenDelim fn value
  ExtMessage  -> UnknownLenDelim fn value

-- Zig-zag encodings per the protobuf spec.
zigzagEncode32 :: Int32 -> Word64
zigzagEncode32 n =
  let !w = fromIntegral n :: Word32
  in fromIntegral ((w `shiftL` 1) `xor` fromIntegral (n `shiftR` 31))

zigzagEncode64 :: Int64 -> Word64
zigzagEncode64 n =
  let !w = fromIntegral n :: Word64
  in (w `shiftL` 1) `xor` fromIntegral (n `shiftR` 63)

zigzagDecode32 :: Word64 -> Int32
zigzagDecode32 v =
  let !w = fromIntegral v :: Word32
  in fromIntegral ((w `shiftR` 1) `xor` (0 - (w .&. 1)))

zigzagDecode64 :: Word64 -> Int64
zigzagDecode64 v =
  fromIntegral ((v `shiftR` 1) `xor` (0 - (v .&. 1)))
