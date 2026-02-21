{-# LANGUAGE BangPatterns #-}
-- | Dynamic messages for runtime protobuf manipulation.
--
-- A 'DynamicMessage' can represent any protobuf message without
-- compile-time generated code, using field descriptors to interpret
-- the wire format at runtime.
--
-- Use cases:
--
-- * Proxies and middleware that forward messages without knowing types
-- * Schema registries and tooling
-- * Testing and debugging
-- * Dynamic configuration systems
module Proto.Dynamic
  ( -- * Dynamic value type
    DynamicValue (..)

    -- * Dynamic message
  , DynamicMessage (..)
  , emptyDynamic
  , dynamicField
  , setDynamicField
  , removeDynamicField

    -- * Encoding / decoding
  , encodeDynamic
  , decodeDynamic

    -- * Conversion
  , dynamicToJson
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64)

import Proto.Wire (Tag(..), WireType(..))
import Proto.Wire.Encode (putTag, putVarint, putFixed32, putFixed64,
  putFloat, putDouble, putText, putByteString, putLengthDelimited)
import Proto.Wire.Decode (Decoder, getTagOr, getVarint, getFixed32, getFixed64,
  getFloat, getDouble, getText, getLengthDelimited, skipField, runDecoder, DecodeError)
import Proto.JSON (JsonValue(..), renderJson)

-- | A dynamically-typed protobuf value.
data DynamicValue
  = DynVarint   !Word64
  | DynSVarint  !Int64
  | DynFixed32  !Word32
  | DynFixed64  !Word64
  | DynFloat    !Float
  | DynDouble   !Double
  | DynBool     !Bool
  | DynString   !Text
  | DynBytes    !ByteString
  | DynMessage  !DynamicMessage
  | DynEnum     !Int
  | DynRepeated ![DynamicValue]
  | DynMap      !(Map DynamicValue DynamicValue)
  deriving stock (Show, Eq, Ord)

-- | A dynamically-typed protobuf message, storing fields by number.
data DynamicMessage = DynamicMessage
  { dynFields :: !(Map Int DynamicValue)
  , dynUnknownFields :: ![(Int, WireType, ByteString)]
  } deriving stock (Show, Eq, Ord)

emptyDynamic :: DynamicMessage
emptyDynamic = DynamicMessage Map.empty []

dynamicField :: Int -> DynamicMessage -> Maybe DynamicValue
dynamicField n (DynamicMessage fs _) = Map.lookup n fs

setDynamicField :: Int -> DynamicValue -> DynamicMessage -> DynamicMessage
setDynamicField n v (DynamicMessage fs unk) =
  DynamicMessage (Map.insert n v fs) unk

removeDynamicField :: Int -> DynamicMessage -> DynamicMessage
removeDynamicField n (DynamicMessage fs unk) =
  DynamicMessage (Map.delete n fs) unk

-- | Encode a dynamic message to bytes.
-- Uses varint wire type for integer values, length-delimited for strings/bytes/messages.
encodeDynamic :: DynamicMessage -> ByteString
encodeDynamic (DynamicMessage fs _) =
  BL.toStrict $ B.toLazyByteString $ Map.foldlWithKey' encodeField mempty fs
  where
    encodeField acc fn val = acc <> encodeDynValue fn val

encodeDynValue :: Int -> DynamicValue -> B.Builder
encodeDynValue fn = \case
  DynVarint v  -> putTag fn WireVarint <> putVarint v
  DynSVarint v -> putTag fn WireVarint <> putVarint (fromIntegral v)
  DynFixed32 v -> putTag fn Wire32Bit <> putFixed32 v
  DynFixed64 v -> putTag fn Wire64Bit <> putFixed64 v
  DynFloat v   -> putTag fn Wire32Bit <> putFloat v
  DynDouble v  -> putTag fn Wire64Bit <> putDouble v
  DynBool v    -> putTag fn WireVarint <> putVarint (if v then 1 else 0)
  DynString v  -> putTag fn WireLengthDelimited <> putText v
  DynBytes v   -> putTag fn WireLengthDelimited <> putByteString v
  DynEnum v    -> putTag fn WireVarint <> putVarint (fromIntegral v)
  DynMessage m ->
    let payload = encodeDynamic m
    in putTag fn WireLengthDelimited <> putLengthDelimited payload
  DynRepeated vs -> foldMap (encodeDynValue fn) vs
  DynMap _kvs -> mempty

-- | Decode a dynamic message from bytes using wire type inference.
decodeDynamic :: ByteString -> Either DecodeError DynamicMessage
decodeDynamic = runDecoder decodeDynLoop

decodeDynLoop :: Decoder DynamicMessage
decodeDynLoop = go Map.empty
  where
    go !acc = do
      mt <- getTagOr
      case mt of
        Nothing -> pure (DynamicMessage acc [])
        Just (Tag fn wt) -> do
          val <- decodeWireValue wt
          let acc' = case Map.lookup fn acc of
                Nothing -> Map.insert fn val acc
                Just (DynRepeated vs) -> Map.insert fn (DynRepeated (vs <> [val])) acc
                Just existing -> Map.insert fn (DynRepeated [existing, val]) acc
          go acc'

decodeWireValue :: WireType -> Decoder DynamicValue
decodeWireValue = \case
  WireVarint -> DynVarint <$> getVarint
  Wire64Bit -> DynFixed64 <$> getFixed64
  Wire32Bit -> DynFixed32 <$> getFixed32
  WireLengthDelimited -> DynBytes <$> getLengthDelimited
  wt -> skipField wt >> pure (DynBytes BS.empty)

-- | Convert a dynamic message to JSON (field numbers as keys).
dynamicToJson :: DynamicMessage -> JsonValue
dynamicToJson (DynamicMessage fs _) =
  JsonObject (Map.mapKeys (T.pack . show) (fmap dynValueToJson fs))

dynValueToJson :: DynamicValue -> JsonValue
dynValueToJson = \case
  DynVarint v  -> JsonNumber (fromIntegral v)
  DynSVarint v -> JsonNumber (fromIntegral v)
  DynFixed32 v -> JsonNumber (fromIntegral v)
  DynFixed64 v -> JsonString (T.pack (show v))
  DynFloat v   -> JsonNumber (realToFrac v)
  DynDouble v  -> JsonNumber v
  DynBool v    -> JsonBool v
  DynString v  -> JsonString v
  DynBytes _   -> JsonString "<bytes>"
  DynEnum v    -> JsonNumber (fromIntegral v)
  DynMessage m -> dynamicToJson m
  DynRepeated vs -> JsonArray (fmap dynValueToJson vs)
  DynMap _     -> JsonObject mempty
