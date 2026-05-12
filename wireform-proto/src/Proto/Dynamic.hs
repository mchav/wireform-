{-# LANGUAGE BangPatterns #-}

{- | Dynamic messages for runtime protobuf manipulation.

A 'DynamicMessage' can represent any protobuf message without
compile-time generated code, using field descriptors to interpret
the wire format at runtime.

Use cases:

* Proxies and middleware that forward messages without knowing types
* Schema registries and tooling
* Testing and debugging
* Dynamic configuration systems
-}
module Proto.Dynamic (
  -- * Dynamic value type
  DynamicValue (..),

  -- * Dynamic message
  DynamicMessage (..),
  emptyDynamic,
  dynamicField,
  setDynamicField,
  removeDynamicField,

  -- * Encoding / decoding
  encodeDynamic,
  decodeDynamic,

  -- * Conversion
  dynamicToJson,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64)
import Proto.Wire (Tag (..), WireType (..))
import Proto.Wire.Decode (
  DecodeError,
  Decoder,
  UMaybe (UJust, UNothing),
  getDouble,
  getFixed32,
  getFixed64,
  getFloat,
  getLengthDelimited,
  getTagOrU,
  getText,
  getVarint,
  runDecoder,
  skipField,
 )
import Proto.Wire.Encode (
  putByteString,
  putDouble,
  putFixed32,
  putFixed64,
  putFloat,
  putLengthDelimited,
  putTag,
  putText,
  putVarint,
 )
import Wireform.Builder qualified as B


-- | A dynamically-typed protobuf value.
data DynamicValue
  = DynVarint !Word64
  | DynSVarint !Int64
  | DynFixed32 !Word32
  | DynFixed64 !Word64
  | DynFloat !Float
  | DynDouble !Double
  | DynBool !Bool
  | DynString !Text
  | DynBytes !ByteString
  | DynMessage !DynamicMessage
  | DynEnum !Int
  | DynRepeated ![DynamicValue]
  | DynMap !(Map DynamicValue DynamicValue)
  deriving stock (Show, Eq, Ord)


-- | A dynamically-typed protobuf message, storing fields by number.
data DynamicMessage = DynamicMessage
  { dynFields :: !(Map Int DynamicValue)
  , dynUnknownFields :: ![(Int, WireType, ByteString)]
  }
  deriving stock (Show, Eq, Ord)


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


{- | Encode a dynamic message to bytes.
Uses varint wire type for integer values, length-delimited for strings/bytes/messages.
-}
encodeDynamic :: DynamicMessage -> ByteString
encodeDynamic (DynamicMessage fs _) =
  BL.toStrict $ B.toLazyByteString $ Map.foldlWithKey' encodeField mempty fs
  where
    encodeField acc fn val = acc <> encodeDynValue fn val


encodeDynValue :: Int -> DynamicValue -> B.Builder
encodeDynValue fn = \case
  DynVarint v -> putTag fn WireVarint <> putVarint v
  DynSVarint v -> putTag fn WireVarint <> putVarint (fromIntegral v)
  DynFixed32 v -> putTag fn Wire32Bit <> putFixed32 v
  DynFixed64 v -> putTag fn Wire64Bit <> putFixed64 v
  DynFloat v -> putTag fn Wire32Bit <> putFloat v
  DynDouble v -> putTag fn Wire64Bit <> putDouble v
  DynBool v -> putTag fn WireVarint <> putVarint (if v then 1 else 0)
  DynString v -> putTag fn WireLengthDelimited <> putText v
  DynBytes v -> putTag fn WireLengthDelimited <> putByteString v
  DynEnum v -> putTag fn WireVarint <> putVarint (fromIntegral v)
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
      mt <- getTagOrU
      case mt of
        UNothing -> pure (DynamicMessage acc [])
        UJust (Tag fn wt) -> do
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
dynamicToJson :: DynamicMessage -> Aeson.Value
dynamicToJson (DynamicMessage fs _) =
  Aeson.Object
    ( AesonKM.fromList
        (fmap (\(k, v) -> (AesonKey.fromText (intToText k), dynValueToJson v)) (Map.toList fs))
    )


dynValueToJson :: DynamicValue -> Aeson.Value
dynValueToJson = \case
  DynVarint v -> Aeson.Number (fromIntegral v)
  DynSVarint v -> Aeson.Number (fromIntegral v)
  DynFixed32 v -> Aeson.Number (fromIntegral v)
  DynFixed64 v -> Aeson.String (word64ToText v)
  DynFloat v -> Aeson.Number (fromFloatDigits v)
  DynDouble v -> Aeson.Number (fromFloatDigits v)
  DynBool v -> Aeson.Bool v
  DynString v -> Aeson.String v
  DynBytes bs -> Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  DynEnum v -> Aeson.Number (fromIntegral v)
  DynMessage m -> dynamicToJson m
  DynRepeated vs -> Aeson.toJSON (fmap dynValueToJson vs)
  DynMap _ -> Aeson.object []


intToText :: Int -> Text
intToText n
  | n < 0 = "-" <> word64ToText (fromIntegral (negate n))
  | otherwise = word64ToText (fromIntegral n)


word64ToText :: Word64 -> Text
word64ToText 0 = "0"
word64ToText n = go T.empty n
  where
    go !acc 0 = acc
    go !acc v =
      let (!q, !r) = v `quotRem` 10
      in go (T.cons (toEnum (fromIntegral r + 48)) acc) q
