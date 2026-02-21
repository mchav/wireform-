{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Implementation of @google.protobuf.Any@.
--
-- @Any@ is protobuf's mechanism for embedding arbitrary messages
-- inside other messages without a compile-time dependency on the
-- contained type. The @type_url@ field identifies the type using
-- a URL like @type.googleapis.com/package.MessageName@, and @value@
-- holds the serialized bytes.
--
-- This module provides:
--
-- * 'IsMessage' typeclass: associates a Haskell type with its proto type URL
-- * 'packAny' / 'unpackAny': type-safe packing and unpacking
-- * 'TypeRegistry': dynamic dispatch for types not known at compile time
-- * 'unpackAnyDynamic': registry-based unpacking for heterogeneous messages
module Proto.Google.Protobuf.Any
  ( -- * The Any type
    Any (..)
  , defaultAny

    -- * Type-safe packing / unpacking
  , IsMessage (..)
  , packAny
  , packAnyWithPrefix
  , unpackAny
  , isMessageType

    -- * Type registry for dynamic dispatch
  , TypeRegistry
  , emptyRegistry
  , registerType
  , lookupType
  , unpackAnyDynamic
  , DynamicMessage (..)

    -- * Utilities
  , typeUrlPrefix
  , typeUrlOf
  , typeNameFromUrl
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.JSON
import Proto.Message (IsMessage (..))
import Proto.Wire (Tag (..))
import Proto.Wire.Encode (fieldTextSize, fieldBytesSize)

-- | The @google.protobuf.Any@ message.
data Any = Any
  { typeUrl :: !Text
  , value   :: !ByteString
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultAny :: Any
defaultAny = Any "" ""

instance MessageEncode Any where
  buildMessage (Any tu v) =
    (if tu == "" then mempty else encodeFieldString 1 tu) <>
    (if BS.null v then mempty else encodeFieldBytes 2 v)
  {-# INLINE buildMessage #-}

instance MessageSize Any where
  messageSize (Any tu v) =
    (if tu == "" then 0 else fieldTextSize 1 tu) +
    (if BS.null v then 0 else fieldBytesSize 2 v)
  {-# INLINE messageSize #-}

instance MessageDecode Any where
  messageDecoder = loop "" ""
    where
      loop !tu !v = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Any tu v)
          Just (Tag fn wt) -> case fn of
            1 -> do t <- decodeFieldString; loop t v
            2 -> do b <- decodeFieldBytes; loop tu b
            _ -> skipField wt >> loop tu v
  {-# INLINE messageDecoder #-}

instance IsMessage Any where
  messageTypeName _ = "google.protobuf.Any"

-- | The default type URL prefix used by Google's protobuf implementation.
typeUrlPrefix :: Text
typeUrlPrefix = "type.googleapis.com/"

-- | Compute the full type URL for a message type.
typeUrlOf :: forall a. IsMessage a => Proxy a -> Text
typeUrlOf p = typeUrlPrefix <> messageTypeName p

-- | Pack an arbitrary message into an 'Any', using the default
-- @type.googleapis.com/@ prefix.
--
-- @
-- let ts = Timestamp 1234567890 0
--     any = packAny ts
-- -- any.typeUrl == "type.googleapis.com/google.protobuf.Timestamp"
-- -- any.value   == encodeMessage ts
-- @
packAny :: forall a. IsMessage a => a -> Any
packAny msg = Any
  { typeUrl = typeUrlOf (Proxy :: Proxy a)
  , value   = encodeMessage msg
  }

-- | Pack with a custom URL prefix instead of the default.
packAnyWithPrefix :: forall a. IsMessage a => Text -> a -> Any
packAnyWithPrefix prefix msg = Any
  { typeUrl = prefix <> messageTypeName (Proxy :: Proxy a)
  , value   = encodeMessage msg
  }

-- | Unpack an 'Any' into a specific message type.
--
-- Returns 'Nothing' if the type URL doesn't match the expected type.
-- Returns 'Left' with a decode error if the bytes can't be parsed.
--
-- @
-- case unpackAny any of
--   Just (Right ts) -> print (ts :: Timestamp)
--   Just (Left err) -> error ("decode failed: " <> show err)
--   Nothing         -> error "wrong type URL"
-- @
unpackAny :: forall a. IsMessage a => Any -> Maybe (Either DecodeError a)
unpackAny (Any tu v)
  | isTypeMatch tu (Proxy :: Proxy a) = Just (decodeMessage v)
  | otherwise = Nothing

-- | Check if an 'Any' contains a specific message type.
isMessageType :: forall a. IsMessage a => Proxy a -> Any -> Bool
isMessageType p (Any tu _) = isTypeMatch tu p

-- | Check whether a type URL matches a given message type.
-- Handles both @type.googleapis.com/pkg.Name@ and bare @pkg.Name@ forms.
isTypeMatch :: forall a. IsMessage a => Text -> Proxy a -> Bool
isTypeMatch tu p =
  let expected = messageTypeName p
  in typeNameFromUrl tu == expected

-- | Extract the type name from a type URL.
-- @"type.googleapis.com/google.protobuf.Timestamp"@ -> @"google.protobuf.Timestamp"@
-- @"google.protobuf.Timestamp"@ -> @"google.protobuf.Timestamp"@
typeNameFromUrl :: Text -> Text
typeNameFromUrl tu = case T.breakOnEnd "/" tu of
  ("", name)  -> name
  (_, name)   -> name

-- | A dynamically-typed decoded message, for use with 'TypeRegistry'.
data DynamicMessage = forall a. (Show a, IsMessage a) => DynamicMessage !a

instance Show DynamicMessage where
  show (DynamicMessage a) = show a

-- | A registry mapping type names to decoders, for runtime dispatch
-- of 'Any' values when the contained type is not known at compile time.
newtype TypeRegistry = TypeRegistry
  (Map Text (ByteString -> Either DecodeError DynamicMessage))

emptyRegistry :: TypeRegistry
emptyRegistry = TypeRegistry Map.empty


-- | Register a message type in the registry.
--
-- @
-- registry = registerType (Proxy :: Proxy Timestamp)
--          . registerType (Proxy :: Proxy Duration)
--          $ emptyRegistry
-- @
registerType :: forall a. (Show a, IsMessage a) => Proxy a -> TypeRegistry -> TypeRegistry
registerType _ (TypeRegistry m) =
  let name = messageTypeName (Proxy :: Proxy a)
      decoder bs' = case decodeMessage bs' of
        Left e  -> Left e
        Right v -> Right (DynamicMessage (v :: a))
  in TypeRegistry (Map.insert name decoder m)

-- | Look up a decoder in the registry by type name.
lookupType :: Text -> TypeRegistry -> Maybe (ByteString -> Either DecodeError DynamicMessage)
lookupType name (TypeRegistry m) = Map.lookup name m


-- | Unpack an 'Any' using a 'TypeRegistry' for dynamic dispatch.
--
-- @
-- case unpackAnyDynamic registry any of
--   Just (Right (DynamicMessage msg)) -> print msg
--   Just (Left err) -> error ("decode failed: " <> show err)
--   Nothing -> error "unknown type"
-- @
unpackAnyDynamic :: TypeRegistry -> Any -> Maybe (Either DecodeError DynamicMessage)
unpackAnyDynamic reg (Any tu v) =
  let name = typeNameFromUrl tu
  in case lookupType name reg of
    Nothing      -> Nothing
    Just decoder -> Just (decoder v)

instance ProtoToJSON Any where
  protoToJSON (Any tu _) = jsonObject [("@type", JsonString tu)]

instance ProtoFromJSON Any where
  protoFromJSON _ = Right defaultAny
