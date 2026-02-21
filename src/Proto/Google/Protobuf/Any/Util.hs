{-# LANGUAGE ScopedTypeVariables #-}
-- | Utility functions for @google.protobuf.Any@ — type-safe packing,
-- unpacking, and a runtime 'TypeRegistry' for dynamic dispatch.
--
-- These are higher-level operations that sit on top of the generated
-- 'Any' data type.
module Proto.Google.Protobuf.Any.Util
  ( -- * Type-safe packing / unpacking
    packAny
  , packAnyWithPrefix
  , unpackAny
  , isMessageType

    -- * Type registry for dynamic dispatch
  , AnyTypeRegistry
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T

import Proto.Encode (MessageEncode, encodeMessage)
import Proto.Decode (MessageDecode, decodeMessage, DecodeError)
import Proto.Message (IsMessage (..))
import Proto.Google.Protobuf.Any (Any(..))

typeUrlPrefix :: Text
typeUrlPrefix = "type.googleapis.com/"

typeUrlOf :: forall a. IsMessage a => Proxy a -> Text
typeUrlOf p = typeUrlPrefix <> messageTypeName p

packAny :: forall a. IsMessage a => a -> Any
packAny msg = Any
  { anyTypeurl = typeUrlOf (Proxy :: Proxy a)
  , anyValue   = encodeMessage msg
  }

packAnyWithPrefix :: forall a. IsMessage a => Text -> a -> Any
packAnyWithPrefix prefix msg = Any
  { anyTypeurl = prefix <> messageTypeName (Proxy :: Proxy a)
  , anyValue   = encodeMessage msg
  }

unpackAny :: forall a. IsMessage a => Any -> Maybe (Either DecodeError a)
unpackAny any'
  | isTypeMatch (anyTypeurl any') (Proxy :: Proxy a) = Just (decodeMessage (anyValue any'))
  | otherwise = Nothing

isMessageType :: forall a. IsMessage a => Proxy a -> Any -> Bool
isMessageType p any' = isTypeMatch (anyTypeurl any') p

isTypeMatch :: forall a. IsMessage a => Text -> Proxy a -> Bool
isTypeMatch tu p =
  let expected = messageTypeName p
  in typeNameFromUrl tu == expected

typeNameFromUrl :: Text -> Text
typeNameFromUrl tu = case T.breakOnEnd "/" tu of
  ("", name) -> name
  (_, name)  -> name

data DynamicMessage = forall a. (Show a, IsMessage a) => DynamicMessage !a

instance Show DynamicMessage where
  show (DynamicMessage a) = show a

newtype AnyTypeRegistry = AnyTypeRegistry
  (Map Text (ByteString -> Either DecodeError DynamicMessage))

emptyRegistry :: AnyTypeRegistry
emptyRegistry = AnyTypeRegistry Map.empty

registerType :: forall a. (Show a, IsMessage a) => Proxy a -> AnyTypeRegistry -> AnyTypeRegistry
registerType _ (AnyTypeRegistry m) =
  let name = messageTypeName (Proxy :: Proxy a)
      decoder bs' = case decodeMessage bs' of
        Left e  -> Left e
        Right v -> Right (DynamicMessage (v :: a))
  in AnyTypeRegistry (Map.insert name decoder m)

lookupType :: Text -> AnyTypeRegistry -> Maybe (ByteString -> Either DecodeError DynamicMessage)
lookupType name (AnyTypeRegistry m) = Map.lookup name m

unpackAnyDynamic :: AnyTypeRegistry -> Any -> Maybe (Either DecodeError DynamicMessage)
unpackAnyDynamic reg any' =
  let name = typeNameFromUrl (anyTypeurl any')
  in case lookupType name reg of
    Nothing      -> Nothing
    Just decoder -> Just (decoder (anyValue any'))
