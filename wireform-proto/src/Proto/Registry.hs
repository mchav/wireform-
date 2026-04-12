{-# LANGUAGE ScopedTypeVariables #-}
-- | Type registry for proto message types, independent of Any.
--
-- This module provides the core 'MessageRegistry' type and 'registerType'
-- function. It has no dependency on @google.protobuf.Any@, so generated
-- proto modules can import it without circular dependencies.
module Proto.Registry
  ( MessageRegistry
  , emptyRegistry
  , registerType
  , lookupDecoder
  , DynamicMessage (..)
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)

import Proto.Decode (MessageDecode, decodeMessage, DecodeError)
import Proto.Message (IsMessage (..))

data DynamicMessage = forall a. (Show a, IsMessage a) => DynamicMessage !a

instance Show DynamicMessage where
  show (DynamicMessage a) = show a

newtype MessageRegistry = MessageRegistry
  (Map Text (ByteString -> Either DecodeError DynamicMessage))

emptyRegistry :: MessageRegistry
emptyRegistry = MessageRegistry Map.empty

registerType :: forall a. (Show a, IsMessage a) => Proxy a -> MessageRegistry -> MessageRegistry
registerType _ (MessageRegistry m) =
  let name = messageTypeName (Proxy :: Proxy a)
      decoder bs' = case decodeMessage bs' of
        Left e  -> Left e
        Right v -> Right (DynamicMessage (v :: a))
  in MessageRegistry (Map.insert name decoder m)

lookupDecoder :: Text -> MessageRegistry -> Maybe (ByteString -> Either DecodeError DynamicMessage)
lookupDecoder name (MessageRegistry m) = Map.lookup name m
