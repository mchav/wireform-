{-# LANGUAGE BangPatterns #-}
module Proto.Google.Protobuf.Struct
  ( Struct (..)
  , defaultStruct
  , Value (..)
  , defaultValue
  , ValueKind (..)
  , NullValue (..)
  , ListValue (..)
  , defaultListValue
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.JSON
import Proto.Decode
import Proto.Wire (Tag (..))

data Struct = Struct
  { structFields :: !(Map Text Value)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultStruct :: Struct
defaultStruct = Struct Map.empty

data Value = Value
  { valueKind :: !(Maybe ValueKind)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultValue :: Value
defaultValue = Value Nothing

data ValueKind
  = NullKind    !NullValue
  | NumberKind  {-# UNPACK #-} !Double
  | StringKind  !Text
  | BoolKind    !Bool
  | StructKind  !Struct
  | ListKind    !ListValue
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data NullValue = NullValueNull
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass NFData

data ListValue = ListValue
  { listValues :: !(V.Vector Value)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultListValue :: ListValue
defaultListValue = ListValue V.empty

instance MessageEncode Struct where
  buildMessage (Struct fs) =
    Map.foldlWithKey' (\acc k v ->
      let entry = messageToByteString (encodeFieldString 1 k <> encodeFieldMessage 2 v)
      in acc <> encodeFieldBytes 1 entry) mempty fs
  {-# INLINE buildMessage #-}

instance MessageDecode Struct where
  messageDecoder = loop Map.empty
    where
      loop !fs = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Struct fs)
          Just (Tag 1 _) -> do
            entryBytes <- getLengthDelimited
            case runDecoder decodeMapEntry entryBytes of
              Left _ -> loop fs
              Right (k, v) -> loop (Map.insert k v fs)
          Just (Tag _ wt) -> skipField wt >> loop fs
      decodeMapEntry = do
        let loopEntry !k !v = do
              mt <- getTagOr
              case mt of
                Nothing -> pure (k, v)
                Just (Tag 1 _) -> decodeFieldString >>= \x -> loopEntry x v
                Just (Tag 2 _) -> decodeFieldMessage >>= \x -> loopEntry k x
                Just (Tag _ wt) -> skipField wt >> loopEntry k v
        loopEntry "" defaultValue
  {-# INLINE messageDecoder #-}

instance MessageEncode Value where
  buildMessage (Value mk) = case mk of
    Nothing -> mempty
    Just (NullKind _)   -> encodeFieldVarint 1 0
    Just (NumberKind d)  -> encodeFieldDouble 2 d
    Just (StringKind s)  -> encodeFieldString 3 s
    Just (BoolKind b)    -> encodeFieldBool 4 b
    Just (StructKind st) -> encodeFieldMessage 5 st
    Just (ListKind lv)   -> encodeFieldMessage 6 lv
  {-# INLINE buildMessage #-}

instance MessageDecode Value where
  messageDecoder = loop Nothing
    where
      loop !mk = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Value mk)
          Just (Tag 1 _) -> getVarint >> loop (Just (NullKind NullValueNull))
          Just (Tag 2 _) -> do d <- getDouble; loop (Just (NumberKind d))
          Just (Tag 3 _) -> do s <- decodeFieldString; loop (Just (StringKind s))
          Just (Tag 4 _) -> do v <- getVarint; loop (Just (BoolKind (v /= 0)))
          Just (Tag 5 _) -> do s <- decodeFieldMessage; loop (Just (StructKind s))
          Just (Tag 6 _) -> do l <- decodeFieldMessage; loop (Just (ListKind l))
          Just (Tag _ wt) -> skipField wt >> loop mk
  {-# INLINE messageDecoder #-}

instance MessageEncode ListValue where
  buildMessage (ListValue vs) =
    V.foldl' (\acc v -> acc <> encodeFieldMessage 1 v) mempty vs
  {-# INLINE buildMessage #-}

instance MessageDecode ListValue where
  messageDecoder = loop V.empty
    where
      loop !vs = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (ListValue vs)
          Just (Tag 1 _) -> do
            v <- decodeFieldMessage
            loop (V.snoc vs v)
          Just (Tag _ wt) -> skipField wt >> loop vs
  {-# INLINE messageDecoder #-}

instance ProtoToJSON Struct where
  protoToJSON _ = JsonNull
instance ProtoFromJSON Struct where
  protoFromJSON _ = Right defaultStruct
instance ProtoToJSON Value where
  protoToJSON _ = JsonNull
instance ProtoFromJSON Value where
  protoFromJSON _ = Right defaultValue
instance ProtoToJSON ListValue where
  protoToJSON _ = JsonNull
instance ProtoFromJSON ListValue where
  protoFromJSON _ = Right defaultListValue
