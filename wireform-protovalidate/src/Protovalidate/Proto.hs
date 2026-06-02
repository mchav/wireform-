{-# LANGUAGE OverloadedStrings #-}

-- | Bridge from @wireform-proto@ dynamic messages to the CEL value model used
-- by the validator.
--
-- A 'Proto.Dynamic.DynamicMessage' stores fields by number and (being
-- schemaless) cannot by itself tell @int64@ from @uint64@ or name its fields.
-- Callers therefore supply a 'MessageSchema' describing each field's number,
-- name, and shape; this module walks the message and produces a CEL 'VMap'
-- suitable for "Protovalidate.Eval.validate".
module Protovalidate.Proto
  ( MessageSchema
  , FieldSchema (..)
  , FieldShape (..)
  , dynamicMessageToCel
  , dynamicValueToCel
  ) where

import Data.Bits (shiftR, xor, (.&.))
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import qualified Data.Map.Strict as Map
import CEL.Value (Value (..), celMapFromList)
import Proto.Dynamic (DynamicMessage (..), DynamicValue (..))
import Protovalidate.Rules (RuleKind (..))

-- | A description of a message's fields, used to name and type-interpret a
-- dynamic message.
type MessageSchema = [FieldSchema]

-- | One field's number, name, and shape.
data FieldSchema = FieldSchema
  { fsNumber :: !Int
  , fsName :: !Text
  , fsShape :: !FieldShape
  }
  deriving stock (Show)

-- | The shape of a field's value.
data FieldShape
  = ScalarField !RuleKind
  -- ^ A scalar; the 'RuleKind' disambiguates signed/unsigned/bool/enum.
  | MessageField !MessageSchema
  -- ^ A nested message with its own schema.
  | RepeatedField !FieldShape
  -- ^ A repeated field of the given element shape.
  deriving stock (Show)

-- | Convert a dynamic message into a CEL 'VMap' keyed by field name, using the
-- supplied schema. Fields not present in the schema are ignored.
dynamicMessageToCel :: MessageSchema -> DynamicMessage -> Value
dynamicMessageToCel schema msg =
  VMap (celMapFromList (mapMaybe convertField schema))
  where
    convertField fsch =
      case Map.lookup (fsNumber fsch) (dynFields msg) of
        Nothing -> Nothing
        Just dv -> Just (VString (fsName fsch), convertShaped (fsShape fsch) dv)

convertShaped :: FieldShape -> DynamicValue -> Value
convertShaped shape dv = case shape of
  ScalarField k -> coerceScalar k dv
  MessageField sub -> case dv of
    DynMessage m -> dynamicMessageToCel sub m
    _ -> dynamicValueToCel dv
  RepeatedField elemShape -> case dv of
    DynRepeated xs -> VList (V.fromList (map (convertShaped elemShape) xs))
    _ -> convertShaped elemShape dv

-- | Interpret a scalar dynamic value according to a 'RuleKind'.
coerceScalar :: RuleKind -> DynamicValue -> Value
coerceScalar k dv = case dv of
  DynBool b -> VBool b
  DynString s -> VString s
  DynBytes b -> VBytes b
  DynFloat f -> VDouble (realToFrac f)
  DynDouble d -> VDouble d
  DynEnum e -> VInt (fromIntegral e)
  DynSVarint i -> VInt i
  DynVarint w -> fromVarint k w
  DynFixed32 w -> fromFixed k (fromIntegral w)
  DynFixed64 w -> fromFixed k w
  DynRepeated xs -> VList (V.fromList (map (coerceScalar k) xs))
  DynMessage m -> dynamicValueToCel (DynMessage m)
  DynMap m -> dynamicValueToCel (DynMap m)
  where
    fromVarint kind w = case kind of
      KBool -> VBool (w /= 0)
      KUint32 -> VUInt w
      KUint64 -> VUInt w
      KSint32 -> VInt (zigzag w)
      KSint64 -> VInt (zigzag w)
      _ -> VInt (fromIntegral w :: Int64) -- int32/int64/enum: two's-complement bits
    fromFixed kind w = case kind of
      KUint32 -> VUInt w
      KUint64 -> VUInt w
      KFixed32 -> VUInt w
      KFixed64 -> VUInt w
      KFloat -> VDouble (realToFrac (wordToFloat w))
      KDouble -> VDouble (wordToDouble w)
      _ -> VInt (fromIntegral w :: Int64)

-- | A best-effort, schema-less conversion of a dynamic value. Varints become
-- unsigned, nested messages become field-number-keyed maps. Prefer
-- 'dynamicMessageToCel' with a schema where field names / signedness matter.
dynamicValueToCel :: DynamicValue -> Value
dynamicValueToCel = \case
  DynVarint w -> VUInt w
  DynSVarint i -> VInt i
  DynFixed32 w -> VUInt (fromIntegral w)
  DynFixed64 w -> VUInt w
  DynFloat f -> VDouble (realToFrac f)
  DynDouble d -> VDouble d
  DynBool b -> VBool b
  DynString s -> VString s
  DynBytes b -> VBytes b
  DynEnum e -> VInt (fromIntegral e)
  DynRepeated xs -> VList (V.fromList (map dynamicValueToCel xs))
  DynMessage m ->
    VMap (celMapFromList [(VInt (fromIntegral n), dynamicValueToCel v) | (n, v) <- Map.toList (dynFields m)])
  DynMap m ->
    VMap (celMapFromList [(dynamicValueToCel kk, dynamicValueToCel vv) | (kk, vv) <- Map.toList m])

zigzag :: Word64 -> Int64
zigzag w = fromIntegral ((w `shiftR` 1) `xor` negate (w .&. 1))

-- Reinterpret the bits of a 32/64-bit word as IEEE-754 float/double.
wordToFloat :: Word64 -> Float
wordToFloat w = castWord32ToFloat (fromIntegral w)

wordToDouble :: Word64 -> Double
wordToDouble = castWord64ToDouble
