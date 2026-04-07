{-# LANGUAGE BangPatterns #-}
-- | Avro JSON encoding, decoding, and schema serialization.
--
-- Implements the Avro specification's JSON encoding for values and schemas.
module Avro.JSON
  ( avroToJSON
  , avroFromJSON
  , avroSchemaToJSON
  , avroSchemaFromJSON
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import Data.Int (Int32, Int64)
import Data.Scientific (fromFloatDigits, toBoundedInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Avro.Schema (AvroType(..), AvroSchema(..), AvroField(..))
import Avro.Value (AvroValue(..))

-- | Convert an 'AvroValue' to a JSON 'Aeson.Value' according to its schema.
avroToJSON :: AvroType -> AvroValue -> Aeson.Value
avroToJSON (AvroPrimitive s) v = primToJSON s v
avroToJSON AvroRecord{avroRecordFields = fields} (AvRecord vals) =
  Aeson.Object $ KM.fromList
    [ (Key.fromText (avroFieldName f), avroToJSON (avroFieldType f) v)
    | (f, v) <- zip (V.toList fields) vals
    ]
avroToJSON AvroEnum{avroEnumSymbols = syms} (AvEnum idx) =
  Aeson.String (syms V.! idx)
avroToJSON AvroArray{avroArrayItems = itemTy} (AvArray items) =
  Aeson.Array $ V.fromList [avroToJSON itemTy v | v <- items]
avroToJSON AvroMap{avroMapValues = valTy} (AvMap entries) =
  Aeson.Object $ KM.fromList
    [(Key.fromText k, avroToJSON valTy v) | (k, v) <- entries]
avroToJSON AvroUnion{avroUnionBranches = branches} (AvUnion idx val) =
  let branchTy = branches V.! idx
  in case val of
    AvNull -> Aeson.Null
    _      -> Aeson.Object $ KM.singleton
                (Key.fromText (typeName branchTy))
                (avroToJSON branchTy val)
avroToJSON AvroFixed{} (AvFixed bs) = bytesToJSON bs
avroToJSON AvroLogical{avroLogicalBase = base} v = avroToJSON base v
avroToJSON _ _ = error "Avro.JSON: schema/value mismatch"

-- | Parse a JSON 'Aeson.Value' into an 'AvroValue' according to its schema.
avroFromJSON :: AvroType -> Aeson.Value -> Either String AvroValue
avroFromJSON (AvroPrimitive s) v = primFromJSON s v
avroFromJSON AvroRecord{avroRecordFields = fields} (Aeson.Object obj) =
  AvRecord <$> mapM (\f ->
    case KM.lookup (Key.fromText (avroFieldName f)) obj of
      Just v  -> avroFromJSON (avroFieldType f) v
      Nothing -> Left $ "missing field: " ++ T.unpack (avroFieldName f)
    ) (V.toList fields)
avroFromJSON AvroEnum{avroEnumSymbols = syms} (Aeson.String s) =
  case V.findIndex (== s) syms of
    Just idx -> Right (AvEnum idx)
    Nothing  -> Left $ "unknown enum symbol: " ++ T.unpack s
avroFromJSON AvroArray{avroArrayItems = itemTy} (Aeson.Array arr) =
  AvArray <$> mapM (avroFromJSON itemTy) (V.toList arr)
avroFromJSON AvroMap{avroMapValues = valTy} (Aeson.Object obj) =
  AvMap <$> mapM (\(k, v) -> (Key.toText k,) <$> avroFromJSON valTy v) (KM.toList obj)
avroFromJSON AvroUnion{avroUnionBranches = branches} v = unionFromJSON branches v
avroFromJSON AvroFixed{avroFixedSize = sz} (Aeson.String s) =
  let bs = textToBytes s
  in if BS.length bs == sz
     then Right (AvFixed bs)
     else Left $ "fixed size mismatch: expected " ++ show sz ++ " got " ++ show (BS.length bs)
avroFromJSON AvroLogical{avroLogicalBase = base} v = avroFromJSON base v
avroFromJSON _ _ = Left "Avro.JSON: type/value mismatch"

-- | Serialize an Avro schema to its canonical JSON representation.
avroSchemaToJSON :: AvroType -> Aeson.Value
avroSchemaToJSON (AvroPrimitive s) = Aeson.String (schemaName s)
avroSchemaToJSON ty@AvroRecord{} =
  Aeson.Object $ KM.fromList
    [ ("type", Aeson.String "record")
    , ("name", Aeson.String (avroRecordName ty))
    , ("fields", Aeson.Array $ V.map fieldToJSON (avroRecordFields ty))
    ]
avroSchemaToJSON ty@AvroEnum{} =
  Aeson.Object $ KM.fromList $
    [ ("type", Aeson.String "enum")
    , ("name", Aeson.String (avroEnumName ty))
    , ("symbols", Aeson.Array $ V.map Aeson.String (avroEnumSymbols ty))
    ] ++ maybe [] (\d -> [("default", Aeson.String d)]) (avroEnumDefault ty)
avroSchemaToJSON AvroArray{avroArrayItems = items} =
  Aeson.Object $ KM.fromList
    [ ("type", Aeson.String "array")
    , ("items", avroSchemaToJSON items)
    ]
avroSchemaToJSON AvroMap{avroMapValues = vals} =
  Aeson.Object $ KM.fromList
    [ ("type", Aeson.String "map")
    , ("values", avroSchemaToJSON vals)
    ]
avroSchemaToJSON AvroUnion{avroUnionBranches = branches} =
  Aeson.Array $ V.map avroSchemaToJSON branches
avroSchemaToJSON ty@AvroFixed{} =
  Aeson.Object $ KM.fromList
    [ ("type", Aeson.String "fixed")
    , ("name", Aeson.String (avroFixedName ty))
    , ("size", Aeson.Number (fromIntegral (avroFixedSize ty)))
    ]
avroSchemaToJSON AvroLogical{avroLogicalBase = base} = avroSchemaToJSON base

-- | Parse a JSON value as an Avro schema.
avroSchemaFromJSON :: Aeson.Value -> Either String AvroType
avroSchemaFromJSON (Aeson.String s) = case s of
  "null"    -> Right (AvroPrimitive AvroNull)
  "boolean" -> Right (AvroPrimitive AvroBool)
  "int"     -> Right (AvroPrimitive AvroInt)
  "long"    -> Right (AvroPrimitive AvroLong)
  "float"   -> Right (AvroPrimitive AvroFloat)
  "double"  -> Right (AvroPrimitive AvroDouble)
  "bytes"   -> Right (AvroPrimitive AvroBytes)
  "string"  -> Right (AvroPrimitive AvroString)
  other     -> Right (AvroPrimitive (AvroSchemaRef other))
avroSchemaFromJSON (Aeson.Array arr) =
  AvroUnion <$> V.mapM avroSchemaFromJSON arr
avroSchemaFromJSON (Aeson.Object obj) = do
  typStr <- case KM.lookup "type" obj of
    Just (Aeson.String t) -> Right t
    _ -> Left "schema object missing 'type' string field"
  case typStr of
    "record" -> do
      name <- requireString "name" obj
      fieldsArr <- case KM.lookup "fields" obj of
        Just (Aeson.Array arr) -> Right arr
        _ -> Left "record missing 'fields' array"
      fields <- V.mapM fieldFromJSON fieldsArr
      Right AvroRecord
        { avroRecordName      = name
        , avroRecordNamespace  = optString "namespace" obj
        , avroRecordDoc        = optString "doc" obj
        , avroRecordAliases    = V.empty
        , avroRecordFields     = fields
        }
    "enum" -> do
      name <- requireString "name" obj
      symsArr <- case KM.lookup "symbols" obj of
        Just (Aeson.Array arr) -> Right arr
        _ -> Left "enum missing 'symbols' array"
      syms <- V.mapM (\case
        Aeson.String s -> Right s
        _ -> Left "enum symbol must be a string") symsArr
      Right AvroEnum
        { avroEnumName      = name
        , avroEnumNamespace  = optString "namespace" obj
        , avroEnumDoc        = optString "doc" obj
        , avroEnumAliases    = V.empty
        , avroEnumSymbols    = syms
        , avroEnumDefault    = optString "default" obj
        }
    "array" -> do
      itemsV <- case KM.lookup "items" obj of
        Just v  -> avroSchemaFromJSON v
        Nothing -> Left "array missing 'items'"
      Right AvroArray { avroArrayItems = itemsV }
    "map" -> do
      valsV <- case KM.lookup "values" obj of
        Just v  -> avroSchemaFromJSON v
        Nothing -> Left "map missing 'values'"
      Right AvroMap { avroMapValues = valsV }
    "fixed" -> do
      name <- requireString "name" obj
      sz <- case KM.lookup "size" obj of
        Just (Aeson.Number n) -> case toBoundedInteger n of
          Just i  -> Right (i :: Int)
          Nothing -> Left "fixed: size out of range"
        _ -> Left "fixed missing 'size'"
      Right AvroFixed
        { avroFixedName      = name
        , avroFixedNamespace  = optString "namespace" obj
        , avroFixedSize       = sz
        , avroFixedAliases    = V.empty
        }
    other -> Left $ "unknown schema type: " ++ T.unpack other
avroSchemaFromJSON _ = Left "invalid schema JSON: expected string, array, or object"

-- ============================================================
-- Internal helpers — value encoding
-- ============================================================

primToJSON :: AvroSchema -> AvroValue -> Aeson.Value
primToJSON AvroNull   AvNull       = Aeson.Null
primToJSON AvroBool   (AvBool b)   = Aeson.Bool b
primToJSON AvroInt    (AvInt n)    = Aeson.Number (fromIntegral n)
primToJSON AvroLong   (AvLong n)
  | n > 9007199254740992 || n < -9007199254740992 =
      Aeson.String (T.pack (show n))
  | otherwise = Aeson.Number (fromIntegral n)
primToJSON AvroFloat  (AvFloat f)  = floatToJSON f
primToJSON AvroDouble (AvDouble d) = doubleToJSON d
primToJSON AvroBytes  (AvBytes bs) = bytesToJSON bs
primToJSON AvroString (AvString t) = Aeson.String t
primToJSON _ _ = error "Avro.JSON: primitive schema/value mismatch"

primFromJSON :: AvroSchema -> Aeson.Value -> Either String AvroValue
primFromJSON AvroNull Aeson.Null = Right AvNull
primFromJSON AvroBool (Aeson.Bool b) = Right (AvBool b)
primFromJSON AvroInt (Aeson.Number n) =
  case toBoundedInteger n :: Maybe Int32 of
    Just i  -> Right (AvInt i)
    Nothing -> Left "int: value out of Int32 range"
primFromJSON AvroLong (Aeson.Number n) =
  case toBoundedInteger n :: Maybe Int64 of
    Just i  -> Right (AvLong i)
    Nothing -> Left "long: value out of Int64 range"
primFromJSON AvroLong (Aeson.String s) =
  case reads (T.unpack s) of
    [(i, "")] -> Right (AvLong i)
    _         -> Left "long: invalid string encoding"
primFromJSON AvroFloat (Aeson.Number n) = Right (AvFloat (toRealFloat n))
primFromJSON AvroFloat (Aeson.String s) = case s of
  "NaN"       -> Right (AvFloat (0 / 0))
  "Infinity"  -> Right (AvFloat (1 / 0))
  "-Infinity" -> Right (AvFloat (negate (1 / 0)))
  _           -> Left "float: unrecognized string value"
primFromJSON AvroDouble (Aeson.Number n) = Right (AvDouble (toRealFloat n))
primFromJSON AvroDouble (Aeson.String s) = case s of
  "NaN"       -> Right (AvDouble (0 / 0))
  "Infinity"  -> Right (AvDouble (1 / 0))
  "-Infinity" -> Right (AvDouble (negate (1 / 0)))
  _           -> Left "double: unrecognized string value"
primFromJSON AvroBytes (Aeson.String s) = Right (AvBytes (textToBytes s))
primFromJSON AvroString (Aeson.String s) = Right (AvString s)
primFromJSON _ _ = Left "primitive type/JSON mismatch"

floatToJSON :: Float -> Aeson.Value
floatToJSON !f
  | isNaN f               = Aeson.String "NaN"
  | isInfinite f && f > 0 = Aeson.String "Infinity"
  | isInfinite f          = Aeson.String "-Infinity"
  | otherwise             = Aeson.Number (fromFloatDigits f)

doubleToJSON :: Double -> Aeson.Value
doubleToJSON !d
  | isNaN d               = Aeson.String "NaN"
  | isInfinite d && d > 0 = Aeson.String "Infinity"
  | isInfinite d          = Aeson.String "-Infinity"
  | otherwise             = Aeson.Number (fromFloatDigits d)

bytesToJSON :: BS.ByteString -> Aeson.Value
bytesToJSON bs = Aeson.String $ T.pack [chr (fromIntegral b) | b <- BS.unpack bs]

textToBytes :: Text -> BS.ByteString
textToBytes = BS.pack . map (fromIntegral . ord) . T.unpack

-- ============================================================
-- Internal helpers — union encoding
-- ============================================================

unionFromJSON :: V.Vector AvroType -> Aeson.Value -> Either String AvroValue
unionFromJSON branches Aeson.Null =
  case V.findIndex isNullType branches of
    Just idx -> Right (AvUnion idx AvNull)
    Nothing  -> Left "union: null is not a branch of this union"
unionFromJSON branches (Aeson.Object obj) =
  case KM.toList obj of
    [(k, v)] ->
      let name = Key.toText k
      in case V.findIndex (\t -> typeName t == name) branches of
           Just idx -> do
             val <- avroFromJSON (branches V.! idx) v
             Right (AvUnion idx val)
           Nothing -> Left $ "union: no branch named " ++ T.unpack name
    _ -> Left "union: expected single-key object or null"
unionFromJSON _ _ = Left "union: expected null or single-key object"

isNullType :: AvroType -> Bool
isNullType (AvroPrimitive AvroNull) = True
isNullType _                        = False

typeName :: AvroType -> Text
typeName (AvroPrimitive s)                  = schemaName s
typeName AvroRecord{avroRecordName = n}     = n
typeName AvroEnum{avroEnumName = n}         = n
typeName AvroArray{}                        = "array"
typeName AvroMap{}                          = "map"
typeName AvroFixed{avroFixedName = n}       = n
typeName AvroUnion{}                        = "union"
typeName AvroLogical{avroLogicalBase = base} = typeName base

-- ============================================================
-- Internal helpers — schema encoding
-- ============================================================

schemaName :: AvroSchema -> Text
schemaName AvroNull          = "null"
schemaName AvroBool          = "boolean"
schemaName AvroInt           = "int"
schemaName AvroLong          = "long"
schemaName AvroFloat         = "float"
schemaName AvroDouble        = "double"
schemaName AvroBytes         = "bytes"
schemaName AvroString        = "string"
schemaName (AvroSchemaRef t) = t

fieldToJSON :: AvroField -> Aeson.Value
fieldToJSON f = Aeson.Object $ KM.fromList $
  [ ("name", Aeson.String (avroFieldName f))
  , ("type", avroSchemaToJSON (avroFieldType f))
  ] ++ dfltPair
  where
    dfltPair = case avroFieldDefault f of
      Just s  -> [("default", defaultSchemaToJSON s)]
      Nothing -> []

defaultSchemaToJSON :: AvroSchema -> Aeson.Value
defaultSchemaToJSON AvroNull   = Aeson.Null
defaultSchemaToJSON AvroBool   = Aeson.Bool False
defaultSchemaToJSON AvroInt    = Aeson.Number 0
defaultSchemaToJSON AvroLong   = Aeson.Number 0
defaultSchemaToJSON AvroFloat  = Aeson.Number 0
defaultSchemaToJSON AvroDouble = Aeson.Number 0
defaultSchemaToJSON AvroBytes  = Aeson.String ""
defaultSchemaToJSON AvroString = Aeson.String ""
defaultSchemaToJSON _          = Aeson.Null

fieldFromJSON :: Aeson.Value -> Either String AvroField
fieldFromJSON (Aeson.Object obj) = do
  name <- requireString "name" obj
  ty <- case KM.lookup "type" obj of
    Just v  -> avroSchemaFromJSON v
    Nothing -> Left "field missing 'type'"
  let dflt = case KM.lookup "default" obj of
        Just _  -> defaultForType ty
        Nothing -> Nothing
  Right AvroField
    { avroFieldName    = name
    , avroFieldType    = ty
    , avroFieldDefault = dflt
    , avroFieldOrder   = Nothing
    , avroFieldAliases = V.empty
    , avroFieldDoc     = Nothing
    }
fieldFromJSON _ = Left "field must be a JSON object"

defaultForType :: AvroType -> Maybe AvroSchema
defaultForType (AvroPrimitive s) = Just s
defaultForType _                 = Just AvroNull

requireString :: Text -> KM.KeyMap Aeson.Value -> Either String Text
requireString k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.String s) -> Right s
  _                     -> Left $ "missing or non-string field: " ++ T.unpack k

optString :: Text -> KM.KeyMap Aeson.Value -> Maybe Text
optString k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.String s) -> Just s
  _                     -> Nothing
