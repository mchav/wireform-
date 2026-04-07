-- | Thrift JSON protocol encoding/decoding.
--
-- Provides two JSON representations:
--
-- * /Simple JSON/ ('thriftToJSON' \/ 'thriftFromJSON'): primitives as direct
--   JSON values, structs as objects keyed by field ID, containers as arrays.
--   Decoding is template-guided (a sample 'ThriftValue' provides the type
--   structure).
--
-- * /Typed JSON/ ('thriftToTypedJSON' \/ 'thriftFromTypedJSON'): each value is
--   wrapped in a type-tagged object (TJSONProtocol-like), enabling decoding
--   without a separate schema.
module Thrift.JSON
  ( -- * Simple JSON
    thriftToJSON
  , thriftFromJSON
    -- * Typed JSON (TJSONProtocol-like)
  , thriftToTypedJSON
  , thriftFromTypedJSON
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import Data.Int (Int8, Int16, Int64)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Scientific as Sci
import qualified Data.Vector as V

import Thrift.Value
import Thrift.Wire (ThriftType(..))

--------------------------------------------------------------------------------
-- Simple JSON encoding
--------------------------------------------------------------------------------

-- | Encode a 'ThriftValue' as simple JSON.
--
-- * Primitives map to their natural JSON counterparts (i64 → string for
--   precision).
-- * Struct → object keyed by field-ID strings.
-- * List\/Set → JSON array.
-- * Map → array of @[key, value]@ pairs.
-- * Binary → base64 string, UUID → hex string.
thriftToJSON :: ThriftValue -> Aeson.Value
thriftToJSON = \case
  TVBool b    -> Aeson.Bool b
  TVByte v    -> Aeson.Number (fromIntegral v)
  TVI16 v     -> Aeson.Number (fromIntegral v)
  TVI32 v     -> Aeson.Number (fromIntegral v)
  TVI64 v     -> Aeson.String (Text.pack (show v))
  TVDouble d  -> Aeson.Number (Sci.fromFloatDigits d)
  TVString t  -> Aeson.String t
  TVBinary bs -> Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  TVUUID bs   -> Aeson.String (TE.decodeUtf8 (Base16.encode bs))
  TVStruct fields ->
    Aeson.object [ (Key.fromText (Text.pack (show fid)), thriftToJSON v)
                 | (fid, v) <- fields ]
  TVList _ elems ->
    Aeson.Array (V.fromList (map thriftToJSON elems))
  TVSet _ elems ->
    Aeson.Array (V.fromList (map thriftToJSON elems))
  TVMap _ _ entries ->
    Aeson.Array (V.fromList
      [ Aeson.Array (V.fromList [thriftToJSON k, thriftToJSON v])
      | (k, v) <- entries ])

--------------------------------------------------------------------------------
-- Simple JSON decoding (template-guided)
--------------------------------------------------------------------------------

-- | Decode a JSON value into a 'ThriftValue', using a template value that
-- supplies the expected type structure.  The template's /values/ are ignored;
-- only its shape (constructor choices, field IDs, element types) matters.
thriftFromJSON :: ThriftValue -> Aeson.Value -> Either String ThriftValue
thriftFromJSON template json = case template of
  TVBool _ -> case json of
    Aeson.Bool b -> Right (TVBool b)
    _            -> Left "Expected JSON bool"

  TVByte _ -> case json of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TVByte (v :: Int8))
      Nothing -> Left "Number out of range for byte"
    _ -> Left "Expected JSON number for byte"

  TVI16 _ -> case json of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TVI16 v)
      Nothing -> Left "Number out of range for i16"
    _ -> Left "Expected JSON number for i16"

  TVI32 _ -> case json of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TVI32 v)
      Nothing -> Left "Number out of range for i32"
    _ -> Left "Expected JSON number for i32"

  TVI64 _ -> case json of
    Aeson.String s -> case (TR.signed TR.decimal s :: Either String (Int64, Text)) of
      Right (v, "") -> Right (TVI64 v)
      _             -> Left ("Invalid i64 string: " <> Text.unpack s)
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TVI64 (v :: Int64))
      Nothing -> Left "Number out of range for i64"
    _ -> Left "Expected JSON string or number for i64"

  TVDouble _ -> case json of
    Aeson.Number n -> Right (TVDouble (Sci.toRealFloat n))
    _              -> Left "Expected JSON number for double"

  TVString _ -> case json of
    Aeson.String t -> Right (TVString t)
    _              -> Left "Expected JSON string"

  TVBinary _ -> case json of
    Aeson.String s -> case Base64.decode (TE.encodeUtf8 s) of
      Right bs -> Right (TVBinary bs)
      Left err -> Left ("Invalid base64: " <> err)
    _ -> Left "Expected JSON string (base64) for binary"

  TVUUID _ -> case json of
    Aeson.String s -> case Base16.decode (TE.encodeUtf8 s) of
      Right bs | BS.length bs == 16 -> Right (TVUUID bs)
               | otherwise -> Left "UUID hex must decode to 16 bytes"
      Left err -> Left ("Invalid hex for UUID: " <> err)
    _ -> Left "Expected JSON string (hex) for UUID"

  TVStruct templateFields -> case json of
    Aeson.Object obj -> do
      let tmplMap = [ (Text.pack (show fid), tmpl)
                    | (fid, tmpl) <- templateFields ]
      fields <- mapM (\(key, val) -> do
          let kt = Key.toText key
          fid <- parseFieldId kt
          case lookup kt tmplMap of
            Just tmpl -> do
              v <- thriftFromJSON tmpl val
              Right (fid, v)
            Nothing -> Left ("Unknown field: " <> Text.unpack kt)
        ) (KM.toList obj)
      Right (TVStruct (sortBy (comparing fst) fields))
    _ -> Left "Expected JSON object for struct"

  TVList et tmplElems -> case json of
    Aeson.Array arr -> do
      let etmpl = case tmplElems of
            (e:_) -> e
            []    -> defaultForWireType et
      elems <- mapM (thriftFromJSON etmpl) (V.toList arr)
      Right (TVList et elems)
    _ -> Left "Expected JSON array for list"

  TVSet et tmplElems -> case json of
    Aeson.Array arr -> do
      let etmpl = case tmplElems of
            (e:_) -> e
            []    -> defaultForWireType et
      elems <- mapM (thriftFromJSON etmpl) (V.toList arr)
      Right (TVSet et elems)
    _ -> Left "Expected JSON array for set"

  TVMap kt vt tmplEntries -> case json of
    Aeson.Array arr -> do
      let (kTmpl, vTmpl) = case tmplEntries of
            ((k, v):_) -> (k, v)
            []         -> (defaultForWireType kt, defaultForWireType vt)
      entries <- mapM (\pairJson -> case pairJson of
          Aeson.Array pairArr
            | V.length pairArr == 2 -> do
                k <- thriftFromJSON kTmpl (pairArr V.! 0)
                v <- thriftFromJSON vTmpl (pairArr V.! 1)
                Right (k, v)
          _ -> Left "Expected [key, value] pair in map"
        ) (V.toList arr)
      Right (TVMap kt vt entries)
    _ -> Left "Expected JSON array of pairs for map"

--------------------------------------------------------------------------------
-- Typed JSON encoding (TJSONProtocol-like)
--------------------------------------------------------------------------------

-- | Encode a 'ThriftValue' as typed JSON (TJSONProtocol format).
--
-- * Struct → object keyed by field-ID strings; each field value is wrapped in
--   a single-key object whose key is the type abbreviation:
--   @{\"1\": {\"tf\": true}, \"2\": {\"i32\": 42}}@.
-- * List\/Set → @[\"elemType\", count, elem1, …]@ inside a @{\"lst\": …}@ /
--   @{\"set\": …}@ wrapper.
-- * Map → @[\"keyType\", \"valType\", count, {k1: v1, …}]@ inside a
--   @{\"map\": …}@ wrapper.
--
-- Type abbreviations: @tf@ bool, @i8@ byte, @i16@, @i32@, @i64@, @dbl@
-- double, @str@ string, @bin@ binary, @rec@ struct, @lst@ list, @set@, @map@,
-- @uuid@.
thriftToTypedJSON :: ThriftValue -> Aeson.Value
thriftToTypedJSON = \case
  TVStruct fields -> encodeTypedStruct fields
  v               -> wrapTyped v

encodeTypedStruct :: [(Int16, ThriftValue)] -> Aeson.Value
encodeTypedStruct fields =
  Aeson.object [ (Key.fromText (Text.pack (show fid)), wrapTyped v)
               | (fid, v) <- fields ]

wrapTyped :: ThriftValue -> Aeson.Value
wrapTyped v = Aeson.object [(Key.fromText (typeAbbrev v), rawTyped v)]

rawTyped :: ThriftValue -> Aeson.Value
rawTyped = \case
  TVBool b    -> Aeson.Bool b
  TVByte v    -> Aeson.Number (fromIntegral v)
  TVI16 v     -> Aeson.Number (fromIntegral v)
  TVI32 v     -> Aeson.Number (fromIntegral v)
  TVI64 v     -> Aeson.String (Text.pack (show v))
  TVDouble d  -> Aeson.Number (Sci.fromFloatDigits d)
  TVString t  -> Aeson.String t
  TVBinary bs -> Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  TVUUID bs   -> Aeson.String (TE.decodeUtf8 (Base16.encode bs))
  TVStruct fields -> encodeTypedStruct fields
  TVList et elems ->
    let etTag = containerElemTag et elems
    in Aeson.Array (V.fromList
        ( Aeson.String etTag
        : Aeson.Number (fromIntegral (length elems))
        : map rawTyped elems ))
  TVSet et elems ->
    let etTag = containerElemTag et elems
    in Aeson.Array (V.fromList
        ( Aeson.String etTag
        : Aeson.Number (fromIntegral (length elems))
        : map rawTyped elems ))
  TVMap kt vt entries ->
    let ktTag = case entries of { ((k,_):_) -> typeAbbrev k; _ -> wireTypeTag kt }
        vtTag = case entries of { ((_,v):_) -> typeAbbrev v; _ -> wireTypeTag vt }
    in Aeson.Array (V.fromList
        [ Aeson.String ktTag
        , Aeson.String vtTag
        , Aeson.Number (fromIntegral (length entries))
        , Aeson.object [ (Key.fromText (stringifyKey k), rawTyped v)
                        | (k, v) <- entries ]
        ])

--------------------------------------------------------------------------------
-- Typed JSON decoding
--------------------------------------------------------------------------------

-- | Decode a typed JSON value (TJSONProtocol format) into a 'ThriftValue'.
--
-- Structs are recognised as objects whose keys are numeric field-ID strings.
-- Single-key objects whose key is a type abbreviation are decoded as the
-- corresponding wrapped value.  An empty object is decoded as an empty struct.
thriftFromTypedJSON :: Aeson.Value -> Either String ThriftValue
thriftFromTypedJSON json = case json of
  Aeson.Object obj
    | KM.null obj -> Right (TVStruct [])
    | [(key, val)] <- KM.toList obj
    , isTypeTag (Key.toText key) ->
        decodeTypedWrapped (Key.toText key) val
    | otherwise -> decodeTypedStruct obj
  _ -> Left "Expected JSON object for typed Thrift value"

decodeTypedStruct :: KM.KeyMap Aeson.Value -> Either String ThriftValue
decodeTypedStruct obj = do
  fields <- mapM (\(key, val) -> do
      fid <- parseFieldId (Key.toText key)
      v <- decodeWrappedField val
      Right (fid, v)
    ) (KM.toList obj)
  Right (TVStruct (sortBy (comparing fst) fields))

decodeWrappedField :: Aeson.Value -> Either String ThriftValue
decodeWrappedField json = case json of
  Aeson.Object obj
    | [(key, val)] <- KM.toList obj ->
        decodeTypedWrapped (Key.toText key) val
    | otherwise -> Left "Expected single type-tag in wrapped field"
  _ -> Left "Expected type-wrapped object for field"

decodeTypedWrapped :: Text -> Aeson.Value -> Either String ThriftValue
decodeTypedWrapped tag val = case tag of
  "tf" -> case val of
    Aeson.Bool b -> Right (TVBool b)
    _            -> Left "Expected bool for 'tf'"
  "i8" -> case val of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TVByte v)
      Nothing -> Left "Out of range for i8"
    _ -> Left "Expected number for 'i8'"
  "i16" -> case val of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TVI16 v)
      Nothing -> Left "Out of range for i16"
    _ -> Left "Expected number for 'i16'"
  "i32" -> case val of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TVI32 v)
      Nothing -> Left "Out of range for i32"
    _ -> Left "Expected number for 'i32'"
  "i64" -> case val of
    Aeson.String s -> case (TR.signed TR.decimal s :: Either String (Int64, Text)) of
      Right (v, "") -> Right (TVI64 v)
      _             -> Left ("Invalid i64: " <> Text.unpack s)
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TVI64 (v :: Int64))
      Nothing -> Left "Out of range for i64"
    _ -> Left "Expected string or number for 'i64'"
  "dbl" -> case val of
    Aeson.Number n -> Right (TVDouble (Sci.toRealFloat n))
    _              -> Left "Expected number for 'dbl'"
  "str" -> case val of
    Aeson.String t -> Right (TVString t)
    _              -> Left "Expected string for 'str'"
  "bin" -> case val of
    Aeson.String s -> case Base64.decode (TE.encodeUtf8 s) of
      Right bs -> Right (TVBinary bs)
      Left err -> Left ("Invalid base64: " <> err)
    _ -> Left "Expected string for 'bin'"
  "uuid" -> case val of
    Aeson.String s -> case Base16.decode (TE.encodeUtf8 s) of
      Right bs | BS.length bs == 16 -> Right (TVUUID bs)
               | otherwise -> Left "UUID hex must be 16 bytes"
      Left err -> Left ("Invalid hex: " <> err)
    _ -> Left "Expected string for 'uuid'"
  "rec" -> thriftFromTypedJSON val
  "lst" -> decodeTypedList val
  "set" -> decodeTypedSet val
  "map" -> decodeTypedMap val
  _     -> Left ("Unknown type tag: " <> Text.unpack tag)

decodeTypedList :: Aeson.Value -> Either String ThriftValue
decodeTypedList json = case json of
  Aeson.Array arr
    | V.length arr >= 2 -> do
        etTag <- parseTypeTagStr (arr V.! 0)
        et <- wireTypeFromTag etTag
        let elems = V.toList (V.drop 2 arr)
        decoded <- mapM (decodeTypedWrapped etTag) elems
        Right (TVList et decoded)
  _ -> Left "Expected array (length >= 2) for typed list"

decodeTypedSet :: Aeson.Value -> Either String ThriftValue
decodeTypedSet json = case json of
  Aeson.Array arr
    | V.length arr >= 2 -> do
        etTag <- parseTypeTagStr (arr V.! 0)
        et <- wireTypeFromTag etTag
        let elems = V.toList (V.drop 2 arr)
        decoded <- mapM (decodeTypedWrapped etTag) elems
        Right (TVSet et decoded)
  _ -> Left "Expected array (length >= 2) for typed set"

decodeTypedMap :: Aeson.Value -> Either String ThriftValue
decodeTypedMap json = case json of
  Aeson.Array arr
    | V.length arr >= 3 -> do
        ktTag <- parseTypeTagStr (arr V.! 0)
        vtTag <- parseTypeTagStr (arr V.! 1)
        kt <- wireTypeFromTag ktTag
        vt <- wireTypeFromTag vtTag
        if V.length arr < 4
          then Right (TVMap kt vt [])
          else case arr V.! 3 of
            Aeson.Object obj -> do
              entries <- mapM (\(k, v) -> do
                  key <- parseTypedKeyFromTag ktTag (Key.toText k)
                  val' <- decodeTypedWrapped vtTag v
                  Right (key, val')
                ) (KM.toList obj)
              Right (TVMap kt vt entries)
            _ -> Left "Expected object for map entries"
  _ -> Left "Expected array (length >= 3) for typed map"

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

parseFieldId :: Text -> Either String Int16
parseFieldId t = case (TR.signed TR.decimal t :: Either String (Integer, Text)) of
  Right (n, "") -> Right (fromIntegral n)
  _             -> Left ("Invalid field ID: " <> Text.unpack t)

parseTypeTagStr :: Aeson.Value -> Either String Text
parseTypeTagStr (Aeson.String t) = Right t
parseTypeTagStr _                = Left "Expected string type tag"

wireTypeFromTag :: Text -> Either String ThriftType
wireTypeFromTag = \case
  "tf"   -> Right TT_BOOL
  "i8"   -> Right TT_BYTE
  "i16"  -> Right TT_I16
  "i32"  -> Right TT_I32
  "i64"  -> Right TT_I64
  "dbl"  -> Right TT_DOUBLE
  "str"  -> Right TT_STRING
  "bin"  -> Right TT_STRING
  "rec"  -> Right TT_STRUCT
  "map"  -> Right TT_MAP
  "lst"  -> Right TT_LIST
  "set"  -> Right TT_SET
  "uuid" -> Right TT_UUID
  t      -> Left ("Unknown type tag: " <> Text.unpack t)

wireTypeTag :: ThriftType -> Text
wireTypeTag = \case
  TT_BOOL   -> "tf"
  TT_BYTE   -> "i8"
  TT_I16    -> "i16"
  TT_I32    -> "i32"
  TT_I64    -> "i64"
  TT_DOUBLE -> "dbl"
  TT_STRING -> "str"
  TT_STRUCT -> "rec"
  TT_MAP    -> "map"
  TT_LIST   -> "lst"
  TT_SET    -> "set"
  TT_UUID   -> "uuid"
  TT_STOP   -> "stop"

typeAbbrev :: ThriftValue -> Text
typeAbbrev = \case
  TVBool{}   -> "tf"
  TVByte{}   -> "i8"
  TVI16{}    -> "i16"
  TVI32{}    -> "i32"
  TVI64{}    -> "i64"
  TVDouble{} -> "dbl"
  TVString{} -> "str"
  TVBinary{} -> "bin"
  TVStruct{} -> "rec"
  TVMap{}    -> "map"
  TVList{}   -> "lst"
  TVSet{}    -> "set"
  TVUUID{}   -> "uuid"

isTypeTag :: Text -> Bool
isTypeTag t = t `elem`
  ["tf","i8","i16","i32","i64","dbl","str","bin","rec","map","lst","set","uuid"]

containerElemTag :: ThriftType -> [ThriftValue] -> Text
containerElemTag et = \case
  (e:_) -> typeAbbrev e
  []    -> wireTypeTag et

stringifyKey :: ThriftValue -> Text
stringifyKey = \case
  TVString t  -> t
  TVBool b    -> if b then "true" else "false"
  TVByte v    -> Text.pack (show v)
  TVI16 v     -> Text.pack (show v)
  TVI32 v     -> Text.pack (show v)
  TVI64 v     -> Text.pack (show v)
  TVDouble d  -> Text.pack (show d)
  TVBinary bs -> TE.decodeUtf8 (Base64.encode bs)
  TVUUID bs   -> TE.decodeUtf8 (Base16.encode bs)
  _           -> "<complex>"

parseTypedKeyFromTag :: Text -> Text -> Either String ThriftValue
parseTypedKeyFromTag tag keyStr = case tag of
  "str" -> Right (TVString keyStr)
  "tf"  -> case keyStr of
    "true"  -> Right (TVBool True)
    "false" -> Right (TVBool False)
    _       -> Left "Invalid bool key"
  "i8"  -> readIntKey (\n -> TVByte (fromIntegral n)) keyStr
  "i16" -> readIntKey (\n -> TVI16 (fromIntegral n)) keyStr
  "i32" -> readIntKey (\n -> TVI32 (fromIntegral n)) keyStr
  "i64" -> readIntKey TVI64 keyStr
  _     -> Left ("Unsupported map key type: " <> Text.unpack tag)

readIntKey :: (Int64 -> ThriftValue) -> Text -> Either String ThriftValue
readIntKey f t = case (TR.signed TR.decimal t :: Either String (Int64, Text)) of
  Right (n, "") -> Right (f n)
  _             -> Left ("Invalid numeric key: " <> Text.unpack t)

defaultForWireType :: ThriftType -> ThriftValue
defaultForWireType = \case
  TT_BOOL   -> TVBool False
  TT_BYTE   -> TVByte 0
  TT_I16    -> TVI16 0
  TT_I32    -> TVI32 0
  TT_I64    -> TVI64 0
  TT_DOUBLE -> TVDouble 0
  TT_STRING -> TVString ""
  TT_STRUCT -> TVStruct []
  TT_MAP    -> TVMap TT_STOP TT_STOP []
  TT_LIST   -> TVList TT_STOP []
  TT_SET    -> TVSet TT_STOP []
  TT_UUID   -> TVUUID BS.empty
  TT_STOP   -> TVBool False
