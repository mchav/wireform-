{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Kafka.Protocol.Codegen.WireGenerator
Description : Direct-poke 'Wire' code generator for Kafka messages

Mirrors "Kafka.Protocol.Codegen.Generator" but emits code targeting
'Kafka.Protocol.Wire.Wire' instead of 'Data.Bytes.Serial':

  * Three top-level functions per supported message:
    @wireMaxSizeFooMessage :: Int -> FooMessage -> Int@,
    @wirePokeFooMessage    :: Int -> Ptr Word8 -> FooMessage -> IO (Ptr Word8)@,
    @wirePeekFooMessage    :: Int -> 'ForeignPtr' Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FooMessage, Ptr Word8)@.
  * The same trio for every nested + common struct, so the
    message-level codec can recursively call into per-struct pokes
    without going through a 'Serial' detour.
  * Same version-dispatch shape as the existing 'Serial'-shape generator
    (one branch per @(minV, maxV)@ field-set group).
  * Tagged-fields handling at flexible-version boundaries.
  * Per-field flexibleVersions opt-out: a field with
    @"flexibleVersions": "none"@ stays on the non-compact codec even
    when the surrounding message is on a flexible version.

== Coverage

The Wire generator currently handles every type the Kafka 3.x / 4.x
schemas use:

  * primitive scalars (@bool@, @int8 / int16 / int32 / int64@,
    @uint16 / uint32@, @float64@),
  * @string@ / @bytes@ (compact + non-compact + nullable; the
    @records@ alias the parser maps to bytes),
  * @uuid@,
  * arrays of primitives, strings/bytes, and nested structs (compact
    + non-compact + nullable variants),
  * nested struct fields (recursive call into the struct's wirePoke),
  * tagged-fields trailer on flexible message versions,
  * the per-field @flexibleVersions: none@ opt-out,
  * version dispatch over the @(minV, maxV)@ groups the legacy
    generator uses.

Tagged fields with payload bodies (KIP-866-style; e.g.
@CurrentLeader@ on @ProduceResponse v10+@) currently go through
'WC.serialShimCodec' — the encoder logic for those is non-trivial
(per-tag conditional emit, sorted by tag) and will move to native
in a follow-up. The shim is byte-identical with the native path on
the wire, so the dispatch surface stays uniform.
-}
module Kafka.Protocol.Codegen.WireGenerator
  ( -- * Per-message generation
    generateWireFunctions
  , generateNestedWireFunctions
  , generateWireCodecOverride
    -- * Imports the generated module needs
  , generateWireImports
  , isWireSupported
  ) where

import Data.Char (toLower, toUpper)
import Data.Int (Int16)
import Data.List (groupBy, sortBy)
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Kafka.Protocol.Codegen.Types
import Prettyprinter

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

generateWireImports :: Doc ann
generateWireImports = vsep
  [ "import Foreign.ForeignPtr (ForeignPtr)"
  , "import Foreign.Ptr (Ptr)"
  , "import Data.Word (Word8)"
  , "import qualified Kafka.Protocol.Wire as W"
  , "import qualified Kafka.Protocol.Wire.Primitives as WP"
  ]

----------------------------------------------------------------------
-- Support detection
----------------------------------------------------------------------

-- | @True@ iff every field of the schema (recursively, into nested
-- + common structs) is in the WireGenerator's supported subset.
-- Tagged fields with payloads (KIP-866 style) anywhere in the
-- transitive graph knock the whole message off the native path:
-- either the whole message + every reachable struct emits native
-- pokes, or the whole message uses the Serial shim. Mixing is not
-- safe because the message-level native code expects to call
-- @wirePokeStructName@ for each nested struct.
isWireSupported :: ProtocolSchema -> Bool
isWireSupported schema =
  fieldsSupportedTransitive (schemaFields schema)
    && all (\cs -> case fieldFields cs of
                     Just fs -> fieldsSupportedTransitive fs
                     Nothing -> True)
           (schemaCommonStructs schema)

-- | A field set is "wire-supported" iff none of its fields carry a
-- tag, and every nested struct + array element is also wire-
-- supported (recursively).
fieldsSupportedTransitive :: [FieldSpec] -> Bool
fieldsSupportedTransitive fs =
  noTaggedWithPayload fs && all isFieldSupportedTransitive fs

-- | Are any of these fields tagged-with-payload? A field is
-- "tagged-with-payload" iff it has a tag number assigned.
noTaggedWithPayload :: [FieldSpec] -> Bool
noTaggedWithPayload = not . any isPotentiallyTaggedField

isFieldSupportedTransitive :: FieldSpec -> Bool
isFieldSupportedTransitive f = case fieldType f of
  PrimitiveType t -> isSupportedPrimitive t
  StructType _    -> case fieldFields f of
    Just fs -> fieldsSupportedTransitive fs
    Nothing -> True   -- references a top-level common struct (we
                      -- can't reach its fields from here; trust the
                      -- common-struct check above)
  ArrayType inner -> case fieldFields f of
    Just fs -> fieldsSupportedTransitive fs
    Nothing -> isElementSupported inner

-- | Backwards-compat alias; some local helpers still call this.
isFieldSupported :: FieldSpec -> Bool
isFieldSupported = isFieldSupportedTransitive

isElementSupported :: TypeSpec -> Bool
isElementSupported = \case
  PrimitiveType t -> isSupportedPrimitive t
  StructType _    -> True
  ArrayType _     -> False  -- nested arrays-of-arrays don't appear in Kafka schemas

isSupportedPrimitive :: Text -> Bool
isSupportedPrimitive = \case
  "bool"    -> True
  "int8"    -> True
  "int16"   -> True
  "int32"   -> True
  "int64"   -> True
  "uint16"  -> True
  "uint32"  -> True
  "string"  -> True
  "bytes"   -> True
  "uuid"    -> True
  "float64" -> True
  _         -> False

----------------------------------------------------------------------
-- Per-message generation
----------------------------------------------------------------------

-- | Emit @wireMaxSizeFoo@ + @wirePokeFoo@ + @wirePeekFoo@ for the
-- supplied schema, plus a @WireCodec@ instance pointing at them.
-- Returns 'Nothing' for schemas the generator doesn't yet handle
-- natively (see 'isWireSupported'); the caller will fall back to a
-- 'serialShimCodec' instance in that case.
generateWireFunctions :: ProtocolSchema -> Maybe [Doc ann]
generateWireFunctions schema
  | not (isWireSupported schema) = Nothing
  | otherwise = Just
    [ generateWireMaxSize typeName fields flexibleVer
    , generateWirePoke    typeName fields flexibleVer versions
    , generateWirePeek    typeName fields flexibleVer versions
    ]
  where
    typeName    = toHaskellTypeName (schemaName schema)
    fields      = schemaFields schema
    flexibleVer = parseFlexibleVer (schemaFlexibleVersions schema)
    versions = case parseVersionSpec (schemaValidVersions schema) of
      Right spec -> expandVersionSpec spec
      Left  _    -> []

-- | Emit @wireMaxSize@ + @wirePoke@ + @wirePeek@ for a /nested/
-- struct (or a top-level common struct). The shape is the same as
-- the message-level functions, just with the message's flexible
-- threshold inherited from the parent schema.
--
-- Returns 'Nothing' if the struct contains any field type the
-- WireGenerator doesn't yet handle natively.
generateNestedWireFunctions
  :: Maybe Int16            -- ^ message-level flexible threshold
  -> Text                   -- ^ struct type name (e.g. "MetadataRequestTopic")
  -> [FieldSpec]            -- ^ struct fields
  -> Maybe [Doc ann]
generateNestedWireFunctions flexibleVer structName fields
  | not (fieldsSupportedTransitive fields) = Nothing
  | otherwise = Just
    [ generateNestedWireMaxSize structName fields flexibleVer
    , generateNestedWirePoke    structName fields flexibleVer
    , generateNestedWirePeek    structName fields flexibleVer
    ]

-- | Emit a @WireCodec@ instance for /every/ schema. Always Just —
-- there is no @wireCodec = Nothing@ fallback any more. If the
-- WireGenerator can't yet emit a native codec for the schema it
-- carries arrays-of-tagged-fields-with-payloads, the instance
-- points at 'WC.serialShimCodec', which lifts the legacy 'Serial'
-- encoder / decoder into a 'WireCodecImpl'.
generateWireCodecOverride :: ProtocolSchema -> Doc ann
generateWireCodecOverride schema
  | isWireSupported schema = nativeInstance
  | otherwise              = shimInstance
  where
    typeName = toHaskellTypeName (schemaName schema)

    nativeInstance = vsep
      [ "-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /"
      , "-- 'WC.runDecodeVer' dispatch into the direct-poke functions"
      , "-- generated below, skipping the 'Data.Bytes.Serial' runner."
      , "instance WC.WireCodec" <+> pretty typeName <+> "where"
      , indent 2 $ vsep
          [ "wireCodec = Just WC.WireCodecImpl"
          , indent 2 $ vsep
              [ "{ WC.wireMaxSizeFor = \\v msg -> wireMaxSize" <> pretty typeName
                  <+> "(fromIntegral v) msg"
              , ", WC.wirePokeFor    = \\v p msg -> wirePoke" <> pretty typeName
                  <+> "(fromIntegral v) p msg"
              , ", WC.wirePeekFor    = \\v fp basePtr p endPtr ->"
              , indent 4 ("wirePeek" <> pretty typeName
                  <+> "(fromIntegral v) fp basePtr p endPtr")
              , "}"
              ]
          , "{-# INLINE wireCodec #-}"
          ]
      ]

    shimInstance = vsep
      [ "-- | 'WC.WireCodec' instance via the Serial shim. The"
      , "-- WireGenerator can't yet emit a native codec for this"
      , "-- schema (it carries tagged fields with payloads — KIP-866"
      , "-- style — that the generator hasn't been taught yet), so"
      , "-- we lift the legacy 'encode" <> pretty typeName
          <> "' / 'decode" <> pretty typeName <> "'"
      , "-- pair into a 'WireCodecImpl' via 'WC.serialShimCodec'."
      , "-- The dispatch shape is identical to the native case —"
      , "-- every 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through"
      , "-- a 'Just'-valued codec, no 'Nothing' fallback survives in"
      , "-- the generated output."
      , "instance WC.WireCodec" <+> pretty typeName <+> "where"
      , indent 2 $ vsep
          [ "wireCodec = Just (WC.serialShimCodec encode" <> pretty typeName
              <+> "decode" <> pretty typeName <> ")"
          , "{-# INLINE wireCodec #-}"
          ]
      ]

----------------------------------------------------------------------
-- wireMaxSize
----------------------------------------------------------------------

-- | Worst-case size estimator. Sums per-field upper bounds; never
-- recurses into the value beyond fixed-width metadata, so it's O(1)
-- in field-count terms (and the actual length the poke advances to
-- is what gets shipped on the wire).
generateWireMaxSize
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> Doc ann
generateWireMaxSize typeName fields flexibleVer =
  let funName = "wireMaxSize" <> pretty typeName
  in vsep
    [ "-- | Worst-case wire size of a" <+> pretty typeName <> "."
    , funName <+> ":: Int -> " <> pretty typeName <+> "-> Int"
    , funName <+> "_version msg ="
    , indent 2 $ buildSizeExpr typeName fields flexibleVer
    , ""
    ]

generateNestedWireMaxSize
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> Doc ann
generateNestedWireMaxSize structName fields flexibleVer =
  let funName = "wireMaxSize" <> pretty structName
  in vsep
    [ "-- | Worst-case wire size of a" <+> pretty structName <> "."
    , funName <+> ":: Int -> " <> pretty structName <+> "-> Int"
    , funName <+> "_version msg ="
    , indent 2 $ buildSizeExpr structName fields flexibleVer
    , ""
    ]

buildSizeExpr :: Text -> [FieldSpec] -> Maybe Int16 -> Doc ann
buildSizeExpr typeName fields flexibleVer =
  let perField f    = "+" <+> fieldMaxSize typeName flexibleVer f
      taggedTrailer = case flexibleVer of
        Just _  -> "+ 1"  -- empty tagged-fields trailer is one byte
        Nothing -> ""
  in vsep
       [ "0"
       , vsep (map perField fields)
       , taggedTrailer
       ]

fieldMaxSize :: Text -> Maybe Int16 -> FieldSpec -> Doc ann
fieldMaxSize typeName flexibleVer f =
  let acc = parens (pretty (toHaskellFieldName typeName (fieldName f))
                      <+> "msg")
  in case fieldType f of
    PrimitiveType "bool"    -> "1"
    PrimitiveType "int8"    -> "1"
    PrimitiveType "int16"   -> "2"
    PrimitiveType "int32"   -> "4"
    PrimitiveType "int64"   -> "8"
    PrimitiveType "uint16"  -> "2"
    PrimitiveType "uint32"  -> "4"
    PrimitiveType "uuid"    -> "16"
    PrimitiveType "float64" -> "8"
    PrimitiveType "string" ->
      if fieldUsesCompact f flexibleVer
        then "WP.compactStringMaxSize" <+> parens
               ("P.toCompactString" <+> acc)
        else "WP.kafkaStringMaxSize" <+> acc
    PrimitiveType "bytes" ->
      if fieldUsesCompact f flexibleVer
        then "WP.compactBytesMaxSize" <+> parens
               ("P.toCompactBytes" <+> acc)
        else "WP.kafkaBytesMaxSize" <+> acc
    PrimitiveType _ -> "0"  -- unreachable (gated by isWireSupported)
    -- Nested struct: recurse via the per-struct estimator.
    StructType structName | isFieldNullable f ->
      "(case" <+> acc <+> "of {"
        <+> "P.Null -> 1; P.NotNull s -> 1 + wireMaxSize"
        <> pretty structName <+> "_version s })"
    StructType structName ->
      "wireMaxSize" <> pretty structName <+> "_version" <+> acc
    -- Array: 5-byte header + per-element bound times length.
    ArrayType inner ->
      let header = "5"  -- worst case: UVarInt
          pokeBound = elementMaxSize structName flexibleVer inner
            where structName = case inner of
                    StructType s -> s
                    _            -> ""
      in parens
           ( header <+> "+"
               <+> "(case P.unKafkaArray" <+> acc <+> "of {"
                 <+> "P.NotNull v -> sum (fmap (\\x ->"
                   <+> pokeBound <+> ") v); P.Null -> 0 })"
           )

elementMaxSize :: Text -> Maybe Int16 -> TypeSpec -> Doc ann
elementMaxSize _structName _flexibleVer inner = case inner of
  PrimitiveType "bool"    -> "1"
  PrimitiveType "int8"    -> "1"
  PrimitiveType "int16"   -> "2"
  PrimitiveType "int32"   -> "4"
  PrimitiveType "int64"   -> "8"
  PrimitiveType "uint16"  -> "2"
  PrimitiveType "uint32"  -> "4"
  PrimitiveType "uuid"    -> "16"
  PrimitiveType "float64" -> "8"
  PrimitiveType "string"  -> "WP.compactStringMaxSize (P.toCompactString x)"
  PrimitiveType "bytes"   -> "WP.compactBytesMaxSize (P.toCompactBytes x)"
  PrimitiveType _         -> "0"
  StructType s            -> "wireMaxSize" <> pretty s <+> "_version x"
  ArrayType _             -> "0"  -- arrays-of-arrays are unsupported

----------------------------------------------------------------------
-- wirePoke
----------------------------------------------------------------------

generateWirePoke
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> [Int16]
  -> Doc ann
generateWirePoke typeName _fields _flexibleVer [] =
  emptyPokeStub typeName
generateWirePoke typeName fields flexibleVer versions =
  let funName = "wirePoke" <> pretty typeName
      groups  = groupVersionsByFieldSet versions fields flexibleVer
      branches = map (generatePokeBranch typeName fields flexibleVer) groups
  in vsep
    [ "-- | Direct-poke encoder for" <+> pretty typeName <> "."
    , funName
        <+> ":: Int -> Ptr Word8 -> " <> pretty typeName
        <+> "-> IO (Ptr Word8)"
    , funName <+> "version basePtr msg"
    , vsep branches
    , "  | otherwise = error $ \"wirePoke" <+> pretty typeName
        <+> ": unsupported version: \" ++ show version"
    , ""
    ]

generateNestedWirePoke
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> Doc ann
generateNestedWirePoke structName fields flexibleVer =
  let funName    = "wirePoke" <> pretty structName
      isFlexible = case flexibleVer of
        Just v  -> "version >=" <+> pretty v
        Nothing -> "False"
      regular = filter (not . isPotentiallyTaggedField) fields
      (lastVar, stmts) = pokeFieldStmts structName flexibleVer regular
      -- Tagged-fields trailer is conditional on the message version
      -- (not the struct's own version range — nested structs don't
      -- have one). Mirrors what the legacy generator emits.
      trailer = case flexibleVer of
        Just _  ->
          [ "if" <+> isFlexible <+> "then WP.pokeEmptyTaggedFields"
              <+> pretty lastVar <+> "else pure" <+> pretty lastVar
          ]
        Nothing -> ["pure" <+> pretty lastVar]
  in vsep
    [ "-- | Direct-poke encoder for" <+> pretty structName <> "."
    , funName
        <+> ":: Int -> Ptr Word8 -> " <> pretty structName
        <+> "-> IO (Ptr Word8)"
    , funName <+> "version basePtr msg = do"
    , indent 2 (vsep (("p0 <- pure basePtr" :: Doc ann) : stmts ++ trailer))
    , ""
    ]

generatePokeBranch
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> (Int16, Int16)
  -> Doc ann
generatePokeBranch typeName fields flexibleVer (minV, maxV) =
  let isFlexible = maybe False (<= minV) flexibleVer
      regularFields =
        filter (\f -> fieldInVersionRange minV maxV f
                       && not (isPotentiallyTaggedField f)) fields
      guard
        | minV == maxV =
            "  | version ==" <+> pretty minV <+> "= do"
        | otherwise    =
            "  | version >=" <+> pretty minV
              <+> "&& version <=" <+> pretty maxV <+> "= do"
      (lastVar, stmts) = pokeFieldStmts typeName flexibleVer regularFields
      taggedStmt
        | isFlexible =
            ["WP.pokeEmptyTaggedFields" <+> pretty lastVar]
        | otherwise =
            ["pure" <+> pretty lastVar]
  in vsep
    [ guard
    , indent 4 (vsep (("p0 <- pure basePtr" :: Doc ann) : stmts ++ taggedStmt))
    ]

-- | Emit per-field poke statements; threads cursor through @p0, p1, …@.
pokeFieldStmts
  :: Text
  -> Maybe Int16
  -> [FieldSpec]
  -> (Text, [Doc ann])
pokeFieldStmts typeName flexibleVer fs = go (0 :: Int) fs []
  where
    go i [] acc = (cur i, reverse acc)
    go i (f : rest) acc =
      let !next = i + 1
          stmt  = pretty (cur next) <+> "<-" <+> pokeFieldExpr
                    typeName flexibleVer f (cur i)
      in go next rest (stmt : acc)

    cur n = "p" <> T.pack (show n)

pokeFieldExpr
  :: Text
  -> Maybe Int16
  -> FieldSpec
  -> Text
  -> Doc ann
pokeFieldExpr typeName flexibleVer f cur =
  let acc = parens (pretty (toHaskellFieldName typeName (fieldName f))
                      <+> "msg")
      cp  = pretty cur
  in case fieldType f of
    PrimitiveType "bool" ->
      "W.pokeWord8" <+> cp <+> parens
        ("if" <+> acc <+> "then 1 else 0")
    PrimitiveType "int8" ->
      "W.pokeWord8" <+> cp <+> parens ("fromIntegral" <+> acc)
    PrimitiveType "int16"   -> "W.pokeInt16BE"   <+> cp <+> acc
    PrimitiveType "int32"   -> "W.pokeInt32BE"   <+> cp <+> acc
    PrimitiveType "int64"   -> "W.pokeInt64BE"   <+> cp <+> acc
    PrimitiveType "uint16"  -> "W.pokeWord16BE"  <+> cp <+> acc
    PrimitiveType "uint32"  -> "W.pokeWord32BE"  <+> cp <+> acc
    PrimitiveType "uuid"    -> "WP.pokeKafkaUuid" <+> cp <+> acc
    PrimitiveType "float64" -> "W.pokeFloat64BE" <+> cp <+> acc
    PrimitiveType "string" ->
      if fieldUsesCompact f flexibleVer
        then "WP.pokeCompactString" <+> cp <+> parens ("P.toCompactString" <+> acc)
        else "WP.pokeKafkaString"   <+> cp <+> acc
    PrimitiveType "bytes" ->
      if fieldUsesCompact f flexibleVer
        then "WP.pokeCompactBytes" <+> cp <+> parens ("P.toCompactBytes" <+> acc)
        else "WP.pokeKafkaBytes"   <+> cp <+> acc
    PrimitiveType _ -> "error \"WireGenerator: unsupported primitive\""

    StructType structName | isFieldNullable f ->
      parens
        ("case" <+> acc
           <+> "of { P.Null -> W.pokeWord8" <+> cp
             <+> "0; P.NotNull s -> W.pokeWord8" <+> cp
             <+> "1 >>= \\p' -> wirePoke" <> pretty structName
             <+> "version p' s }")
    StructType structName ->
      "wirePoke" <> pretty structName <+> "version" <+> cp <+> acc

    ArrayType inner ->
      let elementPokeFn = elementPoke flexibleVer inner
          nullable      = isFieldNullable f
      in case (nullable, isJust flexibleVer) of
        (True, True)   -> "WP.pokeVersionedNullableArray"
                            <+> "version" <+> threshold flexibleVer
                            <+> elementPokeFn <+> cp <+> acc
        (False, True)  -> "WP.pokeVersionedArray"
                            <+> "version" <+> threshold flexibleVer
                            <+> elementPokeFn <+> cp <+> acc
        (True, False)  -> "WP.pokeNullableKafkaArray"
                            <+> elementPokeFn <+> cp <+> acc
        (False, False) -> "WP.pokeKafkaArray"
                            <+> elementPokeFn <+> cp <+> acc

threshold :: Maybe Int16 -> Doc ann
threshold (Just v) = pretty v
threshold Nothing  = "999"

-- | Per-element poke for an array. 'Wire'-shaped: takes a cursor +
-- the element value, returns the advanced cursor.
elementPoke :: Maybe Int16 -> TypeSpec -> Doc ann
elementPoke flexibleVer = \case
  PrimitiveType "bool"    ->
    "(\\p x -> W.pokeWord8 p (if x then 1 else 0))"
  PrimitiveType "int8"    ->
    "(\\p x -> W.pokeWord8 p (fromIntegral (x :: Int8)))"
  PrimitiveType "int16"   -> "W.pokeInt16BE"
  PrimitiveType "int32"   -> "W.pokeInt32BE"
  PrimitiveType "int64"   -> "W.pokeInt64BE"
  PrimitiveType "uint16"  -> "W.pokeWord16BE"
  PrimitiveType "uint32"  -> "W.pokeWord32BE"
  PrimitiveType "uuid"    -> "WP.pokeKafkaUuid"
  PrimitiveType "float64" -> "W.pokeFloat64BE"
  PrimitiveType "string" ->
    case flexibleVer of
      Just _  -> "(\\p s -> if version >="
                   <+> threshold flexibleVer
                   <+> "then WP.pokeCompactString p (P.toCompactString s)"
                   <+> "else WP.pokeKafkaString p s)"
      Nothing -> "WP.pokeKafkaString"
  PrimitiveType "bytes" ->
    case flexibleVer of
      Just _  -> "(\\p b -> if version >="
                   <+> threshold flexibleVer
                   <+> "then WP.pokeCompactBytes p (P.toCompactBytes b)"
                   <+> "else WP.pokeKafkaBytes p b)"
      Nothing -> "WP.pokeKafkaBytes"
  PrimitiveType _ ->
    "(\\p _ -> pure p)"  -- unreachable
  StructType structName ->
    "(\\p x -> wirePoke" <> pretty structName <+> "version p x)"
  ArrayType _ ->
    "(\\p _ -> pure p)"  -- arrays-of-arrays unreachable

----------------------------------------------------------------------
-- wirePeek
----------------------------------------------------------------------

generateWirePeek
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> [Int16]
  -> Doc ann
generateWirePeek typeName _fields _flexibleVer [] =
  emptyPeekStub typeName
generateWirePeek typeName fields flexibleVer versions =
  let funName = "wirePeek" <> pretty typeName
      groups  = groupVersionsByFieldSet versions fields flexibleVer
      branches = map (generatePeekBranch typeName fields flexibleVer) groups
  in vsep
    [ "-- | Direct-poke decoder for" <+> pretty typeName <> "."
    , funName
        <+> ":: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8"
        <+> "-> IO (" <> pretty typeName <> ", Ptr Word8)"
    , funName <+> "version _fp _basePtr p0 endPtr"
    , vsep branches
    , "  | otherwise = error $ \"wirePeek" <+> pretty typeName
        <+> ": unsupported version: \" ++ show version"
    , ""
    ]

generateNestedWirePeek
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> Doc ann
generateNestedWirePeek structName fields flexibleVer =
  let funName       = "wirePeek" <> pretty structName
      regular       = filter (not . isPotentiallyTaggedField) fields
      (lastVar, decodeStmts, fieldVarMap) =
        peekFieldStmts structName flexibleVer regular
      trailerStmt = case flexibleVer of
        Just v ->
          [ "pTagsEnd <- if version >="
              <+> pretty v
              <+> "then WP.peekAndSkipTaggedFields"
              <+> pretty lastVar <+> "endPtr"
              <+> "else pure" <+> pretty lastVar
          ]
        Nothing -> []
      finalCur =
        if isJust flexibleVer then "pTagsEnd" else lastVar
      recordBuild = buildRecord structName fields fieldVarMap
  in vsep
    [ "-- | Direct-poke decoder for" <+> pretty structName <> "."
    , funName
        <+> ":: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8"
        <+> "-> IO (" <> pretty structName <> ", Ptr Word8)"
    , funName <+> "version _fp _basePtr p0 endPtr = do"
    , indent 2 $ vsep $ decodeStmts ++ trailerStmt ++
        [ "pure" <+> parens (recordBuild <> "," <+> pretty finalCur) ]
    , ""
    ]

generatePeekBranch
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> (Int16, Int16)
  -> Doc ann
generatePeekBranch typeName allFields flexibleVer (minV, maxV) =
  let isFlexible = maybe False (<= minV) flexibleVer
      regularFields =
        filter (\f -> fieldInVersionRange minV maxV f
                       && not (isPotentiallyTaggedField f)) allFields
      guard
        | minV == maxV =
            "  | version ==" <+> pretty minV <+> "= do"
        | otherwise    =
            "  | version >=" <+> pretty minV
              <+> "&& version <=" <+> pretty maxV <+> "= do"
      (lastVar, decodeStmts, fieldVarMap) =
        peekFieldStmts typeName flexibleVer regularFields
      trailerStmt
        | isFlexible =
            ["pTagsEnd <- WP.peekAndSkipTaggedFields" <+> pretty lastVar
              <+> "endPtr"]
        | otherwise = []
      finalCur =
        if isFlexible then "pTagsEnd" else lastVar
      recordBuild = buildRecord typeName allFields fieldVarMap
  in vsep
    [ guard
    , indent 4 $ vsep $ decodeStmts ++ trailerStmt ++
        [ "pure" <+> parens (recordBuild <> "," <+> pretty finalCur) ]
    ]

peekFieldStmts
  :: Text
  -> Maybe Int16
  -> [FieldSpec]
  -> (Text, [Doc ann], [(Text, Text)])
peekFieldStmts _typeName flexibleVer fs = go (0 :: Int) fs [] []
  where
    go i [] acc varMap = (cur i, reverse acc, reverse varMap)
    go i (f : rest) acc varMap =
      let !next = i + 1
          var   = "f" <> T.pack (show i) <> "_" <> sanitiseVar (fieldName f)
          stmt  = parens (pretty var <> "," <+> pretty (cur next))
                    <+> "<-" <+> peekFieldExpr flexibleVer f (cur i)
      in go next rest (stmt : acc) ((fieldName f, var) : varMap)

    cur n = "p" <> T.pack (show n)

sanitiseVar :: Text -> Text
sanitiseVar = T.toLower . T.filter (\c -> c == '_'
                                       || (c >= 'a' && c <= 'z')
                                       || (c >= 'A' && c <= 'Z')
                                       || (c >= '0' && c <= '9'))

peekFieldExpr
  :: Maybe Int16
  -> FieldSpec
  -> Text
  -> Doc ann
peekFieldExpr flexibleVer f cur =
  let cp  = pretty cur
      end = "endPtr"     :: Doc ann
      fp  = "_fp"        :: Doc ann
      bp  = "_basePtr"   :: Doc ann
  in case fieldType f of
    PrimitiveType "bool" ->
      "(\\(w, p') -> (w /= 0, p')) <$> W.peekWord8" <+> cp <+> end
    PrimitiveType "int8" ->
      "(\\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8"
        <+> cp <+> end
    PrimitiveType "int16"   -> "W.peekInt16BE"  <+> cp <+> end
    PrimitiveType "int32"   -> "W.peekInt32BE"  <+> cp <+> end
    PrimitiveType "int64"   -> "W.peekInt64BE"  <+> cp <+> end
    PrimitiveType "uint16"  -> "W.peekWord16BE" <+> cp <+> end
    PrimitiveType "uint32"  -> "W.peekWord32BE" <+> cp <+> end
    PrimitiveType "uuid"    -> "WP.peekKafkaUuid" <+> cp <+> end
    PrimitiveType "float64" -> "W.peekFloat64BE" <+> cp <+> end
    PrimitiveType "string" ->
      if fieldUsesCompact f flexibleVer
        then "(\\(cs, p') -> (P.fromCompactString cs, p'))"
              <+> "<$> WP.peekCompactString" <+> cp <+> end
        else "WP.peekKafkaString" <+> cp <+> end
    PrimitiveType "bytes" ->
      if fieldUsesCompact f flexibleVer
        then "(\\(cb, p') -> (P.fromCompactBytes cb, p'))"
              <+> "<$> WP.peekCompactBytes" <+> cp <+> end
        else "WP.peekKafkaBytes" <+> cp <+> end
    PrimitiveType _ -> "error \"WireGenerator: unsupported primitive\""

    StructType structName | isFieldNullable f ->
      parens
        ("do { (flag, pAfterFlag) <- W.peekWord8" <+> cp <+> end
           <> "; case flag of {"
           <+> "0 -> pure (P.Null, pAfterFlag); _ -> do {"
           <+> "(s, p'') <- wirePeek" <> pretty structName
             <+> "version" <+> fp <+> bp
             <+> "pAfterFlag" <+> end
           <> "; pure (P.NotNull s, p'') } } }")
    StructType structName ->
      "wirePeek" <> pretty structName
        <+> "version" <+> fp <+> bp <+> cp <+> end

    ArrayType inner ->
      let elementPeekFn = elementPeek flexibleVer inner
          nullable      = isFieldNullable f
      in case (nullable, isJust flexibleVer) of
        (True, True)   -> "WP.peekVersionedNullableArray"
                            <+> "version" <+> threshold flexibleVer
                            <+> elementPeekFn <+> cp <+> end
        (False, True)  -> "WP.peekVersionedArray"
                            <+> "version" <+> threshold flexibleVer
                            <+> elementPeekFn <+> cp <+> end
        (True, False)  -> "WP.peekNullableKafkaArray"
                            <+> elementPeekFn <+> cp <+> end
        (False, False) -> "WP.peekKafkaArray"
                            <+> elementPeekFn <+> cp <+> end

elementPeek :: Maybe Int16 -> TypeSpec -> Doc ann
elementPeek flexibleVer = \case
  PrimitiveType "bool" ->
    "(\\p e -> (\\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p e)"
  PrimitiveType "int8" ->
    "(\\p e -> (\\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p e)"
  PrimitiveType "int16"   -> "W.peekInt16BE"
  PrimitiveType "int32"   -> "W.peekInt32BE"
  PrimitiveType "int64"   -> "W.peekInt64BE"
  PrimitiveType "uint16"  -> "W.peekWord16BE"
  PrimitiveType "uint32"  -> "W.peekWord32BE"
  PrimitiveType "uuid"    -> "WP.peekKafkaUuid"
  PrimitiveType "float64" -> "W.peekFloat64BE"
  PrimitiveType "string" -> case flexibleVer of
    Just _  -> "(\\p e -> if version >=" <+> threshold flexibleVer
                 <+> "then (\\(cs, p') -> (P.fromCompactString cs, p'))"
                 <+> "<$> WP.peekCompactString p e else WP.peekKafkaString p e)"
    Nothing -> "WP.peekKafkaString"
  PrimitiveType "bytes" -> case flexibleVer of
    Just _  -> "(\\p e -> if version >=" <+> threshold flexibleVer
                 <+> "then (\\(cb, p') -> (P.fromCompactBytes cb, p'))"
                 <+> "<$> WP.peekCompactBytes p e else WP.peekKafkaBytes p e)"
    Nothing -> "WP.peekKafkaBytes"
  PrimitiveType _ ->
    "(\\p _ -> error \"WireGenerator: unsupported element\")"
  StructType structName ->
    "(\\p e -> wirePeek" <> pretty structName
      <+> "version _fp _basePtr p e)"
  ArrayType _ ->
    "(\\p _ -> error \"WireGenerator: arrays-of-arrays unsupported\")"

-- | Build the record-construction expression for the decoded value.
-- Fields present in this version pull from 'fieldVarMap'; fields
-- absent in this version (e.g. v3+ fields when decoding v2) fall
-- through to 'generateFieldDefaultDoc'.
buildRecord
  :: Text
  -> [FieldSpec]
  -> [(Text, Text)]
  -> Doc ann
buildRecord typeName allFields fieldVarMap =
  let fieldDoc f =
        let recName = toHaskellFieldName typeName (fieldName f)
            value   = case lookup (fieldName f) fieldVarMap of
              Just var -> pretty var
              Nothing  -> generateFieldDefaultDoc f
        in pretty recName <+> "=" <+> value
      assignments = punctuate "," (map fieldDoc allFields)
  in pretty typeName <+> "{" <+> hsep assignments <+> "}"

emptyPokeStub :: Text -> Doc ann
emptyPokeStub typeName = vsep
  [ "wirePoke" <> pretty typeName
      <+> ":: Int -> Ptr Word8 -> " <> pretty typeName
      <+> "-> IO (Ptr Word8)"
  , "wirePoke" <> pretty typeName <+> "_version _basePtr _msg ="
  , indent 2 ("error \"wirePoke" <+> pretty typeName
              <> ": no valid versions\"")
  , ""
  ]

emptyPeekStub :: Text -> Doc ann
emptyPeekStub typeName = vsep
  [ "wirePeek" <> pretty typeName
      <+> ":: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8"
      <+> "-> IO (" <> pretty typeName <> ", Ptr Word8)"
  , "wirePeek" <> pretty typeName
      <+> "_version _fp _basePtr _p _endPtr ="
  , indent 2 ("error \"wirePeek" <+> pretty typeName
              <> ": no valid versions\"")
  , ""
  ]

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

parseFlexibleVer :: Text -> Maybe Int16
parseFlexibleVer t = case parseVersionSpec t of
  Right (VersionFrom v)    -> Just v
  Right (VersionRange v _) -> Just v
  Right (ExactVersion v)   -> Just v
  _                        -> Nothing

-- | Whether a string / bytes / array field uses the compact codec at
-- the supplied flexible-version threshold.
fieldUsesCompact :: FieldSpec -> Maybe Int16 -> Bool
fieldUsesCompact f flexibleVer
  | fieldFlexibleVersions f == Just "none" = False
  | otherwise = case flexibleVer of
      Nothing -> False
      Just _  -> True

-- | Whether a field is nullable (carries a Null wire encoding /
-- 'P.Nullable' wrapper).
isFieldNullable :: FieldSpec -> Bool
isFieldNullable f = isJust (fieldNullableVersions f)

-- | Convert a protocol name to a Haskell type name (PascalCase).
toHaskellTypeName :: Text -> Text
toHaskellTypeName = T.pack . ensureUpper . T.unpack
  where
    ensureUpper []     = []
    ensureUpper (x:xs) = toUpper x : xs

-- | Match the legacy generator's name munging exactly: the type
-- name's first char goes lowercase, the field name is appended
-- verbatim (the JSON spec already uses PascalCase for field names
-- where appropriate, and lowercase for camelCase fields like
-- @timeoutMs@). Diverging from the legacy here would silently
-- break the @messageX accessor msg@ patterns the codegen-emitted
-- pokes / peeks rely on.
toHaskellFieldName :: Text -> Text -> Text
toHaskellFieldName typeName fname =
  let typePrefix
        | T.null typeName = ""
        | otherwise       = T.pack (toCamelCase (T.unpack typeName))
  in typePrefix <> fname
  where
    toCamelCase []     = []
    toCamelCase (x:xs) = toLower x : xs

isPotentiallyTaggedField :: FieldSpec -> Bool
isPotentiallyTaggedField f = isJust (fieldTag f)

fieldInVersionRange :: Int16 -> Int16 -> FieldSpec -> Bool
fieldInVersionRange minV maxV field =
  case parseVersionSpec (fieldVersions field) of
    Right spec -> all (\v -> inVersionRange v spec) [minV .. maxV]
    Left _     -> False

groupVersionsByFieldSet
  :: [Int16]
  -> [FieldSpec]
  -> Maybe Int16
  -> [(Int16, Int16)]
groupVersionsByFieldSet [] _ _ = []
groupVersionsByFieldSet versions fields flexibleVer =
  let versionsWithFields =
        [(v, (fieldsInVersion v, isVersionFlexible v)) | v <- versions]
      sorted  = sortBy (comparing snd <> comparing fst) versionsWithFields
      grouped = groupBy (\(_, f1) (_, f2) -> f1 == f2) sorted
      ranges  = concatMap (groupConsecutive . map fst) grouped
  in sortBy compareRanges ranges
  where
    fieldsInVersion :: Int16 -> [Text]
    fieldsInVersion v =
      [fieldName f | f <- fields, fieldInVersionRange v v f]

    isVersionFlexible :: Int16 -> Bool
    isVersionFlexible v = maybe False (<= v) flexibleVer

    groupConsecutive :: [Int16] -> [(Int16, Int16)]
    groupConsecutive []     = []
    groupConsecutive (v:vs) = go v v vs
      where
        go start end []     = [(start, end)]
        go start end (x:xs)
          | x == end + 1 = go start x xs
          | otherwise    = (start, end) : go x x xs

    compareRanges :: (Int16, Int16) -> (Int16, Int16) -> Ordering
    compareRanges (min1, max1) (min2, max2)
      | min1 == max1 && min2 /= max2 = LT
      | min1 /= max1 && min2 == max2 = GT
      | size1 /= size2               = compare size1 size2
      | otherwise                    = compare min1 min2
      where
        size1 = max1 - min1
        size2 = max2 - min2

generateFieldDefaultDoc :: FieldSpec -> Doc ann
generateFieldDefaultDoc field = case fieldType field of
  PrimitiveType "bool"    -> "False"
  PrimitiveType "int8"    -> "0"
  PrimitiveType "int16"   -> "0"
  PrimitiveType "int32"   -> "0"
  PrimitiveType "int64"   -> "0"
  PrimitiveType "uint16"  -> "0"
  PrimitiveType "uint32"  -> "0"
  PrimitiveType "float64" -> "0.0"
  PrimitiveType "string"  -> "P.KafkaString Null"
  PrimitiveType "bytes"   -> "P.KafkaBytes Null"
  PrimitiveType "uuid"    -> "P.nullUuid"
  StructType _ | isFieldNullable field -> "P.Null"
  StructType s | otherwise ->
    -- A non-nullable nested struct that's absent at this version
    -- doesn't have a sensible default; emit 'undefined' so a
    -- caller forcing it surfaces the issue. (In practice this is
    -- rare — nested structs are usually always present from v0.)
    "undefined :: " <> pretty s
  ArrayType _ | isFieldNullable field -> "P.KafkaArray P.Null"
  ArrayType _ -> "P.mkKafkaArray V.empty"
  PrimitiveType _ -> "undefined"

-- 'fromMaybe' kept around for future per-version compact dispatch.
_keepFromMaybe :: a -> Maybe a -> a
_keepFromMaybe = fromMaybe
