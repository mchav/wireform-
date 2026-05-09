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
  * Same version-dispatch shape as the existing 'Serial'-shape generator
    (one branch per @(minV, maxV)@ field-set group).
  * Tagged-fields handling at flexible-version boundaries: emit a
    single @0@ varint on the encode side; on the decode side read the
    UVarInt count and skip that many @(tag, size, bytes)@ triples via
    'WP.peekAndSkipTaggedFields'.
  * Per-field flexibleVersions opt-out: a field with
    @"flexibleVersions": "none"@ stays on the non-compact codec even
    when the surrounding message is on a flexible version (mirrors
    'Generator.fieldOptsOutOfFlexible').
  * Reuses the data-type definitions emitted by the legacy generator
    — only the codec functions are different. Both sets of functions
    coexist in the same module so callers can opt into the direct-poke
    path on a per-call-site basis (the dispatch happens through
    'Kafka.Protocol.Wire.Codec.WireCodec').

== Coverage

The Wire generator currently handles:

  * primitive scalar fields (@bool@, @int8 / int16 / int32 / int64@,
    @uint16 / uint32@),
  * @string@ (compact + non-compact + nullable),
  * @bytes@ (compact + non-compact + nullable, including the @records@
    alias the parser maps to bytes),
  * @uuid@,
  * tagged-fields trailer on flexible message versions,
  * the per-field @flexibleVersions: none@ opt-out,
  * version dispatch over the same @(minV, maxV)@ groups the legacy
    generator uses.

It does /not/ yet handle arrays or nested struct fields. When a
schema contains either, 'generateWireFunctions' returns 'Nothing' and
the surrounding 'Kafka.Protocol.Codegen.Generator' falls through to
the existing default @wireCodec = Nothing@ instance — i.e. those
messages keep going through the 'Serial' fallback, with no caller
visible change.

The intent is to keep growing this surface message-class by
message-class until the full schema is on the native Wire path; each
expansion is a small, testable diff.
-}
module Kafka.Protocol.Codegen.WireGenerator
  ( -- * Per-message generation
    generateWireFunctions
    -- * 'WireCodec' instance override (only emitted when the message
    -- is on the supported subset).
  , generateWireCodecOverride
    -- * Imports the generated module needs (always emitted, since
    -- the cost of an unused import is one warning at most and the
    -- legacy-shape modules silence those with @-Wno-unused-imports@).
  , generateWireImports
    -- * Predicate the surrounding generator uses to decide whether
    -- to skip the default 'wireCodec = Nothing' instance.
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

-- | @True@ iff every field of the schema (and every tagged field)
-- is one of the primitive types the WireGenerator currently supports.
-- Returns @False@ for arrays + nested struct fields; the caller
-- (generateMessage) keeps the default @wireCodec = Nothing@ instance
-- in that case.
isWireSupported :: ProtocolSchema -> Bool
isWireSupported schema = all isFieldSupported (schemaFields schema)

isFieldSupported :: FieldSpec -> Bool
isFieldSupported f = case fieldType f of
  PrimitiveType t -> isSupportedPrimitive t
  StructType _    -> False  -- nested struct not yet supported
  ArrayType _     -> False  -- arrays not yet supported

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
  _         -> False  -- e.g. float64 not yet handled (no Wire helper)

----------------------------------------------------------------------
-- Per-message generation
----------------------------------------------------------------------

-- | Emit @wireMaxSizeFoo@ + @wirePokeFoo@ + @wirePeekFoo@ for the
-- supplied schema, plus a @WireCodec@ instance pointing at them.
-- Returns 'Nothing' when the schema falls outside the supported
-- subset (see 'isWireSupported'); the caller keeps the default
-- 'wireCodec = Nothing' instance in that case.
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
    flexibleVer = case parseVersionSpec (schemaFlexibleVersions schema) of
      Right (VersionFrom v)     -> Just v
      Right (VersionRange v _)  -> Just v
      Right (ExactVersion v)    -> Just v
      _                         -> Nothing
    versions = case parseVersionSpec (schemaValidVersions schema) of
      Right spec -> expandVersionSpec spec
      Left  _    -> []

-- | Emit the @WireCodec@ instance override for a wire-supported
-- schema. The surrounding generator decides which of this and the
-- default @wireCodec = Nothing@ instance to emit.
generateWireCodecOverride :: ProtocolSchema -> Maybe (Doc ann)
generateWireCodecOverride schema
  | not (isWireSupported schema) = Nothing
  | otherwise = Just $ vsep
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
  where
    typeName = toHaskellTypeName (schemaName schema)

----------------------------------------------------------------------
-- wireMaxSize
----------------------------------------------------------------------

-- | Emit a worst-case size estimator. We don't recurse into the
-- value: every field contributes its upper bound (5 for varints,
-- 4 + length for byte payloads, ...). The caller allocates a buffer
-- of this size and the actual length the poke advances to is what
-- gets shipped on the wire.
generateWireMaxSize
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> Doc ann
generateWireMaxSize typeName fields flexibleVer =
  let funName       = "wireMaxSize" <> pretty typeName
      perField f    = "+" <+> fieldMaxSize f
      taggedTrailer = case flexibleVer of
        Just _  -> "+ 1"  -- empty tagged-fields trailer is one byte
        Nothing -> ""
  in vsep
    [ "-- | Worst-case wire size of a" <+> pretty typeName <> "."
    , "-- Sums the per-field upper bounds; the actual poke may advance"
    , "-- the cursor by less."
    , funName <+> ":: Int -> " <> pretty typeName <+> "-> Int"
    , funName <+> "_version msg ="
    , indent 2 $ vsep
        [ "0"  -- starting accumulator
        , vsep (map perField fields)
        , taggedTrailer
        ]
    , ""
    ]
  where
    fieldMaxSize f =
      let acc = parens (pretty (toHaskellFieldName typeName (fieldName f))
                          <+> "msg")
      in case fieldType f of
        PrimitiveType "bool"   -> "1"
        PrimitiveType "int8"   -> "1"
        PrimitiveType "int16"  -> "2"
        PrimitiveType "int32"  -> "4"
        PrimitiveType "int64"  -> "8"
        PrimitiveType "uint16" -> "2"
        PrimitiveType "uint32" -> "4"
        PrimitiveType "uuid"   -> "16"
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
        _ -> "0"  -- unreachable: gated by isWireSupported

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
  -- No valid versions: emit a stub that errors uniformly. Should
  -- never trigger because isWireSupported wouldn't have approved
  -- this schema, but guard anyway so the codegen output always
  -- type-checks.
  vsep
    [ "wirePoke" <> pretty typeName
        <+> ":: Int -> Ptr Word8 -> " <> pretty typeName
        <+> "-> IO (Ptr Word8)"
    , "wirePoke" <> pretty typeName <+> "_version _basePtr _msg ="
    , indent 2 ("error \"wirePoke" <+> pretty typeName
                <> ": no valid versions\"")
    , ""
    ]
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
      hasTagged = isFlexible
      guard
        | minV == maxV =
            "  | version ==" <+> pretty minV <+> "= do"
        | otherwise    =
            "  | version >=" <+> pretty minV
              <+> "&& version <=" <+> pretty maxV <+> "= do"
      (lastVar, stmts) = pokeFieldStmts typeName flexibleVer regularFields
      taggedStmt
        | hasTagged =
            ["WP.pokeEmptyTaggedFields" <+> pretty lastVar]
        | otherwise =
            ["pure" <+> pretty lastVar]
  in vsep
    [ guard
    , indent 4 (vsep (("p0 <- pure basePtr" :: Doc ann) : stmts ++ taggedStmt))
    ]

-- | Emit the per-field poke statements. Threads the cursor through
-- numbered locals (@p0, p1, …@) and returns the name of the last
-- live cursor binding so the trailer can use it.
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
  -> Text                  -- ^ cursor variable name
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
    PrimitiveType "int16"  -> "W.pokeInt16BE"  <+> cp <+> acc
    PrimitiveType "int32"  -> "W.pokeInt32BE"  <+> cp <+> acc
    PrimitiveType "int64"  -> "W.pokeInt64BE"  <+> cp <+> acc
    PrimitiveType "uint16" -> "W.pokeWord16BE" <+> cp <+> acc
    PrimitiveType "uint32" -> "W.pokeWord32BE" <+> cp <+> acc
    PrimitiveType "uuid"   -> "WP.pokeKafkaUuid" <+> cp <+> acc
    PrimitiveType "string" ->
      if fieldUsesCompact f flexibleVer
        then "WP.pokeCompactString" <+> cp <+> parens ("P.toCompactString" <+> acc)
        else "WP.pokeKafkaString"   <+> cp <+> acc
    PrimitiveType "bytes" ->
      if fieldUsesCompact f flexibleVer
        then "WP.pokeCompactBytes" <+> cp <+> parens ("P.toCompactBytes" <+> acc)
        else "WP.pokeKafkaBytes"   <+> cp <+> acc
    _ -> "error \"WireGenerator: unsupported field type\""

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
  vsep
    [ "wirePeek" <> pretty typeName
        <+> ":: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8"
        <+> "-> IO (" <> pretty typeName <> ", Ptr Word8)"
    , "wirePeek" <> pretty typeName
        <+> "_version _fp _basePtr _p _endPtr ="
    , indent 2 ("error \"wirePeek" <+> pretty typeName
                <> ": no valid versions\"")
    , ""
    ]
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

-- | Emit the per-field decode statements. Returns the last cursor
-- variable name and the decode-result variable name for each field.
peekFieldStmts
  :: Text
  -> Maybe Int16
  -> [FieldSpec]
  -> (Text, [Doc ann], [(Text, Text)])
peekFieldStmts typeName flexibleVer fs = go (0 :: Int) fs [] []
  where
    go i [] acc varMap = (cur i, reverse acc, reverse varMap)
    go i (f : rest) acc varMap =
      let !next = i + 1
          var   = "f" <> T.pack (show i) <> "_" <> sanitiseVar (fieldName f)
          stmt  = parens (pretty var <> "," <+> pretty (cur next))
                    <+> "<-" <+> peekFieldExpr typeName flexibleVer f (cur i)
      in go next rest (stmt : acc) ((fieldName f, var) : varMap)

    cur n = "p" <> T.pack (show n)

sanitiseVar :: Text -> Text
sanitiseVar = T.toLower . T.filter (\c -> c == '_'
                                       || (c >= 'a' && c <= 'z')
                                       || (c >= 'A' && c <= 'Z')
                                       || (c >= '0' && c <= '9'))

peekFieldExpr
  :: Text
  -> Maybe Int16
  -> FieldSpec
  -> Text                  -- ^ cursor variable name
  -> Doc ann
peekFieldExpr _typeName flexibleVer f cur =
  let cp = pretty cur
      end = "endPtr"
  in case fieldType f of
    PrimitiveType "bool" ->
      "(\\(w, p') -> (w /= 0, p')) <$> W.peekWord8" <+> cp <+> end
    PrimitiveType "int8" ->
      "(\\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8"
        <+> cp <+> end
    PrimitiveType "int16"  -> "W.peekInt16BE"  <+> cp <+> end
    PrimitiveType "int32"  -> "W.peekInt32BE"  <+> cp <+> end
    PrimitiveType "int64"  -> "W.peekInt64BE"  <+> cp <+> end
    PrimitiveType "uint16" -> "W.peekWord16BE" <+> cp <+> end
    PrimitiveType "uint32" -> "W.peekWord32BE" <+> cp <+> end
    PrimitiveType "uuid"   -> "WP.peekKafkaUuid" <+> cp <+> end
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
    _ -> "error \"WireGenerator: unsupported field type\""

-- | Build the record-construction expression for the decoded value.
-- Fields present in this version pull from 'fieldVarMap'; fields
-- absent in this version (e.g. @v3+@ fields when decoding v2) fall
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

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Whether a string / bytes field should use the compact codec at
-- the supplied flexible-version threshold. Mirrors
-- 'Generator.fieldOptsOutOfFlexible': a field with
-- @"flexibleVersions": "none"@ stays on the non-compact codec even
-- on flexible message versions.
fieldUsesCompact :: FieldSpec -> Maybe Int16 -> Bool
fieldUsesCompact f flexibleVer
  | fieldFlexibleVersions f == Just "none" = False
  | otherwise = case flexibleVer of
      Nothing -> False
      Just _  -> True

-- 'fromMaybe' kept imported for future per-version compact dispatch.
_keepFromMaybe :: a -> Maybe a -> a
_keepFromMaybe = fromMaybe

----------------------------------------------------------------------
-- Helpers (intentionally inlined here rather than imported from
-- 'Kafka.Protocol.Codegen.Generator' to avoid a module-import cycle).
-- They are exact copies of the ones the Serial-shape generator
-- defines; if either copy drifts, the cross-codec property tests in
-- @Protocol.WireCodecSpec@ will catch the divergence.
----------------------------------------------------------------------

-- | Convert a protocol name to a Haskell type name (PascalCase).
toHaskellTypeName :: Text -> Text
toHaskellTypeName = T.pack . ensureUpper . T.unpack
  where
    ensureUpper []     = []
    ensureUpper (x:xs) = toUpper x : xs

-- | Convert a protocol field name to a Haskell record field name.
-- Prepends the type name in camelCase to avoid field-name conflicts.
toHaskellFieldName :: Text -> Text -> Text
toHaskellFieldName typeName fname =
  let typePrefix
        | T.null typeName = ""
        | otherwise       = T.pack (toCamelCase (T.unpack typeName))
  in typePrefix <> T.pack (capitalise (T.unpack fname))
  where
    toCamelCase []     = []
    toCamelCase (x:xs) = toLower x : xs

    capitalise []     = []
    capitalise (x:xs) = toUpper x : xs

-- | Whether a field has a tag number assigned. Tagged fields are
-- skipped in the regular field loop and emitted inside the
-- TaggedFields envelope at the end of flexible message versions.
isPotentiallyTaggedField :: FieldSpec -> Bool
isPotentiallyTaggedField f = isJust (fieldTag f)

-- | True iff the field is present in every version in @[minV..maxV]@.
fieldInVersionRange :: Int16 -> Int16 -> FieldSpec -> Bool
fieldInVersionRange minV maxV field =
  case parseVersionSpec (fieldVersions field) of
    Right spec -> all (\v -> inVersionRange v spec) [minV .. maxV]
    Left _     -> False

-- | Group concrete versions into @(minV, maxV)@ bands sharing the
-- same field set + flexibility. Mirrors
-- 'Kafka.Protocol.Codegen.Generator.groupVersionsByFieldSet'; see
-- the comments there for the rationale (consecutive runs only,
-- exact-version cases first, smaller ranges before larger ones).
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

-- | Default value for a field absent in the current version.
-- Restricted to the primitive subset 'isWireSupported' approves; if
-- a struct / array slips through, the codegen output will fail to
-- build (which is what we want — keeps the supported subset honest).
generateFieldDefaultDoc :: FieldSpec -> Doc ann
generateFieldDefaultDoc field = case fieldType field of
  PrimitiveType "bool"   -> "False"
  PrimitiveType "int8"   -> "0"
  PrimitiveType "int16"  -> "0"
  PrimitiveType "int32"  -> "0"
  PrimitiveType "int64"  -> "0"
  PrimitiveType "uint16" -> "0"
  PrimitiveType "uint32" -> "0"
  PrimitiveType "string" -> "P.KafkaString Null"
  PrimitiveType "bytes"  -> "P.KafkaBytes Null"
  PrimitiveType "uuid"   -> "P.nullUuid"
  _                      -> "undefined"
