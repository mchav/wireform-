{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Template Haskell helpers that emit the four \"satellite\" instance
-- groups every 'loadProto'-generated message wants:
--
--   * 'Proto.Schema.ProtoMessage' — schema metadata
--     ('protoMessageName', 'protoPackageName', 'protoFieldDescriptors',
--     'protoDefaultValue').
--   * 'Data.Aeson.ToJSON' / 'Data.Aeson.FromJSON' — proto3 canonical
--     JSON (camelCase keys, base64 bytes, string-encoded 64-bit
--     integers, NaN\/Infinity sentinels for floats; all of which are
--     already handled by helpers in "Proto.JSON").
--   * 'Data.Hashable.Hashable' — recursive structural hash that mirrors
--     what the pure-text codegen in "Proto.CodeGen" emits.
--   * 'Proto.Schema.ProtoEnum' — enum metadata + numeric \<-\> name
--     conversion.
--
-- The pure-text codegen has emitted these for years. This module
-- catches the TH path up so 'loadProto' produces the same surface.
module Proto.TH.Metadata
  ( -- * Per-message instances (consumed by 'Proto.TH.messageToDecls'')
    mkProtoMessageInstance
  , mkAesonInstancesForMessage
  , mkHashableInstanceForMessage

    -- * Per-oneof instances (consumed for each oneof carrier sum)
  , mkOneofAesonInstances
  , mkOneofHashableInstance

    -- * Per-enum instances
  , mkProtoEnumInstance
  , mkEnumAesonInstances
  , mkEnumHashableInstance

    -- * Field shape descriptor (passed in by 'Proto.TH')
  , MetaField (..)
  , MetaFieldKind (..)
  , JsonKind (..)

    -- * Internal helpers used by spliced code
    -- | Re-exported so the splice doesn't have to qualify them
    -- across module boundaries.
  , bytesVectorToJSON
  , bytesListToJSON
  , parseBytesVectorMaybe
  , parseBytesListMaybe
  ) where

import Data.ByteString (ByteString)
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonT

import qualified Proto.JSON as PJI

import Language.Haskell.TH

import qualified Proto.JSON as PJ
import qualified Proto.Schema as PS

-- ---------------------------------------------------------------------------
-- Field shape (passed in by 'Proto.TH')
-- ---------------------------------------------------------------------------

-- | A condensed view of one record field — enough to drive every
-- satellite-instance emitter without re-deriving anything from the
-- raw 'Proto.AST' shape. The caller (currently only 'Proto.TH')
-- builds this from the 'FieldSpec' it already has.
data MetaField = MetaField
  { mfSelector  :: !Name
    -- ^ Haskell record selector for the field (lower-camel).
  , mfProtoName :: !Text
    -- ^ Proto-side field name (snake_case).
  , mfJsonName  :: !Text
    -- ^ JSON key (proto3 default: camelCase form of the proto name,
    -- overridable via the @json_name@ proto option — the caller is
    -- responsible for resolving that).
  , mfNumber    :: !Int
    -- ^ Proto field number.
  , mfTypeDesc  :: !(Q Exp)
    -- ^ Splice-time builder for the field's
    -- 'Proto.Schema.FieldTypeDescriptor' literal.
  , mfLabel     :: !(Q Exp)
    -- ^ Splice-time builder for the field's
    -- 'Proto.Schema.FieldLabel'' literal.
  , mfKind      :: !MetaFieldKind
    -- ^ Container / wrap shape on the Haskell side. Drives default
    -- value, JSON encoding, and the per-shape branch in
    -- @hashWithSalt@.
  , mfJsonKind  :: !JsonKind
    -- ^ Whether the field needs the bytes-aware JSON helpers from
    -- "Proto.JSON" (because either the value type or the map value
    -- type is @bytes@).
  }

-- | Container shape on the Haskell side. The hash combinator picks
-- @V.foldl'@, @Map.foldlWithKey'@, etc., based on this.
data MetaFieldKind
  = MFKBare
  | MFKMaybe
  | MFKVector
  | MFKList
  | MFKSeq
  | MFKMap
  | MFKOneof   -- ^ Carrier is @Maybe SumType@ but the JSON / hash
               --   shape differs from a plain @MFKMaybe@.

-- | Whether the field needs the bytes-aware JSON encoder/parser
-- helpers in "Proto.JSON". Plain (non-bytes) fields use the
-- 'Aeson.toJSON' instance for their type directly.
data JsonKind
  = JKNormal
    -- ^ Standard 'Aeson.toJSON' / 'parseFieldMaybe' path.
  | JKBytes
    -- ^ A @bytes@-typed field. JSON wants base64 via the
    -- 'PJ.bytesFieldToJSON' / 'PJ.parseBytesFieldMaybe' helpers.
  | JKBytesMap
    -- ^ A @map\<K, bytes\>@ — values must base64.
  | JKBytesVector
    -- ^ A @repeated bytes@ field carried as @Vector ByteString@.
    -- JSON shape is an array of base64 strings.
  | JKBytesList
    -- ^ A @repeated bytes@ field carried as @[ByteString]@.

-- ---------------------------------------------------------------------------
-- ProtoMessage
-- ---------------------------------------------------------------------------

-- | Synthesise the 'PS.ProtoMessage' instance for a record. The
-- field descriptors are built lazily inside @protoFieldDescriptors@
-- so the splice cost is paid only when the user actually inspects
-- the schema.
mkProtoMessageInstance
  :: Name        -- ^ Haskell type name (e.g. @\'\'Account@).
  -> Text        -- ^ Fully-qualified proto name (e.g. @"my.pkg.Account"@).
  -> Text        -- ^ Proto package (may be empty).
  -> Name        -- ^ The @default<Tyname>@ value the splice already emitted.
  -> [MetaField]
  -> Q [Dec]
mkProtoMessageInstance tyName fqName pkg defName fields = do
  descrEntries <- traverse (oneFieldDescriptor tyName) fields
  let descrMap   = AppE (VarE 'Map.fromList) (ListE descrEntries)
      protoNameDec = FunD 'PS.protoMessageName
        [Clause [WildP] (NormalB (textLit fqName)) []]
      protoPkgDec  = FunD 'PS.protoPackageName
        [Clause [WildP] (NormalB (textLit pkg)) []]
      protoDefDec  = FunD 'PS.protoDefaultValue
        [Clause [] (NormalB (VarE defName)) []]
      protoDescrDec = FunD 'PS.protoFieldDescriptors
        [Clause [WildP] (NormalB descrMap) []]
  pure
    [ InstanceD Nothing []
        (AppT (ConT ''PS.ProtoMessage) (ConT tyName))
        [protoNameDec, protoPkgDec, protoDefDec, protoDescrDec]
    ]

-- | One @(fieldNumber, SomeField FieldDescriptor { ... })@ pair.
oneFieldDescriptor :: Name -> MetaField -> Q Exp
oneFieldDescriptor tyName MetaField{..} = do
  msgVar <- newName "msg"
  vVar   <- newName "v"
  tdesc  <- mfTypeDesc
  lbl    <- mfLabel
  let getter = LamE [VarP msgVar] (AppE (VarE mfSelector) (VarE msgVar))
      setter = LamE [VarP vVar, VarP msgVar]
                 (RecUpdE (VarE msgVar) [(mfSelector, VarE vVar)])
      -- 'tyName' is needed to force the right setter target type
      -- (RecUpdE doesn't carry it explicitly); GHC infers it from
      -- the surrounding 'Map.fromList' literal once the FieldDescriptor
      -- is annotated.
      _ = tyName
      record = RecConE 'PS.FieldDescriptor
        [ ('PS.fdName,     textLit mfProtoName)
        , ('PS.fdNumber,   intLit  mfNumber)
        , ('PS.fdTypeDesc, tdesc)
        , ('PS.fdLabel,    lbl)
        , ('PS.fdGet,      getter)
        , ('PS.fdSet,      setter)
        ]
      someField = AppE (ConE 'PS.SomeField) record
      pair = TupE [Just (intLit mfNumber), Just someField]
  pure pair

-- ---------------------------------------------------------------------------
-- ToJSON / FromJSON for messages
-- ---------------------------------------------------------------------------

-- | Synthesise both the 'Aeson.ToJSON' and 'Aeson.FromJSON'
-- instances for a generated record.
--
-- The shape mirrors what the pure-text codegen in "Proto.CodeGen"
-- emits: a @jsonObject@ with one entry per field on the encode side,
-- and @parseFieldMaybe@ + a per-field @maybe (default) id@
-- assignment on the decode side. Bytes / bytes-map fields go
-- through the dedicated helpers in "Proto.JSON" so base64 and
-- 64-bit-integer-as-string encoding happen automatically.
mkAesonInstancesForMessage
  :: Name        -- ^ Type name.
  -> Name        -- ^ @default<Tyname>@.
  -> [MetaField]
  -> Q [Dec]
mkAesonInstancesForMessage tyName defName fields = do
  toJSONDec   <- mkToJSONForMessage tyName fields
  fromJSONDec <- mkFromJSONForMessage tyName defName fields
  pure [toJSONDec, fromJSONDec]

mkToJSONForMessage :: Name -> [MetaField] -> Q Dec
mkToJSONForMessage tyName fields = do
  msgVar <- newName "msg"
  let entries = fmap (toJSONEntry msgVar) fields
      body = AppE (VarE 'PJ.jsonObject) (ListE entries)
  pure $ InstanceD Nothing []
    (AppT (ConT ''Aeson.ToJSON) (ConT tyName))
    [ FunD 'Aeson.toJSON
        [Clause [VarP msgVar] (NormalB body) []]
    ]

toJSONEntry :: Name -> MetaField -> Exp
toJSONEntry msgVar mf =
  let fieldExpr = AppE (VarE (mfSelector mf)) (VarE msgVar)
      jsonKey   = textLit (mfJsonName mf)
  in case mfJsonKind mf of
    JKBytes ->
      AppE (AppE (VarE 'PJ.bytesFieldToJSON) jsonKey) fieldExpr
    JKBytesMap ->
      AppE (AppE (VarE 'PJ.bytesMapFieldToJSON) jsonKey) fieldExpr
    JKBytesVector ->
      let valExp = AppE (VarE 'bytesVectorToJSON) fieldExpr
      in TupE [Just jsonKey, Just valExp]
    JKBytesList ->
      let valExp = AppE (VarE 'bytesListToJSON) fieldExpr
      in TupE [Just jsonKey, Just valExp]
    JKNormal ->
      let valExp = AppE (VarE 'Aeson.toJSON) fieldExpr
      in TupE [Just jsonKey, Just valExp]

mkFromJSONForMessage :: Name -> Name -> [MetaField] -> Q Dec
mkFromJSONForMessage tyName defName fields = do
  objVar    <- newName "obj"
  fldNames  <- mapM (\mf -> (,) mf <$> newName ("fld_" ++ nameBase (mfSelector mf))) fields
  let binds = fmap (uncurry (parseBindStmt objVar)) fldNames
      assigns = fmap (uncurry (fromJSONAssign defName)) fldNames
      finalE  = RecUpdE (VarE defName) assigns
      bodyDo  = DoE Nothing (binds ++ [NoBindS (AppE (VarE 'pure) finalE)])
      -- Aeson.withObject "TypeName" $ \obj -> ...
      typeNameLit = LitE (StringL (nameBase tyName))
      body = AppE (AppE (VarE 'Aeson.withObject) typeNameLit)
               (LamE [VarP objVar] bodyDo)
  pure $ InstanceD Nothing []
    (AppT (ConT ''Aeson.FromJSON) (ConT tyName))
    [ FunD 'Aeson.parseJSON
        [Clause [] (NormalB body) []]
    ]

parseBindStmt :: Name -> MetaField -> Name -> Stmt
parseBindStmt objVar mf fldVar =
  let parseFn = case mfJsonKind mf of
        JKBytes       -> VarE 'PJ.parseBytesFieldMaybe
        JKBytesMap    -> VarE 'PJ.parseBytesMapFieldMaybe
        JKBytesVector -> VarE 'parseBytesVectorMaybe
        JKBytesList   -> VarE 'parseBytesListMaybe
        JKNormal      -> VarE 'PJ.parseFieldMaybe
      call = AppE (AppE parseFn (VarE objVar)) (textLit (mfJsonName mf))
  in BindS (VarP fldVar) call

fromJSONAssign :: Name -> MetaField -> Name -> (Name, Exp)
fromJSONAssign defName mf fldVar =
  -- mfSelector mf = maybe (mfSelector defName) id fld_var
  let dflt = AppE (VarE (mfSelector mf)) (VarE defName)
      e = AppE (AppE (AppE (VarE 'maybe) dflt) (VarE 'id)) (VarE fldVar)
  in (mfSelector mf, e)

-- ---------------------------------------------------------------------------
-- Hashable for messages
-- ---------------------------------------------------------------------------

-- | Synthesise a 'Hashable' instance for a generated record.
-- Mirrors the per-shape combinator the pure-text codegen uses
-- ('V.foldl' for vectors, 'Map.foldlWithKey'' for maps, plain
-- 'hashWithSalt' for everything else).
mkHashableInstanceForMessage :: Name -> [MetaField] -> Q Dec
mkHashableInstanceForMessage tyName fields = do
  saltVar <- newName "salt"
  msgVar  <- newName "msg"
  let body = case fields of
        [] -> VarE saltVar
        _  -> foldl (hashStep msgVar) (VarE saltVar) fields
  pure $ InstanceD Nothing []
    (AppT (ConT ''Hashable) (ConT tyName))
    [ FunD 'hashWithSalt
        [Clause [VarP saltVar, VarP msgVar] (NormalB body) []]
    ]

-- | One step of the unrolled hash: combine the previous accumulator
-- (already a salt) with this field's contribution.
hashStep :: Name -> Exp -> MetaField -> Exp
hashStep msgVar acc mf =
  let fieldExpr = AppE (VarE (mfSelector mf)) (VarE msgVar)
  in case mfKind mf of
    MFKVector ->
      AppE (AppE (AppE (VarE 'V.foldl') (VarE 'hashWithSalt)) acc) fieldExpr
    MFKList ->
      AppE (AppE (AppE (VarE 'foldl) (VarE 'hashWithSalt)) acc) fieldExpr
    MFKSeq ->
      AppE (AppE (AppE (VarE 'foldlSeq) (VarE 'hashWithSalt)) acc) fieldExpr
    MFKMap ->
      -- \s k v -> s `hashWithSalt` k `hashWithSalt` v
      let s = mkName "s"
          k = mkName "k"
          v = mkName "v"
          step = LamE [VarP s, VarP k, VarP v]
            (AppE (AppE (VarE 'hashWithSalt)
              (AppE (AppE (VarE 'hashWithSalt) (VarE s)) (VarE k)))
              (VarE v))
      in AppE (AppE (AppE (VarE 'Map.foldlWithKey') step) acc) fieldExpr
    _ ->
      AppE (AppE (VarE 'hashWithSalt) acc) fieldExpr

-- | A 'Data.List.foldl''-shaped foldl over a 'Seq', exposed as a
-- splice helper so the generated code can avoid touching @Seq@'s
-- own combinators (which differ slightly between containers
-- versions).
foldlSeq :: (a -> b -> a) -> a -> Seq b -> a
foldlSeq f = foldl f

-- ---------------------------------------------------------------------------
-- bytes-vector / bytes-list JSON helpers
-- ---------------------------------------------------------------------------

-- | A @repeated bytes@ field as a JSON array of base64 strings.
bytesVectorToJSON :: V.Vector ByteString -> Aeson.Value
bytesVectorToJSON =
  Aeson.toJSON . fmap PJI.protoBytesToJSON . V.toList

-- | A list-backed @repeated bytes@ field as a JSON array.
bytesListToJSON :: [ByteString] -> Aeson.Value
bytesListToJSON = Aeson.toJSON . fmap PJI.protoBytesToJSON

-- | Parse @Maybe (Vector ByteString)@ from a JSON object key.
parseBytesVectorMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector ByteString))
parseBytesVectorMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> Just . V.fromList <$> traverse PJI.protoBytesFromJSON (vs :: [Aeson.Value])

-- | Parse @Maybe [ByteString]@ from a JSON object key.
parseBytesListMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe [ByteString])
parseBytesListMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> Just <$> traverse PJI.protoBytesFromJSON (vs :: [Aeson.Value])

-- ---------------------------------------------------------------------------
-- Oneof: ToJSON / FromJSON / Hashable for the carrier sum
-- ---------------------------------------------------------------------------

-- | Emit @ToJSON@ + @FromJSON@ for an oneof carrier sum. The
-- pure-text codegen emits @toJSON _ = Aeson.Null@ and
-- @parseJSON _ = fail \"Cannot parse oneof from JSON\"@; we follow
-- suit. (Spec-conformant proto3 JSON handles oneofs at the parent
-- message level rather than here, so a standalone instance for the
-- carrier sum is mostly a placeholder to make the type fit any
-- generic 'ToJSON' constraints downstream code might require.)
mkOneofAesonInstances :: Name -> Q [Dec]
mkOneofAesonInstances sumTy = do
  let toJSONInst = InstanceD Nothing []
        (AppT (ConT ''Aeson.ToJSON) (ConT sumTy))
        [ FunD 'Aeson.toJSON
            [Clause [WildP] (NormalB (ConE 'Aeson.Null)) []]
        ]
      fromJSONInst = InstanceD Nothing []
        (AppT (ConT ''Aeson.FromJSON) (ConT sumTy))
        [ FunD 'Aeson.parseJSON
            [Clause [WildP]
              (NormalB (AppE (VarE 'fail)
                (LitE (StringL "Cannot parse oneof from JSON")))) []]
        ]
  pure [toJSONInst, fromJSONInst]

-- | Emit a 'Hashable' instance for an oneof carrier sum: tag the
-- variant index in front of the payload's hash. Variant indices
-- start at 0 in declaration order (matching the pure-text codegen).
mkOneofHashableInstance :: Name -> [Name] -> Q Dec
mkOneofHashableInstance sumTy variantCons = do
  saltVar <- newName "salt"
  vVar    <- newName "v"
  let mkArm (idx, conName) = Clause
        [ VarP saltVar
        , ConP conName [] [VarP vVar]
        ]
        (NormalB
          (AppE (AppE (VarE 'hashWithSalt)
            (AppE (AppE (VarE 'hashWithSalt) (VarE saltVar))
              (SigE (intLit idx) (ConT ''Int))))
            (VarE vVar)))
        []
      arms = case variantCons of
        [] ->
          [Clause [VarP saltVar, WildP] (NormalB (VarE saltVar)) []]
        _  -> fmap mkArm (zip [0 ..] variantCons)
  pure $ InstanceD Nothing []
    (AppT (ConT ''Hashable) (ConT sumTy))
    [FunD 'hashWithSalt arms]

-- ---------------------------------------------------------------------------
-- Enums
-- ---------------------------------------------------------------------------

-- | Synthesise the 'PS.ProtoEnum' instance: ties each generated
-- constructor to its proto-side wire number and string name.
mkProtoEnumInstance
  :: Name              -- ^ Haskell enum type.
  -> Text              -- ^ Fully-qualified proto enum name.
  -> [(Name, Text, Int)]
                       -- ^ @(haskellCon, protoName, evNumber)@
                       --   for every declared value (aliases
                       --   included).
  -> Q Dec
mkProtoEnumInstance tyName fqName values = do
  let -- protoEnumName _ = "..."
      nameDec = FunD 'PS.protoEnumName
        [Clause [WildP] (NormalB (textLit fqName)) []]
      -- protoEnumValues _ = [(name, num), ...]
      pairs = ListE
        [ TupE [Just (textLit n), Just (intLit num)]
        | (_, n, num) <- values
        ]
      valuesDec = FunD 'PS.protoEnumValues
        [Clause [WildP] (NormalB pairs) []]
      -- toProtoEnumValue: pattern-match on every (alias-inclusive) constructor
      toClauses =
        [ Clause [ConP con [] []]
                 (NormalB (intLit num)) []
        | (con, _, num) <- values
        ]
      toDec = FunD 'PS.toProtoEnumValue toClauses
      -- fromProtoEnumValue: one Just clause per primary number, then catch-all Nothing
      primaries = primaryByNumber values
      fromClauses =
        fmap (\(con, _, num) ->
                Clause [LitP (IntegerL (fromIntegral num))]
                       (NormalB (AppE (ConE 'Just) (ConE con))) [])
             primaries
        <> [Clause [WildP] (NormalB (ConE 'Nothing)) []]
      fromDec = FunD 'PS.fromProtoEnumValue fromClauses
  pure $ InstanceD Nothing []
    (AppT (ConT ''PS.ProtoEnum) (ConT tyName))
    [nameDec, valuesDec, toDec, fromDec]

-- | Drop later occurrences of any wire number; preserves first.
primaryByNumber :: [(Name, Text, Int)] -> [(Name, Text, Int)]
primaryByNumber = go []
  where
    go _    []                          = []
    go seen (v@(_, _, n) : rest)
      | n `elem` seen = go seen rest
      | otherwise     = v : go (n : seen) rest

-- | Synthesise @ToJSON@ / @FromJSON@ for an enum: the primary name
-- string on the encode side, and a string-or-number parser on the
-- decode side (per the proto3 JSON spec, both are accepted on
-- read, but the canonical write form is the name).
mkEnumAesonInstances :: Name -> [(Name, Text, Int)] -> Q [Dec]
mkEnumAesonInstances tyName values = do
  let primaries = primaryByNumber values
      toClauses =
        [ Clause [ConP con [] []]
                 (NormalB (AppE (ConE 'Aeson.String) (textLit pname))) []
        | (con, pname, _) <- primaries
        ]
      toDec = FunD 'Aeson.toJSON toClauses

      -- fromJSON: \case String "FOO" -> pure ConFOO ... Number n -> ... _ -> fail
      stringClauses =
        [ Match (ConP 'Aeson.String [] [LitP (StringL (T.unpack pname))])
                (NormalB (AppE (VarE 'pure) (ConE con))) []
        | (con, pname, _) <- primaries
        ]
      nVar = mkName "n"
      numberMatch = Match (ConP 'Aeson.Number [] [VarP nVar])
        (NormalB (AppE (VarE 'pure)
          (AppE (VarE 'toEnum)
            (AppE (VarE 'round) (VarE nVar)))))
        []
      failMatch = Match WildP
        (NormalB (AppE (VarE 'fail)
          (LitE (StringL ("Invalid enum value for " <> nameBase tyName))))) []
      caseExp = LamCaseE (stringClauses <> [numberMatch, failMatch])

  pure
    [ InstanceD Nothing []
        (AppT (ConT ''Aeson.ToJSON) (ConT tyName))
        [toDec]
    , InstanceD Nothing []
        (AppT (ConT ''Aeson.FromJSON) (ConT tyName))
        [ FunD 'Aeson.parseJSON
            [Clause [] (NormalB caseExp) []]
        ]
    ]

-- | Hash an enum by its proto wire number.
mkEnumHashableInstance :: Name -> Q Dec
mkEnumHashableInstance tyName = do
  saltVar <- newName "salt"
  xVar    <- newName "x"
  let body = AppE (AppE (VarE 'hashWithSalt) (VarE saltVar))
               (AppE (VarE 'PS.toProtoEnumValue) (VarE xVar))
  pure $ InstanceD Nothing []
    (AppT (ConT ''Hashable) (ConT tyName))
    [ FunD 'hashWithSalt
        [Clause [VarP saltVar, VarP xVar] (NormalB body) []]
    ]

-- ---------------------------------------------------------------------------
-- Tiny helpers
-- ---------------------------------------------------------------------------

intLit :: Int -> Exp
intLit n = LitE (IntegerL (fromIntegral n))

textLit :: Text -> Exp
textLit t = AppE (VarE 'T.pack) (LitE (StringL (T.unpack t)))

