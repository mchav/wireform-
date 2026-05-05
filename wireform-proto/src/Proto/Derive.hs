{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Annotation-driven Template Haskell deriver for protobuf wire
-- instances on hand-written Haskell records.
--
-- This module is the TH counterpart to 'Proto.CodeGen' (which emits
-- pure Haskell text from a parsed @.proto@ AST). The text codegen
-- stays the home for cross-platform builds where TH is awkward; this
-- deriver is for users who prefer to write the Haskell record
-- themselves, with explicit @ANN@ annotations supplying the proto
-- field numbers and wire-encoding choices.
--
-- The IDL-driven entry point 'Proto.TH.loadProto' may also reuse the
-- low-level body builders in "Proto.Derive.Internal" to share the
-- encoder \/ decoder \/ size logic without going through @ANN@ +
-- 'Language.Haskell.TH.reifyAnnotations'.
--
-- == Scope (initial release)
--
-- * Records (@TypeShapeRecord@) only. Newtypes/enums/sums are not yet
--   supported by 'deriveProto'; use 'Proto.CodeGen' for those.
-- * Singular fields of one of the recognized scalar types
--   (@Int32 \/ Int64 \/ Word32 \/ Word64 \/ Bool \/ Float \/ Double \/
--   Text \/ ByteString@) or a submessage with existing
--   'MessageEncode' \/ 'MessageDecode' \/ 'MessageSize' instances.
-- * @Maybe a@ for explicit field presence (proto2 optional or proto3
--   @optional@).
-- * @wireOverride WireZigZag@ to force ZigZag for sint32 \/ sint64.
-- * @wireOverride WireFixed@ to force fixed-width for fixed32 \/
--   fixed64 \/ sfixed32 \/ sfixed64.
--
-- == Required modifiers
--
-- Every record field MUST carry an explicit @tag N@ modifier — there
-- is no positional default. This matches Thrift's discipline and is
-- the only correct policy for a wire format where field numbers are
-- part of the contract.
--
-- == Generated instances
--
-- * 'MessageEncode' — @buildMessage@ with proto3 default-value skip.
-- * 'MessageSize'   — mirror of the encoder for two-pass output.
-- * 'MessageDecode' — accumulator loop with field-number dispatch.
-- * 'IsMessage'     — provides 'messageTypeName'. Defaults to the
--   Haskell type's base name; override via the @customModifier
--   "wireform-proto.message-type"@ payload.
--
-- == Out of scope (for now)
--
-- * Repeated / packed / map fields.
-- * Oneof fields.
-- * 'ProtoMessage' schema metadata.
-- * Proto3 JSON ('Aeson.ToJSON' \/ 'Aeson.FromJSON').
-- * 'Hashable' (use @deriving anyclass Hashable@ on the type instead).
-- * Unknown-field preservation.
module Proto.Derive
  ( -- * Annotation-driven entry points
    deriveProto
  , deriveProtoEncode
  , deriveProtoSize
  , deriveProtoDecode
  , deriveProtoIsMessage

    -- * Pre-translated entry point (for IDL bridges)
  , TranslatedField (..)
  , TranslatedMessage (..)
  , TranslatedOneofVariant (..)
  , translatedField
  , deriveProtoFromTranslated
  , deriveProtoFromTranslatedWith

    -- * Re-exports from the internal field model
  , I.ProtoField (..)
  , I.ProtoFieldKind (..)
  , I.ProtoFieldType (..)
  , I.RepeatedRep (..)
  , I.RepeatedMode (..)
  , I.scalarPackable
  , I.OneofVariant (..)
  , I.oneofVariant
  , I.Scalar (..)
  , I.MessageMeta (..)
  , I.defaultMessageMeta
  , StringRep (..)
  , BytesRep (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH

import qualified Proto.Derive.Internal as I
import Proto.Repr (BytesRep (..), StringRep (..))

import Wireform.Derive.Backend (backendProto)
import Wireform.Derive.Modifier
  ( MapKeyScalar (..)
  , Modifier
  , WireOverride (..)
  )
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

-- ---------------------------------------------------------------------------
-- Public entry points (annotation-driven)
-- ---------------------------------------------------------------------------

-- | Derive 'MessageEncode', 'MessageSize', 'MessageDecode', and
-- 'IsMessage' for a record type.
deriveProto :: Name -> Q [Dec]
deriveProto nm = do
  enc <- deriveProtoEncode nm
  siz <- deriveProtoSize nm
  dec <- deriveProtoDecode nm
  ism <- deriveProtoIsMessage nm
  pure (enc ++ siz ++ dec ++ ism)

deriveProtoEncode :: Name -> Q [Dec]
deriveProtoEncode nm = do
  ti  <- recordOnly nm =<< reifyTypeInfo nm
  fis <- protoFields ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  inst <- I.mkEncodeInstance typ fis
  pure [inst]

deriveProtoSize :: Name -> Q [Dec]
deriveProtoSize nm = do
  ti  <- recordOnly nm =<< reifyTypeInfo nm
  fis <- protoFields ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  inst <- I.mkSizeInstance typ fis
  pure [inst]

deriveProtoDecode :: Name -> Q [Dec]
deriveProtoDecode nm = do
  ti  <- recordOnly nm =<< reifyTypeInfo nm
  fis <- protoFields ti
  let typ     = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  conName <- conNameOf nm
  inst    <- I.mkDecodeInstance typ conName fis
  pure [inst]

deriveProtoIsMessage :: Name -> Q [Dec]
deriveProtoIsMessage nm = do
  ti <- recordOnly nm =<< reifyTypeInfo nm
  let nameStr = T.pack (nameBase (typeInfoName ti))
      typ     = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  inst <- I.mkIsMessageInstance typ nameStr
  pure [inst]

-- ---------------------------------------------------------------------------
-- Pre-translated entry point (for IDL bridges)
-- ---------------------------------------------------------------------------

-- | A field already lifted from a non-Haskell source (e.g. a parsed
-- @.proto@ message). Sidesteps GHC's reify graph so callers may
-- emit the corresponding @data@ declaration in the same TH splice.
--
-- The @tf*Shape@ fields disambiguate the field kind for shapes
-- that are otherwise indistinguishable by Haskell type alone
-- (a @Vector Foo@ might be a @repeated@ field; a @Map Text Bar@
-- might be a proto3 @map@; a @Maybe Sum@ might be a @oneof@).
-- Construct via 'translatedField' for the common defaults.
data TranslatedField = TranslatedField
  { tfSelector       :: !Name
    -- ^ Record selector to be used for this field. Must match the
    -- accompanying record constructor that the caller emits.
  , tfInnerType      :: !Type
    -- ^ For singular fields: the value type (with 'Maybe' stripped).
    -- For repeated fields: the element type.
    -- For map fields: the value type.
    -- For oneof fields: ignored (each variant carries its own).
  , tfOptional       :: !Bool
    -- ^ True iff the Haskell field is wrapped in 'Maybe' (and is
    -- not a oneof or repeated/map; those have their own kinds).
  , tfRepeated       :: !(Maybe I.RepeatedRep)
    -- ^ When @Just@, this field is a @repeated@ in the named
    -- container shape.
  , tfPacked         :: !(Maybe Bool)
    -- ^ Override for the packed-encoding choice on this repeated
    -- field. @Nothing@ (the default) lets the bridge decide:
    -- packable scalars (everything except @string@ \/ @bytes@ \/
    -- submessage \/ enum) get 'I.ModePacked'; non-packable
    -- elements stay unpacked. @Just True@ forces packed (only
    -- legal for packable scalars). @Just False@ forces the
    -- one-record-per-element \"expanded\" shape — useful for
    -- proto2 fields without @[packed = true]@ or for
    -- byte-compat with very old wire data.
  , tfMapKey         :: !(Maybe MapKeyScalar)
    -- ^ When @Just@, this field is a proto3 @map<K, V>@. The key
    -- wire encoding is supplied here; the value's wire encoding
    -- is inferred from 'tfInnerType' \/ 'tfModifiers' as usual.
  , tfIsEnum         :: !Bool
    -- ^ True iff the inner type is encoded as a varint via
    -- 'fromEnum' \/ 'toEnum'. Bridges should set this for all
    -- proto enum types.
  , tfOneofVariants  :: ![TranslatedOneofVariant]
    -- ^ Non-empty iff this field is a proto @oneof@. Each variant
    -- pairs a sum-type constructor with its tag and payload type.
  , tfStringRep      :: !StringRep
    -- ^ Haskell representation for proto @string@ fields. Defaults
    -- to 'StrictTextRep'. Only consulted when the field is a
    -- string-typed scalar (singular, repeated element, or map
    -- value); otherwise ignored.
  , tfBytesRep       :: !BytesRep
    -- ^ Haskell representation for proto @bytes@ fields. Defaults
    -- to 'StrictBytesRep'. Only consulted when the field is a
    -- bytes-typed scalar.
  , tfModifiers      :: ![Modifier]
    -- ^ Additional modifiers (tag for non-oneof fields, wire
    -- override, custom payloads, etc.). The proto backend is
    -- consulted via 'foldModifiers'.
  } deriving stock (Show)

-- | One arm of a oneof (used by IDL bridges).
data TranslatedOneofVariant = TranslatedOneofVariant
  { tovConstructor :: !Name
    -- ^ Sum-type constructor name.
  , tovInnerType   :: !Type
    -- ^ The constructor's single argument type.
  , tovModifiers   :: ![Modifier]
    -- ^ Per-arm modifiers (must include a 'tag N').
  } deriving stock (Show)

-- | Convenience: build a 'TranslatedField' for a singular,
-- non-repeated, non-map, non-oneof, non-enum field. Sets the
-- shape fields to their empty defaults; only @tfSelector@,
-- @tfInnerType@, @tfOptional@, and @tfModifiers@ vary at the
-- common call site.
translatedField :: Name -> Type -> Bool -> [Modifier] -> TranslatedField
translatedField sel ty opt mods = TranslatedField
  { tfSelector      = sel
  , tfInnerType     = ty
  , tfOptional      = opt
  , tfRepeated      = Nothing
  , tfPacked        = Nothing
  , tfMapKey        = Nothing
  , tfIsEnum        = False
  , tfOneofVariants = []
  , tfStringRep     = StrictTextRep
  , tfBytesRep      = StrictBytesRep
  , tfModifiers     = mods
  }

-- | A whole message lifted from an external source.
data TranslatedMessage = TranslatedMessage
  { tmType        :: !Type
    -- ^ Fully applied type, e.g. @ConT ''Person@.
  , tmConstructor :: !Name
    -- ^ Record constructor; for single-constructor records this is
    -- usually the type name.
  , tmProtoName   :: !Text
    -- ^ Logical proto name returned by 'PM.messageTypeName'.
  , tmFields      :: ![TranslatedField]
  , tmUnknownFieldsSel :: !(Maybe Name)
    -- ^ Optional selector for an @[Decode.UnknownField]@ field on
    -- the record. When set, the synthesised codecs preserve
    -- unknown tags through that slot. 'Nothing' means unknown
    -- fields are silently dropped (the original
    -- 'Proto.Derive.deriveProto' behaviour).
  } deriving stock (Show)

-- | Derive 'MessageEncode', 'MessageSize', 'MessageDecode', and
-- 'IsMessage' for a 'TranslatedMessage' without consulting the
-- reify graph. Intended for IDL-driven splices that synthesise a
-- fresh @data@ declaration alongside the instance group.
deriveProtoFromTranslated :: TranslatedMessage -> Q [Dec]
deriveProtoFromTranslated tm = do
  let meta = I.MessageMeta { I.mmUnknownFieldsSel = tmUnknownFieldsSel tm }
  deriveProtoFromTranslatedWith meta tm

-- | Like 'deriveProtoFromTranslated' but lets the caller supply an
-- explicit 'I.MessageMeta'. Useful when the unknown-fields slot
-- (or any future per-message knob) must be threaded in by name
-- rather than derived from the 'TranslatedMessage' fields.
deriveProtoFromTranslatedWith :: I.MessageMeta -> TranslatedMessage -> Q [Dec]
deriveProtoFromTranslatedWith meta tm = do
  fis <- traverse translatedFieldToProtoField (tmFields tm)
  I.synthesiseProtoInstancesWith meta (tmType tm) (tmConstructor tm)
                                       (tmProtoName tm) fis

translatedFieldToProtoField :: TranslatedField -> Q I.ProtoField
translatedFieldToProtoField tf = do
  mi <- case foldModifiers backendProto (tfModifiers tf) of
    Right info -> pure info
    Left  err  -> fail $ "Proto.Derive: invalid modifiers on "
                      ++ nameBase (tfSelector tf) ++ ": " ++ show err
  -- Oneofs use the variant tags exclusively; the field-level tag is
  -- ignored.
  let isOneof = not (null (tfOneofVariants tf))
  tagN <- case miTag mi of
    Just n  -> pure n
    Nothing
      | isOneof   -> pure 0
      | otherwise -> fail $ "Proto.Derive: field " ++ nameBase (tfSelector tf)
                        ++ " is missing a `tag N` modifier"
  pft <- if tfIsEnum tf
           then pure I.PFEnum
           else pickFieldType (tfSelector tf) (tfInnerType tf) (miWireOverride mi)
  let kind = case (tfRepeated tf, tfMapKey tf, tfOneofVariants tf, tfOptional tf) of
        (_, _, vs@(_:_), _) ->
          I.FKOneof (map (variantOf pft) vs)
        (Just rep, _, _, _) ->
          I.FKRepeated rep (chooseMode (tfPacked tf) pft)
        (_, Just mks, _, _) -> I.FKMap mks
        (_, _, _, True)     -> I.FKMaybe
        _                   -> I.FKBare
  pure (I.protoField (tfSelector tf) tagN kind pft (tfInnerType tf))
          { I.pfStringRep = tfStringRep tf
          , I.pfBytesRep  = tfBytesRep tf
          }
  where
    -- Pick packed vs. unpacked. Defaults to packed for packable
    -- scalars (proto3 spec default); falls through to unpacked for
    -- string/bytes/submessage/enum and any field the caller
    -- explicitly opted out of with @tfPacked = Just False@.
    chooseMode :: Maybe Bool -> I.ProtoFieldType -> I.RepeatedMode
    chooseMode override pft' = case override of
      Just True  -> I.ModePacked
      Just False -> I.ModeUnpacked
      Nothing    -> case pft' of
        I.PFScalar sc | I.scalarPackable sc -> I.ModePacked
        _                                   -> I.ModeUnpacked
    variantOf _outer tov =
      case foldModifiers backendProto (tovModifiers tov) of
        Left err -> error $ "Proto.Derive: invalid modifiers on oneof variant "
                         ++ nameBase (tovConstructor tov) ++ ": " ++ show err
        Right vmi -> case miTag vmi of
          Nothing -> error $ "Proto.Derive: oneof variant "
                          ++ nameBase (tovConstructor tov)
                          ++ " is missing a `tag N` modifier"
          Just vt ->
            let vpft = case (typeBaseName (tovInnerType tov), miWireOverride vmi) of
                  (Just "Int32",      Just WireZigZag) -> I.PFScalar I.SSInt32
                  (Just "Int64",      Just WireZigZag) -> I.PFScalar I.SSInt64
                  (Just "Word32",     Just WireFixed)  -> I.PFScalar I.SFixed32
                  (Just "Word64",     Just WireFixed)  -> I.PFScalar I.SFixed64
                  (Just "Int32",      Just WireFixed)  -> I.PFScalar I.SSFixed32
                  (Just "Int64",      Just WireFixed)  -> I.PFScalar I.SSFixed64
                  (Just "Int32",      _)               -> I.PFScalar I.SInt32
                  (Just "Int64",      _)               -> I.PFScalar I.SInt64
                  (Just "Word32",     _)               -> I.PFScalar I.SUInt32
                  (Just "Word64",     _)               -> I.PFScalar I.SUInt64
                  (Just "Bool",       _)               -> I.PFScalar I.SBool
                  (Just "Float",      _)               -> I.PFScalar I.SFloat
                  (Just "Double",     _)               -> I.PFScalar I.SDouble
                  (Just "Text",       _)               -> I.PFScalar I.SString
                  (Just "ByteString", _)               -> I.PFScalar I.SBytes
                  _                                    -> I.PFSubmessage
            in I.oneofVariant (tovConstructor tov) vt
                              (tovInnerType tov) vpft

-- ---------------------------------------------------------------------------
-- Annotation-driven field analysis (for deriveProto)
-- ---------------------------------------------------------------------------

protoFields :: TypeInfo -> Q [I.ProtoField]
protoFields ti = case typeInfoShape ti of
  TypeShapeRecord c -> traverse (analyseField (typeInfoName ti)) (conInfoFields c)
  _                 -> fail "Proto.Derive: only records are supported"

analyseField :: Name -> FieldInfo -> Q I.ProtoField
analyseField tyName (FieldInfo mSel fieldTy) = do
  selName <- case mSel of
    Just n  -> pure n
    Nothing -> fail $ "Proto.Derive: " ++ nameBase tyName
                  ++ " has a positional (non-record) field; only records are supported"
  mi    <- reifyModifierInfoFor backendProto selName
  tagN  <- case miTag mi of
    Just n  -> pure n
    Nothing -> fail $ "Proto.Derive: field " ++ nameBase selName
                  ++ " is missing a `tag N` modifier"
  let (kind, innerTy) = unwrapMaybe fieldTy
  pft <- pickFieldType selName innerTy (miWireOverride mi)
  pure (I.protoField selName tagN kind pft innerTy)

unwrapMaybe :: Type -> (I.ProtoFieldKind, Type)
unwrapMaybe (AppT (ConT n) t) | n == ''Maybe = (I.FKMaybe, t)
unwrapMaybe t                                 = (I.FKBare,  t)

-- | Choose the wire encoding for a field. The supplied
-- @WireOverride@ (if any) takes precedence over the type-driven
-- default.
pickFieldType :: Name -> Type -> Maybe WireOverride -> Q I.ProtoFieldType
pickFieldType selName ty mOverride = case (typeBaseName ty, mOverride) of
  (Just "Int32",      Just WireZigZag) -> pure (I.PFScalar I.SSInt32)
  (Just "Int64",      Just WireZigZag) -> pure (I.PFScalar I.SSInt64)

  (Just "Word32",     Just WireFixed)  -> pure (I.PFScalar I.SFixed32)
  (Just "Word64",     Just WireFixed)  -> pure (I.PFScalar I.SFixed64)
  (Just "Int32",      Just WireFixed)  -> pure (I.PFScalar I.SSFixed32)
  (Just "Int64",      Just WireFixed)  -> pure (I.PFScalar I.SSFixed64)

  (Just "Int32",      _)               -> pure (I.PFScalar I.SInt32)
  (Just "Int64",      _)               -> pure (I.PFScalar I.SInt64)
  (Just "Word32",     _)               -> pure (I.PFScalar I.SUInt32)
  (Just "Word64",     _)               -> pure (I.PFScalar I.SUInt64)
  (Just "Bool",       _)               -> pure (I.PFScalar I.SBool)
  (Just "Float",      _)               -> pure (I.PFScalar I.SFloat)
  (Just "Double",     _)               -> pure (I.PFScalar I.SDouble)
  (Just "Text",       _)               -> pure (I.PFScalar I.SString)
  (Just "ByteString", _)               -> pure (I.PFScalar I.SBytes)

  _ -> pure I.PFSubmessage
  where
    _ = selName

typeBaseName :: Type -> Maybe String
typeBaseName = \case
  ConT n   -> Just (nameBase n)
  AppT t _ -> typeBaseName t
  _        -> Nothing

recordOnly :: Name -> TypeInfo -> Q TypeInfo
recordOnly nm ti = case typeInfoShape ti of
  TypeShapeRecord _ -> pure ti
  _ -> fail $ "Proto.Derive: " ++ nameBase nm
          ++ " must be a single-constructor record (got "
          ++ describeShape (typeInfoShape ti) ++ ")"

describeShape :: TypeShape -> String
describeShape = \case
  TypeShapeNewtype _ -> "newtype"
  TypeShapeRecord _  -> "record"
  TypeShapeEnum _    -> "enum"
  TypeShapeSum _     -> "sum"

conNameOf :: Name -> Q Name
conNameOf tyName = do
  ti <- reifyTypeInfo tyName
  case typeInfoShape ti of
    TypeShapeRecord c -> pure (conInfoName c)
    _                 -> fail "Proto.Derive: not a record"

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
