-- | Code generation for Haskell modules from parsed proto files.
--
-- Generates complete Haskell modules with:
-- * Plain record types (no lenses)
-- * Strict fields with UNPACK pragmas for primitives
-- * Specialized encode/decode instances
-- * Enum types with custom Enum instances matching proto values
-- * Oneof types as sum types
-- * Default value instances
module Proto.CodeGen
  ( generateModule
  , generateModuleText
  , GenerateOpts (..)
  , defaultGenerateOpts
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Proto.AST
import Proto.CodeGen.Types
import Proto.CodeGen.Encode
import Proto.CodeGen.Decode

-- | Options controlling code generation.
data GenerateOpts = GenerateOpts
  { genModulePrefix    :: Text       -- ^ Module prefix (e.g. "Proto.Gen")
  , genStrictFields    :: Bool       -- ^ Use strict fields (default True)
  , genUnpackPrims     :: Bool       -- ^ UNPACK primitive fields (default True)
  , genDeriveGeneric   :: Bool       -- ^ Derive Generic (default True)
  , genDeriveNFData    :: Bool       -- ^ Derive NFData (default True)
  , genPackedRepeated  :: Bool       -- ^ Use packed encoding for repeated scalars (default True, proto3)
  } deriving stock (Show, Eq)

defaultGenerateOpts :: GenerateOpts
defaultGenerateOpts = GenerateOpts
  { genModulePrefix   = "Proto.Gen"
  , genStrictFields   = True
  , genUnpackPrims    = True
  , genDeriveGeneric  = True
  , genDeriveNFData   = True
  , genPackedRepeated = True
  }

-- | Generate a complete Haskell module from a proto file.
generateModule :: GenerateOpts -> ProtoFile -> Doc ann
generateModule opts pf =
  vsep
    [ genModuleHeader opts pf
    , mempty
    , genImports
    , mempty
    , vsep (concatMap genTopLevel (protoTopLevels pf))
    ]

-- | Generate module as Text.
generateModuleText :: GenerateOpts -> ProtoFile -> Text
generateModuleText opts pf =
  renderStrict (layoutPretty defaultLayoutOptions (generateModule opts pf))

genModuleHeader :: GenerateOpts -> ProtoFile -> Doc ann
genModuleHeader opts pf =
  let modName = case protoPackage pf of
        Just pkg -> genModulePrefix opts <> "." <> hsModuleName pkg
        Nothing  -> genModulePrefix opts <> ".Generated"
  in vsep
    [ pretty ("{-# LANGUAGE StrictData #-}" :: Text)
    , pretty ("{-# LANGUAGE DeriveGeneric #-}" :: Text)
    , pretty ("{-# LANGUAGE DeriveAnyClass #-}" :: Text)
    , pretty ("{-# LANGUAGE DerivingStrategies #-}" :: Text)
    , pretty ("{-# LANGUAGE OverloadedStrings #-}" :: Text)
    , pretty ("module" :: Text) <+> pretty modName <+> pretty ("where" :: Text)
    ]

genImports :: Doc ann
genImports = vsep
  [ pretty ("import Data.ByteString (ByteString)" :: Text)
  , pretty ("import Data.Int (Int32, Int64)" :: Text)
  , pretty ("import Data.Text (Text)" :: Text)
  , pretty ("import Data.Word (Word32, Word64)" :: Text)
  , pretty ("import qualified Data.Map.Strict as Map" :: Text)
  , pretty ("import qualified Data.Vector as V" :: Text)
  , pretty ("import qualified Data.Vector.Unboxed as VU" :: Text)
  , pretty ("import GHC.Generics (Generic)" :: Text)
  , pretty ("import Control.DeepSeq (NFData(..))" :: Text)
  , pretty ("import Proto.Encode" :: Text)
  , pretty ("import Proto.Decode" :: Text)
  ]

genTopLevel :: TopLevel -> [Doc ann]
genTopLevel = \case
  TLMessage msg ->
    genTypeDecls msg <>
    [ mempty
    , genDefaultInstance msg
    , mempty
    , genEncodeInstance msg
    , mempty
    , genDecodeInstance msg
    ]
  TLEnum ed ->
    [ genEnumDecl ed
    , mempty
    , genEnumProtoInstance ed
    ]
  TLService _svc -> []  -- service stubs could be generated here
  TLExtend _ _   -> []
  TLOption _     -> []

-- | Generate a default value instance for a message.
genDefaultInstance :: MessageDef -> Doc ann
genDefaultInstance msg =
  vsep
    [ pretty ("-- | Default (zero) values for all fields." :: Text)
    , pretty ("default" :: Text) <> pretty (hsTypeName (msgName msg)) <+> pretty ("::" :: Text) <+> pretty (hsTypeName (msgName msg))
    , pretty ("default" :: Text) <> pretty (hsTypeName (msgName msg)) <+> pretty ("=" :: Text) <+> pretty (hsTypeName (msgName msg))
    , indent 2 (genDefaultFields (msgElements msg))
    ]

genDefaultFields :: [MessageElement] -> Doc ann
genDefaultFields elems =
  let fields = concatMap extractDefault elems
  in case fields of
    []     -> pretty ("{ }" :: Text)
    (f:fs) -> vsep (pretty ("{ " :: Text) <> f : fmap (\x -> pretty (", " :: Text) <> x) fs) <> line <> pretty ("}" :: Text)
  where
    extractDefault = \case
      MEField fd ->
        [pretty (hsFieldName (fieldName fd)) <+> pretty ("=" :: Text) <+> defaultValue (fieldLabel fd) (fieldType fd)]
      MEMapField mf ->
        [pretty (hsFieldName (mapFieldName mf)) <+> pretty ("=" :: Text) <+> pretty ("Map.empty" :: Text)]
      MEOneof od ->
        [pretty (hsFieldName (oneofName od)) <+> pretty ("=" :: Text) <+> pretty ("Nothing" :: Text)]
      _ -> []

    defaultValue lbl ft = case lbl of
      Just Repeated -> case ft of
        FTScalar s | isUnboxableScalar s -> pretty ("VU.empty" :: Text)
        _                                -> pretty ("V.empty" :: Text)
      Just Optional -> pretty ("Nothing" :: Text)
      _ -> case ft of
        FTScalar SBool   -> pretty ("False" :: Text)
        FTScalar SString -> pretty ("\"\"" :: Text)
        FTScalar SBytes  -> pretty ("\"\"" :: Text)
        FTScalar _       -> pretty ("0" :: Text)
        FTNamed _        -> pretty ("Nothing" :: Text)

    isUnboxableScalar = \case
      SString -> False
      SBytes  -> False
      _       -> True

-- | Generate proto-aware Enum instance for enums (mapping to specific int values).
genEnumProtoInstance :: EnumDef -> Doc ann
genEnumProtoInstance ed =
  vsep
    [ pretty ("-- | Proto enum number mapping" :: Text)
    , pretty ("toProtoEnum" :: Text) <> pretty (hsTypeName (enumName ed)) <+>
      pretty ("::" :: Text) <+> pretty (hsTypeName (enumName ed)) <+> pretty ("-> Int" :: Text)
    , vsep (fmap genToProto (enumValues ed))
    , mempty
    , pretty ("fromProtoEnum" :: Text) <> pretty (hsTypeName (enumName ed)) <+>
      pretty ("::" :: Text) <+> pretty ("Int ->" :: Text) <+> pretty ("Maybe" :: Text) <+> pretty (hsTypeName (enumName ed))
    , vsep (fmap genFromProto (enumValues ed))
    , pretty ("fromProtoEnum" :: Text) <> pretty (hsTypeName (enumName ed)) <+>
      pretty ("_ = Nothing" :: Text)
    ]
  where
    genToProto ev =
      pretty ("toProtoEnum" :: Text) <> pretty (hsTypeName (enumName ed)) <+>
      pretty (hsEnumCon (enumName ed) (evName ev)) <+> pretty ("=" :: Text) <+>
      pretty (T.pack (show (evNumber ev)))

    genFromProto ev =
      pretty ("fromProtoEnum" :: Text) <> pretty (hsTypeName (enumName ed)) <+>
      pretty (T.pack (show (evNumber ev))) <+> pretty ("= Just" :: Text) <+>
      pretty (hsEnumCon (enumName ed) (evName ev))
