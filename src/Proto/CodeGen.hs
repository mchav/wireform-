-- | Code generation for Haskell modules from parsed proto files.
--
-- Generates complete, compilable Haskell modules with:
-- * Plain record types (no lenses)
-- * Strict fields with UNPACK pragmas for primitives
-- * Specialized MessageEncode/MessageDecode/MessageSize instances
-- * Enum types with proto-value-aware conversion
-- * Oneof types as sum types
-- * Default value constructors
-- * Two-pass encoding: size computation then serialization (Buf-style)
-- * Lazy submessage decoding option
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
import Proto.CodeGen.Encode (genEncodeInstance, genSizeInstance)
import Proto.CodeGen.Decode (genDecodeInstance)

-- | Options controlling code generation.
data GenerateOpts = GenerateOpts
  { genModulePrefix    :: Text
  , genStrictFields    :: Bool
  , genUnpackPrims     :: Bool
  , genDeriveGeneric   :: Bool
  , genDeriveNFData    :: Bool
  , genPackedRepeated  :: Bool
  , genLazySubmessages :: Bool
  } deriving stock (Show, Eq)

defaultGenerateOpts :: GenerateOpts
defaultGenerateOpts = GenerateOpts
  { genModulePrefix    = "Proto.Gen"
  , genStrictFields    = True
  , genUnpackPrims     = True
  , genDeriveGeneric   = True
  , genDeriveNFData    = True
  , genPackedRepeated  = True
  , genLazySubmessages = False
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
    , pretty ("{-# LANGUAGE BangPatterns #-}" :: Text)
    , pretty ("module" :: Text) <+> pretty modName <+> pretty ("where" :: Text)
    ]

genImports :: Doc ann
genImports = vsep
  [ pretty ("import Data.ByteString (ByteString)" :: Text)
  , pretty ("import qualified Data.ByteString as BS" :: Text)
  , pretty ("import qualified Data.ByteString.Builder as B" :: Text)
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
  , pretty ("import Proto.Wire (Tag(..), WireType(..))" :: Text)
  , pretty ("import Proto.Wire.Encode (putTag, putVarint, putFixed32, putFixed64," :: Text)
  , pretty ("  putFloat, putDouble, putText, putByteString, putLengthDelimited," :: Text)
  , pretty ("  putSVarint32, putSVarint64, putVarintSigned," :: Text)
  , pretty ("  varintSize, tagSize, fieldMessageSize)" :: Text)
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
    , genMessageSizeInstance msg
    , mempty
    , genDecodeInstance msg
    ]
  TLEnum ed ->
    [ genEnumDecl ed
    , mempty
    , genEnumProtoInstance ed
    ]
  TLService _svc -> []
  TLExtend _ _   -> []
  TLOption _     -> []

-- | Generate a default value constructor for a message.
genDefaultInstance :: MessageDef -> Doc ann
genDefaultInstance msg =
  vsep
    [ pretty ("default" :: Text) <> pretty (hsTypeName (msgName msg)) <+> pretty ("::" :: Text) <+> pretty (hsTypeName (msgName msg))
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

genMessageSizeInstance :: MessageDef -> Doc ann
genMessageSizeInstance = genSizeInstance

-- | Generate proto-aware Enum instance for enums.
genEnumProtoInstance :: EnumDef -> Doc ann
genEnumProtoInstance ed =
  vsep
    [ pretty ("toProtoEnum" :: Text) <> pretty (hsTypeName (enumName ed)) <+>
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
