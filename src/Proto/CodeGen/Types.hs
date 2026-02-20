-- | Code generation for Haskell type definitions from proto messages.
--
-- Generates plain Haskell records with strict fields and UNPACK pragmas
-- for primitive types. Uses Vector for repeated fields and Maybe for
-- optional message fields.
module Proto.CodeGen.Types
  ( genTypeDecls
  , genEnumDecl
  , genOneofDecl
  , hsTypeName
  , hsFieldName
  , hsEnumCon
  , hsModuleName
  ) where

import Data.Char (toLower, toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter

import Proto.AST

-- | Generate all type declarations for a message and its nested types.
genTypeDecls :: MessageDef -> [Doc ann]
genTypeDecls msg = mainDecl : nestedDecls
  where
    mainDecl = genMessageRecord msg
    nestedDecls = concatMap genNested (msgElements msg)
    genNested = \case
      MEMessage inner -> genTypeDecls inner
      MEEnum e        -> [genEnumDecl e]
      MEOneof o       -> [genOneofDecl (msgName msg) o]
      _               -> []

genMessageRecord :: MessageDef -> Doc ann
genMessageRecord msg =
  vsep
    [ pretty ("data" :: Text) <+> pretty (hsTypeName (msgName msg)) <+> pretty ("=" :: Text) <+> pretty (hsTypeName (msgName msg))
    , indent 2 (braceFields fields)
    , indent 2 derivingClause
    ]
  where
    fields = concatMap extractField (msgElements msg)

    extractField = \case
      MEField fd      -> [genFieldDeclWithDoc fd]
      MEMapField mf   -> [genMapFieldDecl mf]
      MEOneof od      -> [genOneofFieldRef (msgName msg) od]
      _               -> []

    braceFields [] = pretty ("{ }" :: Text)
    braceFields (f:fs) =
      vsep (pretty ("{ " :: Text) <> f : fmap (\x -> pretty (", " :: Text) <> x) fs) <> line <> pretty ("}" :: Text)

    derivingClause = pretty ("deriving stock (Show, Eq, Generic)" :: Text)

genFieldDeclWithDoc :: FieldDef -> Doc ann
genFieldDeclWithDoc fd =
  let labelTxt :: Text
      labelTxt = case fieldLabel fd of
        Just Optional -> "optional "
        Just Required -> "required "
        Just Repeated -> "repeated "
        Nothing       -> ""
      typeTxt = showFieldTypeForDoc (fieldType fd)
      num = T.pack (show (unFieldNumber (fieldNumber fd)))
      jsonNote = case lookupJsonName (fieldOptions fd) of
        Nothing -> mempty
        Just jn -> line <> pretty ("-- JSON name: @" :: Text) <> pretty jn <> pretty ("@" :: Text)
      deprNote = if isFieldDeprecated (fieldOptions fd)
        then line <> pretty ("--" :: Text) <> line <> pretty ("-- __Deprecated__" :: Text)
        else mempty
      doc = pretty ("-- | Proto field: @" :: Text) <> pretty labelTxt <>
            pretty typeTxt <> pretty (" " :: Text) <> pretty (fieldName fd) <>
            pretty (" = " :: Text) <> pretty num <> pretty ("@" :: Text) <>
            jsonNote <> deprNote
  in doc <> line <> genFieldDecl fd

lookupJsonName :: [OptionDef] -> Maybe Text
lookupJsonName opts = do
  val <- lookupSimpleOption' "json_name" opts
  case val of
    CString s -> Just s
    _         -> Nothing

lookupSimpleOption' :: Text -> [OptionDef] -> Maybe Constant
lookupSimpleOption' name opts =
  case filter matchSimple opts of
    (o:_) -> Just (optValue o)
    []    -> Nothing
  where
    matchSimple o = case optNameParts (optName o) of
      [SimpleOption n] -> n == name
      _                -> False

isFieldDeprecated :: [OptionDef] -> Bool
isFieldDeprecated opts = case lookupSimpleOption' "deprecated" opts of
  Just (CBool True) -> True
  _                 -> False

showFieldTypeForDoc :: FieldType -> Text
showFieldTypeForDoc = \case
  FTScalar SDouble   -> "double"
  FTScalar SFloat    -> "float"
  FTScalar SInt32    -> "int32"
  FTScalar SInt64    -> "int64"
  FTScalar SUInt32   -> "uint32"
  FTScalar SUInt64   -> "uint64"
  FTScalar SSInt32   -> "sint32"
  FTScalar SSInt64   -> "sint64"
  FTScalar SFixed32  -> "fixed32"
  FTScalar SFixed64  -> "fixed64"
  FTScalar SSFixed32 -> "sfixed32"
  FTScalar SSFixed64 -> "sfixed64"
  FTScalar SBool     -> "bool"
  FTScalar SString   -> "string"
  FTScalar SBytes    -> "bytes"
  FTNamed n          -> n

genFieldDecl :: FieldDef -> Doc ann
genFieldDecl fd =
  pretty (hsFieldName (fieldName fd)) <+> pretty ("::" :: Text) <+> unpackAnnotation ft lbl <> hsFieldType ft lbl
  where
    ft = fieldType fd
    lbl = fieldLabel fd

genMapFieldDecl :: MapField -> Doc ann
genMapFieldDecl mf =
  pretty (hsFieldName (mapFieldName mf)) <+> pretty ("::" :: Text) <+>
  pretty ("!(Map.Map" :: Text) <+> hsScalarType (mapKeyType mf) <+> hsFieldTypeInner (mapValueType mf) <> pretty (")" :: Text)

genOneofFieldRef :: Text -> OneofDef -> Doc ann
genOneofFieldRef _msgName od =
  pretty (hsFieldName (oneofName od)) <+> pretty ("::" :: Text) <+>
  pretty ("!(Maybe" :: Text) <+> pretty (hsTypeName (oneofName od)) <> pretty (")" :: Text)

-- | Generate a sum type for a oneof.
genOneofDecl :: Text -> OneofDef -> Doc ann
genOneofDecl _parentName od =
  vsep
    [ pretty ("data" :: Text) <+> pretty (hsTypeName (oneofName od))
    , indent 2 (vsep (zipWith (\pfx f -> pfx <+> genOneofCon f) seps (oneofFields od)))
    , indent 2 (pretty ("deriving stock (Show, Eq, Generic)" :: Text))
    ]
  where
    seps = pretty ("=" :: Text) : repeat (pretty ("|" :: Text))
    genOneofCon f =
      pretty (hsTypeName (oneofFieldName f)) <+>
      hsOneofFieldType (oneofFieldType f)

-- | Generate an enum definition.
genEnumDecl :: EnumDef -> Doc ann
genEnumDecl ed =
  vsep
    [ pretty ("data" :: Text) <+> pretty (hsTypeName (enumName ed))
    , indent 2 (vsep (zipWith (\pfx v -> pfx <+> pretty (hsEnumCon (enumName ed) (evName v))) seps (enumValues ed)))
    , indent 2 (pretty ("deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)" :: Text))
    ]
  where
    seps = pretty ("=" :: Text) : repeat (pretty ("|" :: Text))

-- | The Haskell type for a field, with appropriate wrapping.
hsFieldType :: FieldType -> Maybe FieldLabel -> Doc ann
hsFieldType ft = \case
  Just Repeated -> hsRepeatedType ft
  Just Optional -> pretty ("!(Maybe" :: Text) <+> hsFieldTypeInner ft <> pretty (")" :: Text)
  Just Required -> pretty ("!" :: Text) <> hsFieldTypeInner ft
  Nothing       -> hsFieldTypeInner ft  -- proto3: singular

hsFieldTypeInner :: FieldType -> Doc ann
hsFieldTypeInner = \case
  FTScalar s -> hsScalarType s
  FTNamed n  -> pretty (hsTypeName n)

hsOneofFieldType :: FieldType -> Doc ann
hsOneofFieldType = \case
  FTScalar s -> unpackPragma s <> hsScalarType s
  FTNamed n  -> pretty ("!" :: Text) <> pretty (hsTypeName n)

hsRepeatedType :: FieldType -> Doc ann
hsRepeatedType = \case
  FTScalar s | isUnboxable s -> pretty ("!(VU.Vector" :: Text) <+> hsScalarType s <> pretty (")" :: Text)
  ft -> pretty ("!(V.Vector" :: Text) <+> hsFieldTypeInner ft <> pretty (")" :: Text)

hsScalarType :: ScalarType -> Doc ann
hsScalarType = \case
  SDouble   -> pretty ("Double" :: Text)
  SFloat    -> pretty ("Float" :: Text)
  SInt32    -> pretty ("Int32" :: Text)
  SInt64    -> pretty ("Int64" :: Text)
  SUInt32   -> pretty ("Word32" :: Text)
  SUInt64   -> pretty ("Word64" :: Text)
  SSInt32   -> pretty ("Int32" :: Text)
  SSInt64   -> pretty ("Int64" :: Text)
  SFixed32  -> pretty ("Word32" :: Text)
  SFixed64  -> pretty ("Word64" :: Text)
  SSFixed32 -> pretty ("Int32" :: Text)
  SSFixed64 -> pretty ("Int64" :: Text)
  SBool     -> pretty ("Bool" :: Text)
  SString   -> pretty ("Text" :: Text)
  SBytes    -> pretty ("ByteString" :: Text)

isUnboxable :: ScalarType -> Bool
isUnboxable = \case
  SString -> False
  SBytes  -> False
  _       -> True

unpackAnnotation :: FieldType -> Maybe FieldLabel -> Doc ann
unpackAnnotation ft lbl = case lbl of
  Just Repeated -> mempty
  Just Optional -> mempty
  _ -> case ft of
    FTScalar s | isUnboxable s -> pretty ("{-# UNPACK #-} " :: Text)
    _                          -> pretty ("!" :: Text) <> mempty

unpackPragma :: ScalarType -> Doc ann
unpackPragma s
  | isUnboxable s = pretty ("{-# UNPACK #-} !" :: Text)
  | otherwise     = pretty ("!" :: Text)

-- | Convert a proto name to a Haskell type name (PascalCase).
hsTypeName :: Text -> Text
hsTypeName t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing        -> t

-- | Convert a proto field name to a Haskell record field name.
-- Uses camelCase convention and escapes Haskell reserved words.
hsFieldName :: Text -> Text
hsFieldName = escapeReserved . snakeToCamel

escapeReserved :: Text -> Text
escapeReserved t
  | t `elem` haskellReserved = t <> "'"
  | otherwise = t

haskellReserved :: [Text]
haskellReserved =
  [ "type", "class", "data", "default", "deriving", "do", "else"
  , "if", "import", "in", "infix", "infixl", "infixr", "instance"
  , "let", "module", "newtype", "of", "then", "where", "case"
  , "foreign", "forall", "mdo", "qualified", "hiding"
  ]

-- | Convert a proto enum value name to a Haskell constructor.
hsEnumCon :: Text -> Text -> Text
hsEnumCon _enumName valName = snakeToPascal valName

-- | Convert a proto package name to a Haskell module name.
hsModuleName :: Text -> Text
hsModuleName = T.intercalate (T.singleton '.') . fmap capitalize . T.splitOn (T.singleton '.')
  where
    capitalize t = case T.uncons t of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing        -> t

snakeToCamel :: Text -> Text
snakeToCamel t =
  let parts = T.splitOn (T.singleton '_') t
  in case parts of
    []     -> t
    (p:ps) -> T.concat (titleLowerFirst p : fmap titleCase ps)

snakeToPascal :: Text -> Text
snakeToPascal t =
  let parts = T.splitOn (T.singleton '_') t
  in T.concat (fmap titleCase parts)

titleCase :: Text -> Text
titleCase s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
  Nothing        -> s

titleLowerFirst :: Text -> Text
titleLowerFirst s = case T.uncons s of
  Just (c, rest) -> T.cons (toLower c) (T.toLower rest)
  Nothing        -> s
