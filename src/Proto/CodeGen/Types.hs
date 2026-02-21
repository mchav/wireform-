-- | Code generation for Haskell type definitions from proto messages.
--
-- All generated names are scoped to avoid conflicts:
-- * Nested messages/enums are prefixed with parent name: Person'Address
-- * Record fields are prefixed with message name: personName, personId
-- * Oneof types are prefixed: Person'Contact
-- * Enum constructors include the enum name: PhoneType'Mobile
module Proto.CodeGen.Types
  ( genTypeDecls
  , genEnumDecl
  , genOneofDecl
  , hsTypeName
  , hsFieldName
  , hsEnumCon
  , hsModuleName
  , hsScopedTypeName
  , hsScopedFieldName
  , hsScopedEnumCon
  ) where

import Data.Char (toLower, toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter

import Proto.AST

-- | Generate all type declarations for a message and its nested types.
-- The scope parameter is the chain of parent message names (empty at top level).
genTypeDecls :: MessageDef -> [Doc ann]
genTypeDecls = genTypeDeclsScoped []

genTypeDeclsScoped :: [Text] -> MessageDef -> [Doc ann]
genTypeDeclsScoped scope msg =
  let scope' = scope <> [msgName msg]
  in genMessageRecord scope' msg : concatMap (genNested scope') (msgElements msg)
  where
    genNested s = \case
      MEMessage inner -> genTypeDeclsScoped s inner
      MEEnum e        -> [genEnumDecl' s e]
      MEOneof o       -> [genOneofDecl' s o]
      _               -> []

genMessageRecord :: [Text] -> MessageDef -> Doc ann
genMessageRecord scope msg =
  let tyN = scopedTypeName scope
  in vsep
    [ pretty ("data" :: Text) <+> pretty tyN <+> pretty ("=" :: Text) <+> pretty tyN
    , indent 2 (braceFields (concatMap (extractField scope) (msgElements msg)))
    , indent 2 (pretty ("deriving stock (Show, Eq, Generic)" :: Text))
    ]
  where
    braceFields [] = pretty ("{ }" :: Text)
    braceFields (f:fs) =
      vsep (pretty ("{ " :: Text) <> f : fmap (\x -> pretty (", " :: Text) <> x) fs) <> line <> pretty ("}" :: Text)

    extractField s = \case
      MEField fd  -> [genFieldDeclWithDoc s fd]
      MEMapField mf -> [genMapFieldDecl s mf]
      MEOneof od  -> [genOneofFieldRef s od]
      _           -> []

-- | Scoped type name: ["Person", "Address"] -> "Person'Address"
scopedTypeName :: [Text] -> Text
scopedTypeName = T.intercalate "'" . fmap hsTypeName

-- | Public API: scoped type name from a list of parent names + the type name.
hsScopedTypeName :: [Text] -> Text -> Text
hsScopedTypeName parents name = scopedTypeName (parents <> [name])

-- | Scoped field name: scope=["Person"], field="name" -> "personName"
scopedFieldName :: [Text] -> Text -> Text
scopedFieldName scope fName =
  let prefix = case scope of
        []    -> ""
        (s:_) -> titleLowerFirst (hsTypeName s)
  in escapeReserved (prefix <> titleCase (snakeToCamel fName))

-- | Public API: scoped field name.
hsScopedFieldName :: [Text] -> Text -> Text
hsScopedFieldName = scopedFieldName

genFieldDeclWithDoc :: [Text] -> FieldDef -> Doc ann
genFieldDeclWithDoc scope fd =
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
  in doc <> line <> genFieldDecl scope fd

genFieldDecl :: [Text] -> FieldDef -> Doc ann
genFieldDecl scope fd =
  pretty (scopedFieldName scope (fieldName fd)) <+> pretty ("::" :: Text) <+>
  unpackAnnotation ft lbl <> hsFieldType scope ft lbl
  where
    ft = fieldType fd
    lbl = fieldLabel fd

genMapFieldDecl :: [Text] -> MapField -> Doc ann
genMapFieldDecl scope mf =
  pretty (scopedFieldName scope (mapFieldName mf)) <+> pretty ("::" :: Text) <+>
  pretty ("!(Map.Map" :: Text) <+> hsScalarType (mapKeyType mf) <+>
  hsFieldTypeInner scope (mapValueType mf) <> pretty (")" :: Text)

genOneofFieldRef :: [Text] -> OneofDef -> Doc ann
genOneofFieldRef scope od =
  pretty (scopedFieldName scope (oneofName od)) <+> pretty ("::" :: Text) <+>
  pretty ("!(Maybe" :: Text) <+> pretty (scopedTypeName (scope <> [oneofName od])) <> pretty (")" :: Text)

-- | Generate a sum type for a oneof.
genOneofDecl :: Text -> OneofDef -> Doc ann
genOneofDecl parentName = genOneofDecl' [parentName]

genOneofDecl' :: [Text] -> OneofDef -> Doc ann
genOneofDecl' scope od =
  let tyN = scopedTypeName (scope <> [oneofName od])
  in vsep
    [ pretty ("data" :: Text) <+> pretty tyN
    , indent 2 (vsep (zipWith (\pfx f -> pfx <+> genOneofCon scope f) seps (oneofFields od)))
    , indent 2 (pretty ("deriving stock (Show, Eq, Generic)" :: Text))
    ]
  where
    seps = pretty ("=" :: Text) : repeat (pretty ("|" :: Text))
    genOneofCon s f =
      pretty (scopedTypeName (s <> [oneofFieldName f])) <+>
      hsOneofFieldType s (oneofFieldType f)

-- | Generate an enum definition.
genEnumDecl :: EnumDef -> Doc ann
genEnumDecl = genEnumDecl' []

genEnumDecl' :: [Text] -> EnumDef -> Doc ann
genEnumDecl' scope ed =
  let tyN = scopedTypeName (scope <> [enumName ed])
  in vsep
    [ pretty ("data" :: Text) <+> pretty tyN
    , indent 2 (vsep (zipWith (\pfx v -> pfx <+> pretty (hsScopedEnumCon scope (enumName ed) (evName v))) seps (enumValues ed)))
    , indent 2 (pretty ("deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)" :: Text))
    ]
  where
    seps = pretty ("=" :: Text) : repeat (pretty ("|" :: Text))

-- | Scoped enum constructor: scope=["Msg"], enum="Status", val="ACTIVE" -> "Msg'Active"
hsScopedEnumCon :: [Text] -> Text -> Text -> Text
hsScopedEnumCon scope _enumName valName =
  case scope of
    [] -> snakeToPascal valName
    _  -> scopedTypeName scope <> "'" <> snakeToPascal valName

-- | The Haskell type for a field, with appropriate wrapping.
hsFieldType :: [Text] -> FieldType -> Maybe FieldLabel -> Doc ann
hsFieldType scope ft = \case
  Just Repeated -> hsRepeatedType scope ft
  Just Optional -> pretty ("!(Maybe" :: Text) <+> hsFieldTypeInner scope ft <> pretty (")" :: Text)
  Just Required -> pretty ("!" :: Text) <> hsFieldTypeInner scope ft
  Nothing       -> hsFieldTypeInner scope ft

hsFieldTypeInner :: [Text] -> FieldType -> Doc ann
hsFieldTypeInner scope = \case
  FTScalar s -> hsScalarType s
  FTNamed n  -> pretty (hsTypeName n)

hsOneofFieldType :: [Text] -> FieldType -> Doc ann
hsOneofFieldType scope = \case
  FTScalar s -> unpackPragma s <> hsScalarType s
  FTNamed n  -> pretty ("!" :: Text) <> pretty (hsTypeName n)

hsRepeatedType :: [Text] -> FieldType -> Doc ann
hsRepeatedType scope = \case
  FTScalar s | isUnboxable s -> pretty ("!(VU.Vector" :: Text) <+> hsScalarType s <> pretty (")" :: Text)
  ft -> pretty ("!(V.Vector" :: Text) <+> hsFieldTypeInner scope ft <> pretty (")" :: Text)

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

-- | Convert a proto name to a Haskell type name (PascalCase).
hsTypeName :: Text -> Text
hsTypeName t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing        -> t

-- | Convert a proto field name to a Haskell record field name (no prefix).
-- For the prefixed version, use 'hsScopedFieldName'.
hsFieldName :: Text -> Text
hsFieldName = escapeReserved . snakeToCamel

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
