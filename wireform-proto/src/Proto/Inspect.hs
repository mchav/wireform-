{-# LANGUAGE TupleSections #-}
-- | AST inspection and query utilities for proto files.
--
-- Provides functions for navigating, searching, and extracting
-- information from parsed proto ASTs. Useful for tooling, linting,
-- documentation generation, and programmatic analysis of proto schemas.
module Proto.Inspect
  ( -- * Message queries
    allMessages
  , findMessage
  , messageFields
  , messageEnums
  , nestedMessages
  , messageOneofs
  , messageMapFields

    -- * Enum queries
  , allEnums
  , findEnum
  , enumValueByName
  , enumValueByNumber

    -- * Service queries
  , allServices
  , findService
  , serviceRpcs

    -- * Field queries
  , allFields
  , findField
  , fieldsByNumber
  , requiredFields
  , repeatedFields
  , optionalFields

    -- * Type queries
  , allTypeNames
  , referencedTypes
  , isScalarField
  , isMessageField

    -- * Option queries
  , fileOptions
  , messageOptions
  , fieldOptionsOf

    -- * Import queries
  , publicImports
  , allImportPaths

    -- * Structural summary
  , ProtoSummary (..)
  , summarize
  , prettyPrintSummary
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Proto.AST

-- | All top-level and nested message definitions, flattened.
allMessages :: ProtoFile -> [MessageDef]
allMessages pf = concatMap go (protoTopLevels pf)
  where
    go (TLMessage msg) = msg : concatMap goElem (msgElements msg)
    go _ = []
    goElem (MEMessage msg) = msg : concatMap goElem (msgElements msg)
    goElem _ = []

-- | Find a message by name (searches nested messages too).
findMessage :: Text -> ProtoFile -> Maybe MessageDef
findMessage name pf =
  case filter (\m -> msgName m == name) (allMessages pf) of
    (m:_) -> Just m
    []    -> Nothing

-- | All field definitions in a message (excluding oneofs and maps).
messageFields :: MessageDef -> [FieldDef]
messageFields msg = mapMaybe go (msgElements msg)
  where
    go (MEField fd) = Just fd
    go _ = Nothing

-- | All enum definitions nested inside a message.
messageEnums :: MessageDef -> [EnumDef]
messageEnums msg = mapMaybe go (msgElements msg)
  where
    go (MEEnum ed) = Just ed
    go _ = Nothing

-- | All nested message definitions (one level deep).
nestedMessages :: MessageDef -> [MessageDef]
nestedMessages msg = mapMaybe go (msgElements msg)
  where
    go (MEMessage m) = Just m
    go _ = Nothing

-- | All oneof definitions in a message.
messageOneofs :: MessageDef -> [OneofDef]
messageOneofs msg = mapMaybe go (msgElements msg)
  where
    go (MEOneof od) = Just od
    go _ = Nothing

-- | All map field definitions in a message.
messageMapFields :: MessageDef -> [MapField]
messageMapFields msg = mapMaybe go (msgElements msg)
  where
    go (MEMapField mf) = Just mf
    go _ = Nothing

-- | All top-level and nested enum definitions, flattened.
allEnums :: ProtoFile -> [EnumDef]
allEnums pf = concatMap go (protoTopLevels pf)
  where
    go (TLEnum ed) = [ed]
    go (TLMessage msg) = concatMap goElem (msgElements msg)
    go _ = []
    goElem (MEEnum ed) = [ed]
    goElem (MEMessage msg) = concatMap goElem (msgElements msg)
    goElem _ = []

-- | Find an enum by name.
findEnum :: Text -> ProtoFile -> Maybe EnumDef
findEnum name pf =
  case filter (\e -> enumName e == name) (allEnums pf) of
    (e:_) -> Just e
    []    -> Nothing

-- | Find an enum value by name within an enum.
enumValueByName :: Text -> EnumDef -> Maybe EnumValue
enumValueByName name ed =
  case filter (\v -> evName v == name) (enumValues ed) of
    (v:_) -> Just v
    []    -> Nothing

-- | Find an enum value by number within an enum.
enumValueByNumber :: Int -> EnumDef -> Maybe EnumValue
enumValueByNumber num ed =
  case filter (\v -> evNumber v == num) (enumValues ed) of
    (v:_) -> Just v
    []    -> Nothing

-- | All top-level service definitions.
allServices :: ProtoFile -> [ServiceDef]
allServices pf = mapMaybe go (protoTopLevels pf)
  where
    go (TLService svc) = Just svc
    go _ = Nothing

-- | Find a service by name.
findService :: Text -> ProtoFile -> Maybe ServiceDef
findService name pf =
  case filter (\s -> svcName s == name) (allServices pf) of
    (s:_) -> Just s
    []    -> Nothing

-- | All RPCs in a service.
serviceRpcs :: ServiceDef -> [RpcDef]
serviceRpcs = svcRpcs

-- | All fields across all messages, with their parent message name.
allFields :: ProtoFile -> [(Text, FieldDef)]
allFields pf = concatMap go (allMessages pf)
  where
    go msg = fmap (msgName msg,) (messageFields msg)

-- | Find a field by name within a message.
findField :: Text -> MessageDef -> Maybe FieldDef
findField name msg =
  case filter (\fd -> fieldName fd == name) (messageFields msg) of
    (fd:_) -> Just fd
    []     -> Nothing

-- | All fields indexed by field number.
fieldsByNumber :: MessageDef -> Map Int FieldDef
fieldsByNumber msg =
  Map.fromList (fmap (\fd -> (unFieldNumber (fieldNumber fd), fd)) (messageFields msg))

-- | All required fields in a message (proto2).
requiredFields :: MessageDef -> [FieldDef]
requiredFields = filter (\fd -> fieldLabel fd == Just Required) . messageFields

-- | All repeated fields in a message.
repeatedFields :: MessageDef -> [FieldDef]
repeatedFields = filter (\fd -> fieldLabel fd == Just Repeated) . messageFields

-- | All optional fields in a message.
optionalFields :: MessageDef -> [FieldDef]
optionalFields = filter (\fd -> fieldLabel fd == Just Optional) . messageFields

-- | All type names defined in a proto file (messages + enums).
allTypeNames :: ProtoFile -> [Text]
allTypeNames pf =
  fmap msgName (allMessages pf) <> fmap enumName (allEnums pf)

-- | All type names referenced by fields (named types only, not scalars).
referencedTypes :: ProtoFile -> [Text]
referencedTypes pf = concatMap goMsg (allMessages pf)
  where
    goMsg msg = mapMaybe goField (messageFields msg)
      <> concatMap goOneof (messageOneofs msg)
      <> mapMaybe goMap (messageMapFields msg)
    goField fd = case fieldType fd of
      FTNamed n -> Just n
      _         -> Nothing
    goOneof od = mapMaybe (\f -> case oneofFieldType f of
      FTNamed n -> Just n
      _         -> Nothing) (oneofFields od)
    goMap mf = case mapValueType mf of
      FTNamed n -> Just n
      _         -> Nothing

-- | Check if a field has a scalar type.
isScalarField :: FieldDef -> Bool
isScalarField fd = case fieldType fd of
  FTScalar _ -> True
  FTNamed _  -> False

-- | Check if a field has a message (named) type.
isMessageField :: FieldDef -> Bool
isMessageField = not . isScalarField

-- | All file-level options.
fileOptions :: ProtoFile -> [OptionDef]
fileOptions = protoOptions

-- | All options on a message.
messageOptions :: MessageDef -> [OptionDef]
messageOptions msg = mapMaybe go (msgElements msg)
  where
    go (MEOption opt) = Just opt
    go _ = Nothing

-- | Options on a specific field.
fieldOptionsOf :: FieldDef -> [OptionDef]
fieldOptionsOf = fieldOptions

-- | All public imports.
publicImports :: ProtoFile -> [ImportDef]
publicImports = filter (\i -> importModifier i == Just ImportPublic) . protoImports

-- | All import paths (strings).
allImportPaths :: ProtoFile -> [Text]
allImportPaths = fmap importPath . protoImports

-- | A structural summary of a proto file.
data ProtoSummary = ProtoSummary
  { summSyntax       :: !Syntax
  , summPackage      :: !(Maybe Text)
  , summImportCount  :: !Int
  , summMessageCount :: !Int
  , summEnumCount    :: !Int
  , summServiceCount :: !Int
  , summFieldCount   :: !Int
  , summRpcCount     :: !Int
  , summTypeNames    :: ![Text]
  } deriving stock (Show, Eq)

-- | Compute a structural summary of a proto file.
summarize :: ProtoFile -> ProtoSummary
summarize pf = ProtoSummary
  { summSyntax       = protoSyntax pf
  , summPackage      = protoPackage pf
  , summImportCount  = length (protoImports pf)
  , summMessageCount = length (allMessages pf)
  , summEnumCount    = length (allEnums pf)
  , summServiceCount = length (allServices pf)
  , summFieldCount   = length (allFields pf)
  , summRpcCount     = sum (fmap (length . svcRpcs) (allServices pf))
  , summTypeNames    = allTypeNames pf
  }

-- | Pretty-print a summary for human consumption.
prettyPrintSummary :: ProtoSummary -> Text
prettyPrintSummary s = T.unlines
  [ "Proto File Summary"
  , "  Syntax:   " <> T.pack (show (summSyntax s))
  , "  Package:  " <> fromMaybe "(none)" (summPackage s)
  , "  Imports:  " <> T.pack (show (summImportCount s))
  , "  Messages: " <> T.pack (show (summMessageCount s))
  , "  Enums:    " <> T.pack (show (summEnumCount s))
  , "  Services: " <> T.pack (show (summServiceCount s))
  , "  Fields:   " <> T.pack (show (summFieldCount s))
  , "  RPCs:     " <> T.pack (show (summRpcCount s))
  , "  Types:    " <> T.intercalate ", " (summTypeNames s)
  ]
