-- | Custom option extension support.
--
-- Protobuf allows defining custom options by extending the standard option
-- message types (FileOptions, MessageOptions, FieldOptions, etc.).
-- This module provides utilities for extracting and resolving custom options
-- from parsed proto files.
--
-- Example proto:
--
-- @
-- import "google/protobuf/descriptor.proto";
--
-- extend google.protobuf.FieldOptions {
--   optional string my_option = 51234;
-- }
--
-- message MyMessage {
--   string name = 1 [(my_option) = "special"];
-- }
-- @
module Proto.Options.Custom
  ( -- * Custom option registry
    CustomOptionRegistry
  , emptyCustomOptionRegistry
  , registerCustomOption
  , lookupCustomOption

    -- * Custom option descriptor
  , CustomOptionDef (..)

    -- * Extraction
  , extractCustomOption
  , extractCustomOptions
  , extractExtensionOptions
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Proto.AST

-- | A registered custom option descriptor.
data CustomOptionDef = CustomOptionDef
  { codName       :: !Text
  , codFieldNum   :: !Int
  , codExtendType :: !Text
  , codFieldType  :: !FieldType
  } deriving stock (Show, Eq)

-- | Registry mapping extension option names to their definitions.
newtype CustomOptionRegistry = CustomOptionRegistry
  { unRegistry :: Map Text CustomOptionDef
  } deriving stock (Show, Eq)

emptyCustomOptionRegistry :: CustomOptionRegistry
emptyCustomOptionRegistry = CustomOptionRegistry Map.empty

-- | Register a custom option extension.
registerCustomOption :: CustomOptionDef -> CustomOptionRegistry -> CustomOptionRegistry
registerCustomOption cod (CustomOptionRegistry m) =
  CustomOptionRegistry (Map.insert (codName cod) cod m)

-- | Look up a custom option by name.
lookupCustomOption :: Text -> CustomOptionRegistry -> Maybe CustomOptionDef
lookupCustomOption name (CustomOptionRegistry m) = Map.lookup name m

-- | Extract a specific custom option value from a list of options.
extractCustomOption :: Text -> [OptionDef] -> Maybe Constant
extractCustomOption name opts =
  case filter (matchesExtension name) opts of
    (o:_) -> Just (optValue o)
    []    -> Nothing
  where
    matchesExtension n o = case optNameParts (optName o) of
      [ExtensionOption en] -> en == n
      _ -> False

-- | Extract all custom (extension) options from a list.
extractCustomOptions :: [OptionDef] -> [(Text, Constant)]
extractCustomOptions = concatMap go
  where
    go o = case optNameParts (optName o) of
      [ExtensionOption name] -> [(name, optValue o)]
      _ -> []

-- | Extract custom option definitions from extend blocks in a proto file.
-- These are the definitions themselves, not the usage of options.
extractExtensionOptions :: ProtoFile -> [CustomOptionDef]
extractExtensionOptions pf = concatMap go (protoTopLevels pf)
  where
    go (TLExtend typeName fields) =
      fmap (\fd -> CustomOptionDef
        { codName = fieldName fd
        , codFieldNum = unFieldNumber (fieldNumber fd)
        , codExtendType = typeName
        , codFieldType = fieldType fd
        }) fields
    go _ = []
