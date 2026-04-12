-- | Custom annotation and option handling for protobuf definitions.
--
-- Protobuf allows custom options on messages, fields, enums, services, RPCs,
-- and files. This module provides utilities for querying and extracting
-- custom option values from parsed proto definitions.
module Proto.Annotations
  ( -- * Option querying
    lookupOption
  , lookupSimpleOption
  , lookupExtensionOption
  , hasOption

    -- * Typed option extraction
  , optionAsInt
  , optionAsFloat
  , optionAsBool
  , optionAsString
  , optionAsIdent
  , optionAsAggregate

    -- * Bulk operations
  , allOptions
  , extensionOptions
  , simpleOptions

    -- * Custom annotation types
  , Annotation (..)
  , extractAnnotations
  , lookupAnnotation
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Proto.AST

-- | Look up an option by its full name (matching all name parts).
lookupOption :: Text -> [OptionDef] -> Maybe Constant
lookupOption name opts =
  case filter (matchesName name) opts of
    (o:_) -> Just (optValue o)
    []    -> Nothing

-- | Look up a simple (non-extension) option by name.
lookupSimpleOption :: Text -> [OptionDef] -> Maybe Constant
lookupSimpleOption name opts =
  case filter matchSimple opts of
    (o:_) -> Just (optValue o)
    []    -> Nothing
  where
    matchSimple o = case optNameParts (optName o) of
      [SimpleOption n] -> n == name
      _                -> False

-- | Look up an extension option by its fully qualified name.
lookupExtensionOption :: Text -> [OptionDef] -> Maybe Constant
lookupExtensionOption name opts =
  case filter matchExt opts of
    (o:_) -> Just (optValue o)
    []    -> Nothing
  where
    matchExt o = case optNameParts (optName o) of
      [ExtensionOption n] -> n == name
      _                   -> False

-- | Check if an option with the given name exists.
hasOption :: Text -> [OptionDef] -> Bool
hasOption name opts = case lookupOption name opts of
  Just _  -> True
  Nothing -> False

matchesName :: Text -> OptionDef -> Bool
matchesName name o = case optNameParts (optName o) of
  [SimpleOption n]    -> n == name
  [ExtensionOption n] -> n == name
  _                   -> False

-- | Extract integer value from a constant.
optionAsInt :: Constant -> Maybe Integer
optionAsInt (CInt n) = Just n
optionAsInt _        = Nothing

-- | Extract float value from a constant.
optionAsFloat :: Constant -> Maybe Double
optionAsFloat (CFloat n) = Just n
optionAsFloat _          = Nothing

-- | Extract bool value from a constant.
optionAsBool :: Constant -> Maybe Bool
optionAsBool (CBool b) = Just b
optionAsBool _         = Nothing

-- | Extract string value from a constant.
optionAsString :: Constant -> Maybe Text
optionAsString (CString s) = Just s
optionAsString _           = Nothing

-- | Extract identifier value from a constant.
optionAsIdent :: Constant -> Maybe Text
optionAsIdent (CIdent i) = Just i
optionAsIdent _          = Nothing

-- | Extract aggregate value from a constant.
optionAsAggregate :: Constant -> Maybe [(Text, Constant)]
optionAsAggregate (CAggregate kvs) = Just kvs
optionAsAggregate _                = Nothing

-- | Collect all options from a list.
allOptions :: [OptionDef] -> [(OptionName, Constant)]
allOptions = fmap (\o -> (optName o, optValue o))

-- | Collect only extension options.
extensionOptions :: [OptionDef] -> [(Text, Constant)]
extensionOptions = mapMaybe go
  where
    go o = case optNameParts (optName o) of
      [ExtensionOption n] -> Just (n, optValue o)
      _                   -> Nothing

-- | Collect only simple options.
simpleOptions :: [OptionDef] -> [(Text, Constant)]
simpleOptions = mapMaybe go
  where
    go o = case optNameParts (optName o) of
      [SimpleOption n] -> Just (n, optValue o)
      _                -> Nothing

-- | A resolved annotation (custom option with structured data).
data Annotation = Annotation
  { annotationName  :: !Text
  , annotationValue :: !Constant
  } deriving stock (Show, Eq)

-- | Extract all custom annotations (extension options) from a list of options.
extractAnnotations :: [OptionDef] -> [Annotation]
extractAnnotations = mapMaybe go
  where
    go o = case optNameParts (optName o) of
      [ExtensionOption n] -> Just Annotation
        { annotationName  = n
        , annotationValue = optValue o
        }
      _ -> Nothing

-- | Look up a specific annotation by name.
lookupAnnotation :: Text -> [Annotation] -> Maybe Constant
lookupAnnotation name anns =
  case filter (\a -> annotationName a == name) anns of
    (a:_) -> Just (annotationValue a)
    []    -> Nothing
