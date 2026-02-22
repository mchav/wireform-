-- | Attribute-driven codegen hook system.
--
-- This module provides a mechanism for custom protobuf attributes (options)
-- to influence Haskell code generation. Users register hooks that inspect
-- options on proto elements and produce additional Haskell code.
--
-- == Usage
--
-- @
-- import Proto.CodeGen.Hooks
-- import Proto.CodeGen (GenerateOpts(..), defaultGenerateOpts)
--
-- myHooks :: CodeGenHooks
-- myHooks = mempty
--   { onMessageCodeGen = \\ctx ->
--       if hasAttribute \"(generate_lens)\" (mhcOptions ctx)
--         then [\"-- TODO: generate lenses for \" <> mhcHsTypeName ctx]
--         else []
--   }
--
-- opts :: GenerateOpts
-- opts = defaultGenerateOpts { genHooks = myHooks }
-- @
--
-- Hooks are composable via their 'Semigroup' and 'Monoid' instances, so
-- multiple independent hooks can be combined with @(<>)@.
--
-- == Attribute matching
--
-- The 'onAttribute' and 'onMessageAttribute' combinators create hooks
-- triggered by specific named extension options:
--
-- @
-- lensHook :: CodeGenHooks
-- lensHook = onMessageAttribute \"generate_lens\" $ \\val ctx ->
--   case val of
--     CBool True -> [\"-- generate lenses for \" <> mhcHsTypeName ctx]
--     _          -> []
-- @
module Proto.CodeGen.Hooks
  ( -- * Hook record
    CodeGenHooks (..)
  , defaultCodeGenHooks

    -- * Hook contexts
  , FileHookCtx (..)
  , MessageHookCtx (..)
  , EnumHookCtx (..)
  , ServiceHookCtx (..)

    -- * Attribute-driven hook constructors
  , onMessageAttribute
  , onEnumAttribute
  , onServiceAttribute
  , onFileAttribute

    -- * Querying attributes in hooks
  , lookupAttribute
  , hasAttribute
  , attributeAsText
  , attributeAsBool
  , attributeAsInt
  , attributeAsFloat
  , attributeAsAggregate

    -- * Extracting options from message elements
  , messageOptions
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Proto.AST

-- ---------------------------------------------------------------------------
-- Hook contexts
-- ---------------------------------------------------------------------------

-- | Context passed to file-level hooks.
data FileHookCtx = FileHookCtx
  { fhcProtoFile   :: !ProtoFile
  , fhcModuleName  :: !Text
  , fhcFileOptions :: ![OptionDef]
  } deriving stock (Show)

-- | Context passed to message-level hooks.
--
-- Includes the full 'MessageDef' so hooks can inspect fields, nested types,
-- and message-level options.
data MessageHookCtx = MessageHookCtx
  { mhcMessageDef  :: !MessageDef
  , mhcScope       :: ![Text]
  , mhcHsTypeName  :: !Text
  , mhcFqProtoName :: !Text
  , mhcOptions     :: ![OptionDef]
  } deriving stock (Show)

-- | Context passed to enum-level hooks.
data EnumHookCtx = EnumHookCtx
  { ehcEnumDef    :: !EnumDef
  , ehcScope      :: ![Text]
  , ehcHsTypeName :: !Text
  , ehcOptions    :: ![OptionDef]
  } deriving stock (Show)

-- | Context passed to service-level hooks.
data ServiceHookCtx = ServiceHookCtx
  { shcServiceDef :: !ServiceDef
  , shcScope      :: ![Text]
  , shcHsTypeName :: !Text
  , shcOptions    :: ![OptionDef]
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Hook record
-- ---------------------------------------------------------------------------

-- | Collection of codegen hooks. Each field is a function that receives a
-- context and returns extra lines of Haskell code to emit at the
-- corresponding point in the generated module.
--
-- Use 'defaultCodeGenHooks' or 'mempty' for no-op hooks, then override
-- the fields you need. Combine multiple hooks with @(<>)@.
data CodeGenHooks = CodeGenHooks
  { onFileCodeGen    :: !(FileHookCtx -> [Text])
  , onMessageCodeGen :: !(MessageHookCtx -> [Text])
  , onEnumCodeGen    :: !(EnumHookCtx -> [Text])
  , onServiceCodeGen :: !(ServiceHookCtx -> [Text])
  }

-- | No-op hooks (produce no extra code).
defaultCodeGenHooks :: CodeGenHooks
defaultCodeGenHooks = CodeGenHooks
  { onFileCodeGen    = const []
  , onMessageCodeGen = const []
  , onEnumCodeGen    = const []
  , onServiceCodeGen = const []
  }

instance Semigroup CodeGenHooks where
  a <> b = CodeGenHooks
    { onFileCodeGen    = \ctx -> onFileCodeGen a ctx    <> onFileCodeGen b ctx
    , onMessageCodeGen = \ctx -> onMessageCodeGen a ctx <> onMessageCodeGen b ctx
    , onEnumCodeGen    = \ctx -> onEnumCodeGen a ctx    <> onEnumCodeGen b ctx
    , onServiceCodeGen = \ctx -> onServiceCodeGen a ctx <> onServiceCodeGen b ctx
    }

instance Monoid CodeGenHooks where
  mempty = defaultCodeGenHooks

-- ---------------------------------------------------------------------------
-- Attribute-driven hook constructors
-- ---------------------------------------------------------------------------

-- | Create a hook that fires on messages bearing a specific extension option.
--
-- @
-- onMessageAttribute \"my_annotation\" $ \\val ctx ->
--   [\"-- annotated: \" <> mhcHsTypeName ctx]
-- @
onMessageAttribute :: Text -> (Constant -> MessageHookCtx -> [Text]) -> CodeGenHooks
onMessageAttribute attrName f = mempty
  { onMessageCodeGen = \ctx ->
      case lookupAttribute attrName (mhcOptions ctx) of
        Just val -> f val ctx
        Nothing  -> []
  }

-- | Create a hook that fires on enums bearing a specific extension option.
onEnumAttribute :: Text -> (Constant -> EnumHookCtx -> [Text]) -> CodeGenHooks
onEnumAttribute attrName f = mempty
  { onEnumCodeGen = \ctx ->
      case lookupAttribute attrName (ehcOptions ctx) of
        Just val -> f val ctx
        Nothing  -> []
  }

-- | Create a hook that fires on services bearing a specific extension option.
onServiceAttribute :: Text -> (Constant -> ServiceHookCtx -> [Text]) -> CodeGenHooks
onServiceAttribute attrName f = mempty
  { onServiceCodeGen = \ctx ->
      case lookupAttribute attrName (shcOptions ctx) of
        Just val -> f val ctx
        Nothing  -> []
  }

-- | Create a hook that fires when a file-level extension option is present.
onFileAttribute :: Text -> (Constant -> FileHookCtx -> [Text]) -> CodeGenHooks
onFileAttribute attrName f = mempty
  { onFileCodeGen = \ctx ->
      case lookupAttribute attrName (fhcFileOptions ctx) of
        Just val -> f val ctx
        Nothing  -> []
  }

-- ---------------------------------------------------------------------------
-- Attribute querying
-- ---------------------------------------------------------------------------

-- | Look up an extension (custom) option by name from a list of options.
-- Searches for options enclosed in parentheses, e.g. @(my_option)@.
lookupAttribute :: Text -> [OptionDef] -> Maybe Constant
lookupAttribute name opts =
  case filter (matchesExtension name) opts of
    (o:_) -> Just (optValue o)
    []    -> Nothing
  where
    matchesExtension n o = case optNameParts (optName o) of
      [ExtensionOption en] -> en == n
      _ -> False

-- | Check whether an extension option with the given name exists.
hasAttribute :: Text -> [OptionDef] -> Bool
hasAttribute name opts = case lookupAttribute name opts of
  Just _  -> True
  Nothing -> False

-- | Extract a 'Text' value from an attribute, if present and string-typed.
attributeAsText :: Text -> [OptionDef] -> Maybe Text
attributeAsText name opts = lookupAttribute name opts >>= extractString
  where
    extractString (CString s) = Just s
    extractString _           = Nothing

-- | Extract a 'Bool' value from an attribute, if present and bool-typed.
attributeAsBool :: Text -> [OptionDef] -> Maybe Bool
attributeAsBool name opts = lookupAttribute name opts >>= extractBool
  where
    extractBool (CBool b) = Just b
    extractBool _         = Nothing

-- | Extract an 'Integer' value from an attribute, if present and int-typed.
attributeAsInt :: Text -> [OptionDef] -> Maybe Integer
attributeAsInt name opts = lookupAttribute name opts >>= extractInt
  where
    extractInt (CInt n) = Just n
    extractInt _        = Nothing

-- | Extract a 'Double' value from an attribute, if present and float-typed.
attributeAsFloat :: Text -> [OptionDef] -> Maybe Double
attributeAsFloat name opts = lookupAttribute name opts >>= extractFloat
  where
    extractFloat (CFloat n) = Just n
    extractFloat _          = Nothing

-- | Extract an aggregate (key-value) value from an attribute.
attributeAsAggregate :: Text -> [OptionDef] -> Maybe [(Text, Constant)]
attributeAsAggregate name opts = lookupAttribute name opts >>= extractAgg
  where
    extractAgg (CAggregate kvs) = Just kvs
    extractAgg _                = Nothing

-- ---------------------------------------------------------------------------
-- Extracting options from message elements
-- ---------------------------------------------------------------------------

-- | Extract all message-level options from a 'MessageDef'.
-- These are the @option@ statements inside the message body.
messageOptions :: MessageDef -> [OptionDef]
messageOptions msg = mapMaybe extractOpt (msgElements msg)
  where
    extractOpt (MEOption o) = Just o
    extractOpt _            = Nothing
