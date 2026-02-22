-- | Attribute-driven codegen hook system.
--
-- Protobuf custom options (attributes) can drive additional Haskell code
-- generation. This module provides hook types for both the text-based
-- code generator ('CodeGenHooks') and the Template Haskell splice path
-- ('THHooks'), sharing the same context types.
--
-- = Three integration points
--
-- There are three ways proto files get turned into Haskell code in hs-proto,
-- and hooks plug into all of them:
--
-- 1. __Text-based codegen__ (@hs-proto-gen@ CLI, 'Proto.Setup', 'Proto.CodeGen.generateModuleText'):
--    set the 'genHooks' field of 'Proto.CodeGen.GenerateOpts'.
-- 2. __Template Haskell splices__ ('Proto.TH.loadProtoWith'):
--    set the 'loTHHooks' field of 'Proto.TH.LoadOpts'.
-- 3. __Cabal Setup.hs__ ('Proto.Setup.protoGenPreBuildHook'):
--    set 'Proto.Setup.pgcHooks' in 'Proto.Setup.ProtoGenConfig'.
--
-- = Text-based codegen hooks
--
-- Use 'CodeGenHooks' when generating Haskell source as 'Text'. Each hook
-- receives a typed context and returns @['Text']@ lines to append after
-- the element's standard generated code.
--
-- @
-- import Proto.CodeGen.Hooks
-- import Proto.CodeGen ('Proto.CodeGen.GenerateOpts'(..), 'Proto.CodeGen.defaultGenerateOpts')
--
-- -- Add a comment after every message that has the (audited) annotation
-- auditHook :: 'CodeGenHooks'
-- auditHook = 'onMessageAttribute' "audited" $ \\val ctx ->
--   case val of
--     'CBool' True ->
--       [ "-- | WARNING: " \<> 'mhcHsTypeName' ctx
--         \<> " is an audited message (proto: " \<> 'mhcFqProtoName' ctx \<> ")"
--       ]
--     _ -> []
--
-- opts :: 'Proto.CodeGen.GenerateOpts'
-- opts = 'Proto.CodeGen.defaultGenerateOpts' { 'Proto.CodeGen.genHooks' = auditHook }
-- @
--
-- Given this proto:
--
-- @
-- message Transfer {
--   option (audited) = true;
--   string from = 1;
--   string to   = 2;
--   int64 amount = 3;
-- }
-- @
--
-- The generated module will contain, after @Transfer@'s instances:
--
-- @
-- -- | WARNING: Transfer is an audited message (proto: myapp.Transfer)
-- @
--
-- = Template Haskell hooks
--
-- Use 'THHooks' when loading proto files via Template Haskell. Each hook
-- receives the same context types but returns @'Language.Haskell.TH.Q' ['Language.Haskell.TH.Dec']@
-- — real TH declarations spliced into the calling module.
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import Proto.TH
-- import Proto.CodeGen.Hooks
--
-- showHook :: 'THHooks'
-- showHook = mempty
--   { 'thOnMessage' = \\ctx -> do
--       -- For every message, generate a \"describe\" function
--       let tyStr = 'Data.Text.unpack' ('mhcHsTypeName' ctx)
--           fnName = 'Language.Haskell.TH.mkName' ("describe" \<> tyStr)
--       sig  \<- 'Language.Haskell.TH.sigD' fnName [t| String |]
--       body \<- 'Language.Haskell.TH.valD' ('Language.Haskell.TH.varP' fnName)
--                ('Language.Haskell.TH.normalB' ('Language.Haskell.TH.litE' ('Language.Haskell.TH.stringL' ("Proto message: " \<> tyStr)))) []
--       pure [sig, body]
--   }
--
-- \$(loadProtoWith defaultLoadOpts { loTHHooks = showHook } "person.proto")
--
-- -- Now @describePerson :: String@ is available.
-- @
--
-- = Composing hooks
--
-- Both 'CodeGenHooks' and 'THHooks' are 'Semigroup' and 'Monoid', so
-- independent hooks compose with @(\<>)@:
--
-- @
-- allHooks :: 'CodeGenHooks'
-- allHooks = auditHook \<> loggingHook \<> metricsHook
-- @
--
-- = Attribute-driven constructors
--
-- 'onMessageAttribute', 'onEnumAttribute', etc. create hooks that only
-- fire when a specific extension option is present:
--
-- @
-- -- Only fires on messages with option (generate_lens) = true;
-- lensHook :: 'CodeGenHooks'
-- lensHook = 'onMessageAttribute' "generate_lens" $ \\val ctx ->
--   case val of
--     'CBool' True -> ["makeLenses ''" \<> 'mhcHsTypeName' ctx]
--     _           -> []
-- @
--
-- The equivalent for TH:
--
-- @
-- thLensHook :: 'THHooks'
-- thLensHook = 'thOnMessageAttribute' "generate_lens" $ \\val ctx ->
--   case val of
--     'CBool' True -> do
--       -- ... generate TH declarations ...
--       pure []
--     _ -> pure []
-- @
module Proto.CodeGen.Hooks
  ( -- * Text-based codegen hooks
    CodeGenHooks (..)
  , defaultCodeGenHooks

    -- * Template Haskell codegen hooks
  , THHooks (..)
  , defaultTHHooks

    -- * Hook contexts (shared by both hook types)
  , FileHookCtx (..)
  , MessageHookCtx (..)
  , EnumHookCtx (..)
  , ServiceHookCtx (..)

    -- * Attribute-driven constructors (text codegen)
  , onMessageAttribute
  , onEnumAttribute
  , onServiceAttribute
  , onFileAttribute

    -- * Attribute-driven constructors (Template Haskell)
  , thOnMessageAttribute
  , thOnEnumAttribute
  , thOnFileAttribute

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
import Language.Haskell.TH (Q, Dec)

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
-- Template Haskell hooks
-- ---------------------------------------------------------------------------

-- | Hooks for the Template Haskell code generation path.
--
-- Each field receives the same context types as 'CodeGenHooks' but returns
-- @'Q' ['Dec']@ — actual TH declarations that are spliced into the module
-- alongside the standard generated types and instances.
data THHooks = THHooks
  { thOnFile    :: !(FileHookCtx -> Q [Dec])
  , thOnMessage :: !(MessageHookCtx -> Q [Dec])
  , thOnEnum    :: !(EnumHookCtx -> Q [Dec])
  }

-- | No-op TH hooks (produce no extra declarations).
defaultTHHooks :: THHooks
defaultTHHooks = THHooks
  { thOnFile    = const (pure [])
  , thOnMessage = const (pure [])
  , thOnEnum    = const (pure [])
  }

instance Semigroup THHooks where
  a <> b = THHooks
    { thOnFile    = \ctx -> (<>) <$> thOnFile a ctx    <*> thOnFile b ctx
    , thOnMessage = \ctx -> (<>) <$> thOnMessage a ctx <*> thOnMessage b ctx
    , thOnEnum    = \ctx -> (<>) <$> thOnEnum a ctx    <*> thOnEnum b ctx
    }

instance Monoid THHooks where
  mempty = defaultTHHooks

-- ---------------------------------------------------------------------------
-- Attribute-driven hook constructors (text codegen)
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
-- Attribute-driven hook constructors (Template Haskell)
-- ---------------------------------------------------------------------------

-- | Create a TH hook that fires on messages bearing a specific extension option.
thOnMessageAttribute :: Text -> (Constant -> MessageHookCtx -> Q [Dec]) -> THHooks
thOnMessageAttribute attrName f = mempty
  { thOnMessage = \ctx ->
      case lookupAttribute attrName (mhcOptions ctx) of
        Just val -> f val ctx
        Nothing  -> pure []
  }

-- | Create a TH hook that fires on enums bearing a specific extension option.
thOnEnumAttribute :: Text -> (Constant -> EnumHookCtx -> Q [Dec]) -> THHooks
thOnEnumAttribute attrName f = mempty
  { thOnEnum = \ctx ->
      case lookupAttribute attrName (ehcOptions ctx) of
        Just val -> f val ctx
        Nothing  -> pure []
  }

-- | Create a TH hook that fires when a file-level extension option is present.
thOnFileAttribute :: Text -> (Constant -> FileHookCtx -> Q [Dec]) -> THHooks
thOnFileAttribute attrName f = mempty
  { thOnFile = \ctx ->
      case lookupAttribute attrName (fhcFileOptions ctx) of
        Just val -> f val ctx
        Nothing  -> pure []
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
