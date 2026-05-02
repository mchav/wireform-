{-# LANGUAGE TemplateHaskell #-}

-- | Backend-aware folding of @[Modifier]@ annotations into a typed
-- 'ModifierInfo' record.
--
-- A 'ModifierInfo' is the input each per-format deriver consumes. It
-- has been resolved against a single 'Backend' so the deriver does
-- not need to think about per-backend overrides itself.
--
-- == Resolution strategy
--
-- Modifiers are folded in two passes:
--
-- 1. /Global pass/ — every modifier that is not 'modifierIsBackendScoped'
--    is folded in. Conflicts within the global pass raise a
--    'ModifierError' (e.g. two distinct global renames).
-- 2. /Backend pass/ — modifiers wrapped in 'ModForBackends' /
--    'ModBackendOnly' / 'ModBackendDisable' that target the active
--    'Backend' are then folded in /on top of/ the global result. This
--    pass shadows global directives without raising a conflict, which
--    is the whole point of per-backend overrides.
module Wireform.Derive.ModifierInfo
  ( -- * Folded representation
    ModifierInfo (..)
  , RenameSpec (..)
  , emptyModifierInfo

    -- * Resolution
  , foldModifiers
  , reifyModifierInfoFor
  , reifyModifierInfo

    -- * Errors
  , ModifierError (..)

    -- * Wire-key rendering
  , renderRenameKey
  , renderWireKey

    -- * Helpers
  , defaultRenameForBackend
  ) where

import Control.Exception (Exception)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH (Exp, Name, Q, varE)
import Language.Haskell.TH.Syntax (Lift (lift), Quasi, qReifyAnnotations)
import qualified Language.Haskell.TH.Syntax as TH

import Wireform.Derive.Backend
import Wireform.Derive.Modifier
import Wireform.Derive.NameStyle

-- ---------------------------------------------------------------------------
-- Folded ModifierInfo
-- ---------------------------------------------------------------------------

-- | A field's resolved rename spec.
--
-- * 'RenameSpecLiteral' — a fully-baked 'Text' (from 'RenameTo'). Used
--   verbatim as the wire key.
-- * 'RenameSpecStyle' — a 'NameStyle' to apply to the field's
--   selector base name at render time (so 'Idiomatic' can be resolved
--   against the deriver's active backend).
-- * 'RenameSpecApply' — a @Text -> Text@ function to call at /runtime/
--   on the selector base name.
data RenameSpec
  = RenameSpecLiteral !Text
  | RenameSpecStyle   !NameStyle
  | RenameSpecApply   !Name
  deriving (Eq, Show)

-- | A fully resolved view of the modifiers attached to a single
-- 'Name', filtered for one active 'Backend'.
data ModifierInfo = ModifierInfo
  { miBackend       :: !Backend
    -- ^ The backend this 'ModifierInfo' was resolved for.
  , miRename        :: !(Maybe RenameSpec)
    -- ^ The wire key, if explicitly customised. When 'Nothing' the
    -- deriver should fall back to its default policy (typically
    -- 'defaultRenameForBackend').
  , miCoerce        :: !(Maybe Name)
  , miFlatten       :: !Bool
  , miSkip          :: !Bool
  , miDefaults      :: !(Maybe Name)
  , miTag           :: !(Maybe Int)
  , miRequired      :: !(Maybe Bool)
    -- ^ 'Just True' = required; 'Just False' = optional; 'Nothing' =
    -- format default.
  , miWireOverride  :: !(Maybe WireOverride)
  , miMapKey        :: !(Maybe MapKeyScalar)
    -- ^ Proto map key scalar (proto deriver only).
  , miOneof         :: !(Maybe Text)
    -- ^ Name of the proto @oneof@ this field belongs to (proto
    -- deriver only).
  , miCustom        :: !(Map Text [Modifier])
    -- ^ All 'ModCustom' payloads grouped by tag. Backends can scan
    -- their own tag and ignore the rest.
  } deriving (Eq, Show)

-- | The empty 'ModifierInfo' for a given backend.
emptyModifierInfo :: Backend -> ModifierInfo
emptyModifierInfo b = ModifierInfo
  { miBackend       = b
  , miRename        = Nothing
  , miCoerce        = Nothing
  , miFlatten       = False
  , miSkip          = False
  , miDefaults      = Nothing
  , miTag           = Nothing
  , miRequired      = Nothing
  , miWireOverride  = Nothing
  , miMapKey        = Nothing
  , miOneof         = Nothing
  , miCustom        = Map.empty
  }

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Conflicts surfaced during 'foldModifiers'.
data ModifierError
  = ConflictRename       !RenameSpec !RenameSpec
  | ConflictCoerce       !Name !Name
  | ConflictDefaults     !Name !Name
  | ConflictTag          !Int !Int
  | ConflictRequired     !Bool !Bool
  | ConflictWireOverride !WireOverride !WireOverride
  | ConflictMapKey       !MapKeyScalar !MapKeyScalar
  | ConflictOneof        !Text !Text
  | ConflictFlattenSkip
    -- ^ Both 'flatten' and 'skip' set on the same name.
  | InvalidRenameFnArity !Name !Int
  deriving (Eq, Show)

instance Exception ModifierError

-- ---------------------------------------------------------------------------
-- foldModifiers
-- ---------------------------------------------------------------------------

-- | Resolve a list of modifiers against a backend. Conflicts within
-- the global pass yield 'Left'; per-backend overrides cleanly shadow
-- global directives.
foldModifiers :: Backend -> [Modifier] -> Either ModifierError ModifierInfo
foldModifiers b ms = do
  let (globals, scoped) = partitionScope ms
  globalInfo <- foldList b (emptyModifierInfo b) globals
  -- Per-backend pass: do not raise on conflict; just shadow.
  let active = expandScoped b scoped
  pure (foldShadow active globalInfo)

-- ---------------------------------------------------------------------------
-- reifyModifierInfo / reifyModifierInfoFor
-- ---------------------------------------------------------------------------

-- | Reify all 'Modifier' annotations attached to a 'Name', resolved
-- for the given 'Backend'. Failure short-circuits the splice with
-- 'fail'.
reifyModifierInfoFor :: Quasi m => Backend -> Name -> m ModifierInfo
reifyModifierInfoFor b n = do
  (ms :: [Modifier]) <- qReifyAnnotations (TH.AnnLookupName n)
  case foldModifiers b ms of
    Left err ->
      let msg = "Wireform.Derive.ModifierInfo: invalid modifier "
             ++ "annotations on " ++ show n ++ ": " ++ show err
      in TH.qReport True msg >> fail msg
    Right mi -> pure mi

-- | Convenience: reify modifiers for every 'Backend' the deriver may
-- consult, returning a 'Map'. Useful when a single deriver wants to
-- emit instances for several formats from one annotation pass.
reifyModifierInfo
  :: Quasi m
  => [Backend]
  -> Name
  -> m (Map Backend ModifierInfo)
reifyModifierInfo backends n = do
  pairs <- mapM (\b -> (b,) <$> reifyModifierInfoFor b n) backends
  pure (Map.fromList pairs)

-- ---------------------------------------------------------------------------
-- Wire-key rendering
-- ---------------------------------------------------------------------------

-- | Splice a 'RenameSpec' into a Haskell expression of type 'Text'.
--
-- 'RenameSpecLiteral' bakes a string literal. 'RenameSpecStyle'
-- resolves any 'Idiomatic' inside the style against the supplied
-- 'Backend' and bakes the post-style 'Text' as a literal — zero
-- runtime cost. 'RenameSpecApply' splices a runtime function call
-- against the selector base name.
renderRenameKey :: Backend -> RenameSpec -> Text -> Q Exp
renderRenameKey b spec selBase = case spec of
  RenameSpecLiteral t  -> lift t
  RenameSpecStyle s    -> lift (applyStyle (resolveIdiomatic b s) selBase)
  RenameSpecApply  fn  -> [| $(varE fn) (T.pack $(lift (T.unpack selBase))) |]

-- | High-level wire-key renderer. If 'miRename' is set, defers to
-- 'renderRenameKey'; otherwise applies the backend's idiomatic style
-- (see 'idiomaticFor' / 'defaultRenameForBackend') to the selector
-- base name and bakes the result in as a literal.
renderWireKey :: ModifierInfo -> Text -> Q Exp
renderWireKey mi selBase = case miRename mi of
  Just spec -> renderRenameKey (miBackend mi) spec selBase
  Nothing   -> lift (defaultRenameForBackend (miBackend mi) selBase)

-- | The default wire key for a backend when no 'rename' modifier was
-- supplied: applies the backend's idiomatic style to the selector
-- base name.
defaultRenameForBackend :: Backend -> Text -> Text
defaultRenameForBackend b = applyStyle (resolveIdiomatic b (idiomaticFor b))

-- ---------------------------------------------------------------------------
-- Internals: scope partitioning
-- ---------------------------------------------------------------------------

-- | Split modifiers into global (apply to every backend) vs.
-- backend-scoped.
partitionScope :: [Modifier] -> ([Modifier], [Modifier])
partitionScope = foldr step ([], [])
  where
    step m (g, s)
      | modifierIsBackendScoped m = (g, m : s)
      | otherwise                 = (m : g, s)

-- | Expand backend-scoped wrappers into a flat list of modifiers
-- that apply to the active backend.
expandScoped :: Backend -> [Modifier] -> [Modifier]
expandScoped b = concatMap unwrap
  where
    unwrap (ModForBackends bs ms)
      | b `elem` bs = concatMap unwrap ms
      | otherwise   = []
    unwrap (ModBackendOnly bs m)
      | b `elem` bs = unwrap m
      | otherwise   = []
    unwrap (ModBackendDisable bs)
      | b `elem` bs = [ModSkip]
      | otherwise   = []
    -- Nested non-scoped modifier — should not occur post-partition,
    -- but if it does, treat it as global for this backend.
    unwrap m = [m]

-- ---------------------------------------------------------------------------
-- Internals: folding
-- ---------------------------------------------------------------------------

-- | Strict left fold raising on conflict.
foldList
  :: Backend
  -> ModifierInfo
  -> [Modifier]
  -> Either ModifierError ModifierInfo
foldList _ acc []     = pure acc
foldList b acc (m:ms) = do
  acc' <- mergeOne acc m
  foldList b acc' ms

-- | Strict left fold that silently shadows existing fields. Used for
-- per-backend overrides.
foldShadow :: [Modifier] -> ModifierInfo -> ModifierInfo
foldShadow ms acc0 = foldr (flip shadowOne) acc0 (reverse ms)

-- | Merge a single modifier into a 'ModifierInfo', raising on
-- conflict.
mergeOne :: ModifierInfo -> Modifier -> Either ModifierError ModifierInfo
mergeOne mi = \case
  ModRename r ->
    let new = renameSpec r
    in case miRename mi of
         Just old | old /= new -> Left (ConflictRename old new)
         _                     -> pure mi { miRename = Just new }

  ModCoerce n ->
    case miCoerce mi of
      Just old | old /= n -> Left (ConflictCoerce old n)
      _                   -> pure mi { miCoerce = Just n }

  ModFlatten
    | miSkip mi -> Left ConflictFlattenSkip
    | otherwise -> pure mi { miFlatten = True }

  ModSkip
    | miFlatten mi -> Left ConflictFlattenSkip
    | otherwise    -> pure mi { miSkip = True }

  ModDefaults n ->
    case miDefaults mi of
      Just old | old /= n -> Left (ConflictDefaults old n)
      _                   -> pure mi { miDefaults = Just n }

  ModTag t ->
    case miTag mi of
      Just old | old /= t -> Left (ConflictTag old t)
      _                   -> pure mi { miTag = Just t }

  ModRequired ->
    case miRequired mi of
      Just False -> Left (ConflictRequired False True)
      _          -> pure mi { miRequired = Just True }

  ModOptional ->
    case miRequired mi of
      Just True -> Left (ConflictRequired True False)
      _         -> pure mi { miRequired = Just False }

  ModWireOverride wo ->
    case miWireOverride mi of
      Just old | old /= wo -> Left (ConflictWireOverride old wo)
      _                    -> pure mi { miWireOverride = Just wo }

  ModMapKey k ->
    case miMapKey mi of
      Just old | old /= k -> Left (ConflictMapKey old k)
      _                   -> pure mi { miMapKey = Just k }

  ModOneof o ->
    case miOneof mi of
      Just old | old /= o -> Left (ConflictOneof old o)
      _                   -> pure mi { miOneof = Just o }

  m@(ModCustom tagName _) ->
    pure mi { miCustom = Map.insertWith (++) tagName [m] (miCustom mi) }

  -- Backend-scoped wrappers should not appear in the global pass; if
  -- they do (e.g. through nesting), shadow rather than raise.
  ModForBackends   _ _ -> pure mi
  ModBackendOnly   _ _ -> pure mi
  ModBackendDisable _  -> pure mi

-- | Like 'mergeOne' but unconditionally overwrites; used for
-- per-backend overrides.
shadowOne :: ModifierInfo -> Modifier -> ModifierInfo
shadowOne mi = \case
  ModRename r       -> mi { miRename       = Just (renameSpec r) }
  ModCoerce n       -> mi { miCoerce       = Just n }
  ModFlatten        -> mi { miFlatten      = True, miSkip = False }
  ModSkip           -> mi { miSkip         = True, miFlatten = False }
  ModDefaults n     -> mi { miDefaults     = Just n }
  ModTag t          -> mi { miTag          = Just t }
  ModRequired       -> mi { miRequired     = Just True }
  ModOptional       -> mi { miRequired     = Just False }
  ModWireOverride w -> mi { miWireOverride = Just w }
  ModMapKey k       -> mi { miMapKey       = Just k }
  ModOneof o        -> mi { miOneof        = Just o }
  m@(ModCustom tagName _) ->
    mi { miCustom = Map.insertWith (++) tagName [m] (miCustom mi) }
  ModForBackends   _ _ -> mi
  ModBackendOnly   _ _ -> mi
  ModBackendDisable _  -> mi

-- | Convert a 'Rename' modifier into a 'RenameSpec'. 'RenameStyle'
-- entries keep their 'NameStyle' verbatim so per-backend resolution
-- of 'Idiomatic' (and application against the selector base name)
-- can happen at render time in 'renderRenameKey'.
renameSpec :: Rename -> RenameSpec
renameSpec = \case
  RenameTo t    -> RenameSpecLiteral t
  RenameStyle s -> RenameSpecStyle s
  RenameFn n    -> RenameSpecApply n
