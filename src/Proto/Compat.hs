-- | Schema compatibility checking in the style of Confluent Schema Registry.
--
-- Confluent defines several compatibility levels for schema evolution:
--
-- * __BACKWARD__: new schema can read data written by the old schema.
--   Consumers can be upgraded before producers.
--
-- * __FORWARD__: old schema can read data written by the new schema.
--   Producers can be upgraded before consumers.
--
-- * __FULL__: both backward and forward compatible.
--   Producers and consumers can be upgraded in any order.
--
-- * __BACKWARD_TRANSITIVE__: backward compatible with all prior versions.
-- * __FORWARD_TRANSITIVE__: forward compatible with all prior versions.
-- * __FULL_TRANSITIVE__: full compatible with all prior versions.
-- * __NONE__: no compatibility checking.
--
-- For protobuf, this translates to checking specific rules about field
-- additions, removals, type changes, and number reuse.
--
-- Backward-compatible changes (new reads old):
-- * Add a new optional/repeated field
-- * Remove a field (must be reserved)
-- * Change a field from required to optional (proto2)
--
-- Forward-compatible changes (old reads new):
-- * Add a new optional/repeated field (old ignores it)
-- * Remove an optional/repeated field
--
-- Breaking changes:
-- * Change a field's type (different wire type)
-- * Change a field's number
-- * Reuse a previously deleted field number without reserving
-- * Remove a required field (proto2)
-- * Add a required field (proto2)
-- * Change between scalar types with different wire formats
-- * Rename an enum value that's used for JSON encoding
module Proto.Compat
  ( -- * Compatibility levels
    CompatLevel (..)

    -- * Compatibility checking
  , checkCompat
  , checkCompatAll

    -- * Compatibility results
  , CompatResult (..)
  , CompatError (..)
  , Severity (..)
  , isCompatible
  , compatErrors

    -- * Individual checks
  , checkBackward
  , checkForward
  , checkFull

    -- * Specific rules
  , checkMessageCompat
  , checkEnumCompat
  , checkFieldCompat

    -- * Direction (for specific rule checks)
  , Direction (..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Proto.AST
import Proto.Inspect

-- | Compatibility level, matching Confluent Schema Registry semantics.
data CompatLevel
  = None
  | Backward
  | BackwardTransitive
  | Forward
  | ForwardTransitive
  | Full
  | FullTransitive
  deriving stock (Show, Eq, Ord)

data Severity = Warning | Error
  deriving stock (Show, Eq, Ord)

data CompatError = CompatError
  { ceMessage  :: !Text
  , cePath     :: !Text     -- e.g. "Person.name" or "Status.ACTIVE"
  , ceSeverity :: !Severity
  , ceRule     :: !Text     -- short rule identifier
  } deriving stock (Show, Eq)

data CompatResult = CompatResult
  { crErrors :: ![CompatError]
  } deriving stock (Show, Eq)

instance Semigroup CompatResult where
  CompatResult a <> CompatResult b = CompatResult (a <> b)

instance Monoid CompatResult where
  mempty = CompatResult []

isCompatible :: CompatResult -> Bool
isCompatible (CompatResult errs) = not (any (\e -> ceSeverity e == Error) errs)

compatErrors :: CompatResult -> [CompatError]
compatErrors = crErrors

-- | Check compatibility between a new schema and the previous version.
checkCompat :: CompatLevel -> ProtoFile -> ProtoFile -> CompatResult
checkCompat level new old = case level of
  None                -> mempty
  Backward            -> checkBackward new old
  BackwardTransitive  -> checkBackward new old
  Forward             -> checkForward new old
  ForwardTransitive   -> checkForward new old
  Full                -> checkFull new old
  FullTransitive      -> checkFull new old

-- | Check against all previous versions (transitive).
checkCompatAll :: CompatLevel -> ProtoFile -> [ProtoFile] -> CompatResult
checkCompatAll level new olds = case level of
  None                 -> mempty
  Backward             -> maybe mempty (checkBackward new) (safeHead olds)
  BackwardTransitive   -> foldMap (checkBackward new) olds
  Forward              -> maybe mempty (checkForward new) (safeHead olds)
  ForwardTransitive    -> foldMap (checkForward new) olds
  Full                 -> maybe mempty (checkFull new) (safeHead olds)
  FullTransitive       -> foldMap (checkFull new) olds
  where
    safeHead [] = Nothing
    safeHead (x:_) = Just x

-- | BACKWARD: can the new schema read data written by the old schema?
checkBackward :: ProtoFile -> ProtoFile -> CompatResult
checkBackward new old = checkMessages BackwardDir new old <> checkEnums BackwardDir new old

-- | FORWARD: can the old schema read data written by the new schema?
checkForward :: ProtoFile -> ProtoFile -> CompatResult
checkForward new old = checkMessages ForwardDir new old <> checkEnums ForwardDir new old

-- | FULL: both backward and forward compatible.
checkFull :: ProtoFile -> ProtoFile -> CompatResult
checkFull new old = checkBackward new old <> checkForward new old

data Direction = BackwardDir | ForwardDir
  deriving stock (Show, Eq)

dirLabel :: Direction -> Text
dirLabel BackwardDir = "BACKWARD"
dirLabel ForwardDir  = "FORWARD"

-- Message-level checks

checkMessages :: Direction -> ProtoFile -> ProtoFile -> CompatResult
checkMessages dir new old =
  let newMsgs = Map.fromList (fmap (\m -> (msgName m, m)) (allMessages new))
      oldMsgs = Map.fromList (fmap (\m -> (msgName m, m)) (allMessages old))
  in
    -- Messages present in old but absent in new
    foldMap (\name -> case dir of
      BackwardDir -> mempty
      ForwardDir -> makeError (dirLabel dir <> ": message '" <> name <> "' was removed")
                      name Error "MESSAGE_REMOVED"
    ) (Map.keys (Map.difference oldMsgs newMsgs))
    <>
    -- Messages present in both: check field-level compatibility
    Map.foldlWithKey' (\acc name newMsg ->
      case Map.lookup name oldMsgs of
        Nothing -> acc
        Just oldMsg -> acc <> checkMessageCompat dir name newMsg oldMsg
    ) mempty newMsgs

checkMessageCompat :: Direction -> Text -> MessageDef -> MessageDef -> CompatResult
checkMessageCompat dir msgPath newMsg oldMsg =
  let newFields = fieldMap newMsg
      oldFields = fieldMap oldMsg
      newNums   = Map.keysSet newFields
      oldNums   = Map.keysSet oldFields
      reserved  = reservedNumbers newMsg
  in
    -- Fields removed from old
    foldMap (\num ->
      let oldFd = oldFields Map.! num
          path = msgPath <> "." <> fieldName oldFd
      in case dir of
        BackwardDir ->
          if num `Set.member` reserved
          then mempty
          else makeError (dirLabel dir <> ": field '" <> fieldName oldFd
                 <> "' (number " <> T.pack (show num) <> ") removed without reserving the number")
                 path Error "FIELD_REMOVED_NOT_RESERVED"
          <>
          case fieldLabel oldFd of
            Just Required -> makeError (dirLabel dir <> ": required field '" <> fieldName oldFd <> "' removed")
                               path Error "REQUIRED_FIELD_REMOVED"
            _ -> mempty
        ForwardDir -> mempty
    ) (Set.difference oldNums newNums)
    <>
    -- Fields added in new
    foldMap (\num ->
      let newFd = newFields Map.! num
          path = msgPath <> "." <> fieldName newFd
      in case dir of
        BackwardDir -> case fieldLabel newFd of
          Just Required -> makeError (dirLabel dir <> ": required field '" <> fieldName newFd <> "' added")
                             path Error "REQUIRED_FIELD_ADDED"
          _ -> mempty
        ForwardDir -> mempty
    ) (Set.difference newNums oldNums)
    <>
    -- Fields present in both: check type compatibility
    Map.foldlWithKey' (\acc num newFd ->
      case Map.lookup num oldFields of
        Nothing -> acc
        Just oldFd -> acc <> checkFieldCompat dir msgPath newFd oldFd
    ) mempty newFields
    <>
    -- Number reuse check: field numbers in new that were in old but deleted
    -- and then re-added with a different name
    foldMap (\num ->
      let newFd = newFields Map.! num
          oldFd = oldFields Map.! num
          path = msgPath <> "." <> fieldName newFd
      in if fieldName newFd /= fieldName oldFd && not (num `Set.member` reserved)
         then makeWarning (dirLabel dir <> ": field number " <> T.pack (show num)
                <> " changed name from '" <> fieldName oldFd <> "' to '" <> fieldName newFd <> "'")
                path "FIELD_NAME_CHANGED"
         else mempty
    ) (Set.intersection newNums oldNums)

-- | Check compatibility of a single field.
checkFieldCompat :: Direction -> Text -> FieldDef -> FieldDef -> CompatResult
checkFieldCompat dir msgPath newFd oldFd =
  let path = msgPath <> "." <> fieldName newFd
  in
    -- Type change
    (if fieldType newFd /= fieldType oldFd
     then
       if wireCompatible (fieldType newFd) (fieldType oldFd)
       then makeWarning (dirLabel dir <> ": field '" <> fieldName newFd
              <> "' type changed from " <> showFieldType (fieldType oldFd)
              <> " to " <> showFieldType (fieldType newFd)
              <> " (wire-compatible)") path "FIELD_TYPE_CHANGED_COMPATIBLE"
       else makeError (dirLabel dir <> ": field '" <> fieldName newFd
              <> "' type changed from " <> showFieldType (fieldType oldFd)
              <> " to " <> showFieldType (fieldType newFd)
              <> " (wire-INCOMPATIBLE)") path Error "FIELD_TYPE_CHANGED_INCOMPATIBLE"
     else mempty)
    <>
    -- Label change (required -> optional is OK for backward, not for forward)
    (case (fieldLabel oldFd, fieldLabel newFd) of
      (Just Required, Just Optional) -> case dir of
        BackwardDir -> mempty
        ForwardDir -> makeError (dirLabel dir <> ": field '" <> fieldName newFd
                        <> "' changed from required to optional") path Error "REQUIRED_TO_OPTIONAL"
      (Just Optional, Just Required) ->
        makeError (dirLabel dir <> ": field '" <> fieldName newFd
          <> "' changed from optional to required") path Error "OPTIONAL_TO_REQUIRED"
      (Nothing, Just Required) ->
        makeError (dirLabel dir <> ": field '" <> fieldName newFd
          <> "' changed to required") path Error "BECAME_REQUIRED"
      _ -> mempty)

-- Enum-level checks

checkEnums :: Direction -> ProtoFile -> ProtoFile -> CompatResult
checkEnums dir new old =
  let newEnums = Map.fromList (fmap (\e -> (enumName e, e)) (allEnums new))
      oldEnums = Map.fromList (fmap (\e -> (enumName e, e)) (allEnums old))
  in
    Map.foldlWithKey' (\acc name newEnum ->
      case Map.lookup name oldEnums of
        Nothing -> acc
        Just oldEnum -> acc <> checkEnumCompat dir name newEnum oldEnum
    ) mempty newEnums

-- | Check compatibility of an enum definition.
checkEnumCompat :: Direction -> Text -> EnumDef -> EnumDef -> CompatResult
checkEnumCompat dir enumPath newEnum oldEnum =
  let newVals = Map.fromList (fmap (\v -> (evNumber v, evName v)) (enumValues newEnum))
      oldVals = Map.fromList (fmap (\v -> (evNumber v, evName v)) (enumValues oldEnum))
      newNums = Map.keysSet newVals
      oldNums = Map.keysSet oldVals
  in
    -- Values removed
    foldMap (\num ->
      let name = oldVals Map.! num
          path = enumPath <> "." <> name
      in case dir of
        BackwardDir -> makeError (dirLabel dir <> ": enum value '"
                         <> name <> "' (number " <> T.pack (show num) <> ") removed")
                         path Error "ENUM_VALUE_REMOVED"
        ForwardDir -> mempty
    ) (Set.difference oldNums newNums)
    <>
    -- Values added
    foldMap (\num ->
      let name = newVals Map.! num
          path = enumPath <> "." <> name
      in case dir of
        ForwardDir -> makeWarning (dirLabel dir <> ": enum value '"
                        <> name <> "' (number " <> T.pack (show num) <> ") added; "
                        <> "old readers will see the numeric value") path "ENUM_VALUE_ADDED"
        BackwardDir -> mempty
    ) (Set.difference newNums oldNums)
    <>
    -- Values renamed (same number, different name) — problematic for JSON encoding
    foldMap (\num ->
      let newName = newVals Map.! num
          oldName = oldVals Map.! num
          path = enumPath <> "." <> newName
      in if newName /= oldName
         then makeWarning (dirLabel dir <> ": enum value at number " <> T.pack (show num)
                <> " renamed from '" <> oldName <> "' to '" <> newName
                <> "' (breaks JSON compatibility)") path "ENUM_VALUE_RENAMED"
         else mempty
    ) (Set.intersection newNums oldNums)

-- Wire compatibility: two types are wire-compatible if they use the same
-- wire format on the wire. This means data written with one type can be
-- read (if perhaps misinterpreted) with the other.

wireCompatible :: FieldType -> FieldType -> Bool
wireCompatible a b = wireType a == wireType b

data WireGroup = WGVarint | WG64Bit | WG32Bit | WGLenDelim
  deriving stock (Eq)

wireType :: FieldType -> WireGroup
wireType = \case
  FTScalar SDouble   -> WG64Bit
  FTScalar SFloat    -> WG32Bit
  FTScalar SInt32    -> WGVarint
  FTScalar SInt64    -> WGVarint
  FTScalar SUInt32   -> WGVarint
  FTScalar SUInt64   -> WGVarint
  FTScalar SSInt32   -> WGVarint
  FTScalar SSInt64   -> WGVarint
  FTScalar SFixed32  -> WG32Bit
  FTScalar SFixed64  -> WG64Bit
  FTScalar SSFixed32 -> WG32Bit
  FTScalar SSFixed64 -> WG64Bit
  FTScalar SBool     -> WGVarint
  FTScalar SString   -> WGLenDelim
  FTScalar SBytes    -> WGLenDelim
  FTNamed _          -> WGLenDelim

-- Helpers

fieldMap :: MessageDef -> Map Int FieldDef
fieldMap msg = Map.fromList
  (fmap (\fd -> (unFieldNumber (fieldNumber fd), fd)) (messageFields msg))

reservedNumbers :: MessageDef -> Set Int
reservedNumbers msg = Set.fromList $ concatMap go (msgElements msg)
  where
    go (MEReserved (ReservedNumbers ranges)) = concatMap expandRange ranges
    go _ = []
    expandRange (ReservedSingle n) = [n]
    expandRange (ReservedRange lo hi) = [lo..hi]

showFieldType :: FieldType -> Text
showFieldType = \case
  FTScalar s -> showScalar s
  FTNamed n  -> n

showScalar :: ScalarType -> Text
showScalar = \case
  SDouble   -> "double"
  SFloat    -> "float"
  SInt32    -> "int32"
  SInt64    -> "int64"
  SUInt32   -> "uint32"
  SUInt64   -> "uint64"
  SSInt32   -> "sint32"
  SSInt64   -> "sint64"
  SFixed32  -> "fixed32"
  SFixed64  -> "fixed64"
  SSFixed32 -> "sfixed32"
  SSFixed64 -> "sfixed64"
  SBool     -> "bool"
  SString   -> "string"
  SBytes    -> "bytes"

makeError :: Text -> Text -> Severity -> Text -> CompatResult
makeError msg path sev rule = CompatResult [CompatError msg path sev rule]

makeWarning :: Text -> Text -> Text -> CompatResult
makeWarning msg path rule = CompatResult [CompatError msg path Warning rule]
