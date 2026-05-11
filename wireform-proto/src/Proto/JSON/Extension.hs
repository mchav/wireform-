{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Runtime registry for proto2 extension JSON helpers.
--
-- Proto2 lets messages declare extension ranges:
--
-- @
-- message TestAllTypesProto2 \{
--   extensions 120 to 200;
-- }
-- extend TestAllTypesProto2 \{
--   optional int32 extension_int32 = 120;
-- }
-- @
--
-- The proto3 canonical JSON for an extension targets it via
-- a bracket-quoted fully-qualified name:
--
-- @
-- {\"[protobuf_test_messages.proto2.extension_int32]\": 1}
-- @
--
-- The library doesn't (yet) thread extension descriptors
-- statically through every generated message, so we maintain a
-- runtime registry: 'loadProto' emits a registration call per
-- extension declaration and the user-facing JSON
-- encoder\/parser consults the registry at run time.
module Proto.JSON.Extension
  ( ExtJsonCodec (..)
  , registerExtensionJson
  , lookupExtensionByFqn
  , lookupExtensionByNumber
  , parentHasExtensions
  , extensionEntriesForJson
  , parseExtensionEntry

    -- * Per-extension codec primitives (used by the
    -- 'loadProto'-generated registration code)
  , parseExtValueViaConstructor
  , encodeExtValueViaConstructor
  ) where

import Control.Monad (foldM)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import qualified Data.Aeson.Types as AesonT
import qualified Data.ByteString as BS

import Data.Int (Int32, Int64)
import qualified Data.Scientific as Sci
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)

import Proto.Decode (UnknownField (..))
import Proto.Extension
  ( ExtensionType (..)
  , encodeExtensionValue
  , decodeExtensionValue
  , unknownFieldNumber
  )
import qualified Proto.JSON as PJ

-- | Describes how to bridge a single proto2 extension between
-- its JSON value form and the wire 'UnknownField' representation
-- the message stores under its unknown-fields slot.
data ExtJsonCodec = ExtJsonCodec
  { ejcExtensionFqn :: !Text
    -- ^ Fully-qualified proto name of the extension (the
    --   bracket-quoted JSON key without the brackets).
  , ejcFieldNumber  :: !Int
  , ejcParseValue   :: Aeson.Value -> Either String UnknownField
  , ejcEncodeValue  :: UnknownField -> Either String Aeson.Value
  }

-- | One per parent message: maps both extension FQN (for parse)
-- and field number (for output) to the codec.
data ExtRegistryEntry = ExtRegistryEntry
  { byFqn :: !(Map Text ExtJsonCodec)
  , byNum :: !(Map Int  ExtJsonCodec)
  }

emptyEntry :: ExtRegistryEntry
emptyEntry = ExtRegistryEntry Map.empty Map.empty

{-# NOINLINE registryRef #-}
registryRef :: IORef (Map Text ExtRegistryEntry)
registryRef = unsafePerformIO (newIORef Map.empty)

-- | Tri-state cached "is the registry empty?" flag. The hot
-- path ('parentHasExtensions') consults this before the IORef
-- read so the common case (proto3 codebases with zero
-- 'extend' blocks across the entire process) gets a single
-- pointer-comparison check instead of an IORef + 'Map.lookup'
-- on every JSON encode \/ decode call.
{-# NOINLINE registryAnyRegisteredRef #-}
registryAnyRegisteredRef :: IORef Bool
registryAnyRegisteredRef = unsafePerformIO (newIORef False)

-- | Register a JSON codec for an extension targeting the named
-- parent message. Idempotent: re-registering the same FQN
-- overrides the prior entry. Called by 'loadProto'-generated
-- code at module load time.
registerExtensionJson
  :: Text          -- ^ Parent message FQN.
  -> ExtJsonCodec
  -> IO ()
registerExtensionJson parentFqn codec = do
  atomicModifyIORef' registryRef (\m ->
    let !entry  = Map.findWithDefault emptyEntry parentFqn m
        !entry' = entry
          { byFqn = Map.insert (ejcExtensionFqn codec) codec (byFqn entry)
          , byNum = Map.insert (ejcFieldNumber  codec) codec (byNum entry)
          }
    in (Map.insert parentFqn entry' m, ()))
  atomicModifyIORef' registryAnyRegisteredRef (const (True, ()))

-- | Fast registry lookup: returns 'Nothing' when the parent
-- has no registered extensions (the common case for proto3
-- messages and any proto2 message without an @extend@ block).
-- Callers fall back to the empty-list short-circuit then.
--
-- Uses 'unsafeDupablePerformIO' (instead of 'unsafePerformIO')
-- because the result is purely a read-only pointer comparison —
-- duplicating the read across threads is harmless.
lookupEntryFast :: Text -> Maybe ExtRegistryEntry
lookupEntryFast parentFqn =
  unsafeDupablePerformIO (Map.lookup parentFqn <$> readIORef registryRef)
{-# NOINLINE lookupEntryFast #-}

-- | Same as 'lookupEntryFast' but returns the 'emptyEntry'
-- when nothing's registered, for callers that find @Maybe@
-- handling tedious.
lookupEntry :: Text -> ExtRegistryEntry
lookupEntry parentFqn =
  case lookupEntryFast parentFqn of
    Just e  -> e
    Nothing -> emptyEntry

lookupExtensionByFqn :: Text -> Text -> Maybe ExtJsonCodec
lookupExtensionByFqn parentFqn fqn =
  case lookupEntryFast parentFqn of
    Nothing -> Nothing
    Just e  -> Map.lookup fqn (byFqn e)

lookupExtensionByNumber :: Text -> Int -> Maybe ExtJsonCodec
lookupExtensionByNumber parentFqn n =
  case lookupEntryFast parentFqn of
    Nothing -> Nothing
    Just e  -> Map.lookup n (byNum e)

-- | Cheap registry membership check the splice can use to
-- short-circuit JSON extension drain on every parsed message
-- whose parent has zero registered extensions.
--
-- Two-level fast path:
--
--  * Process-wide 'registryAnyRegisteredRef' Bool — when the
--    entire registry is empty (the common case for proto3
--    codebases), this is the only IORef touched.
--  * Per-parent Map lookup — only consulted when the global
--    flag is @True@.
parentHasExtensions :: Text -> Bool
parentHasExtensions parentFqn
  | not registryHasAny = False
  | otherwise = case lookupEntryFast parentFqn of
      Nothing -> False
      Just _  -> True
{-# INLINE parentHasExtensions #-}

-- | Read the global "any extension ever registered?" flag.
-- Bypasses the per-parent Map lookup the common case never
-- needs.
registryHasAny :: Bool
registryHasAny =
  unsafeDupablePerformIO (readIORef registryAnyRegisteredRef)
{-# NOINLINE registryHasAny #-}

-- | Translate every registered extension that's present in the
-- supplied unknown-fields slot into its bracket-quoted JSON
-- @(key, value)@ pair. Unknown fields not matching a registered
-- extension stay invisible to JSON output (no schema to bind
-- them to).
--
-- Fast-paths: empty unknown-fields list AND missing-from-
-- registry both bypass the per-uf walk and allocate nothing.
extensionEntriesForJson
  :: Text             -- ^ Parent message FQN.
  -> [UnknownField]
  -> [(Text, Aeson.Value)]
extensionEntriesForJson _ [] = []
extensionEntriesForJson _ _ | not registryHasAny = []
extensionEntriesForJson parentFqn ufs =
  case lookupEntryFast parentFqn of
    Nothing    -> []
    Just entry ->
      let go uf = case Map.lookup (unknownFieldNumber uf) (byNum entry) of
            Nothing    -> Nothing
            Just codec -> case ejcEncodeValue codec uf of
              Left _  -> Nothing
              Right v ->
                Just ( T.cons '[' (ejcExtensionFqn codec <> T.singleton ']')
                     , v
                     )
      in mapMaybe go ufs
{-# INLINE extensionEntriesForJson #-}

-- | If @key@ has the bracket-quoted form @\"[FQN]\"@, look up
-- the FQN in the parent's registry and parse @value@ through
-- the matched codec. Returns 'Just (Right uf)' on success,
-- 'Just (Left e)' when the bracket key is recognised but parsing
-- fails (so the FromJSON instance can propagate the error), and
-- 'Nothing' when the key isn't bracket-quoted (so the caller
-- treats it as an ordinary field).
parseExtensionEntry
  :: Text                    -- ^ Parent message FQN.
  -> AesonKey.Key
  -> Aeson.Value
  -> Maybe (Either String UnknownField)
parseExtensionEntry parentFqn key val =
  let t = AesonKey.toText key
  in case T.uncons t of
       Just ('[', rest) -> case T.unsnoc rest of
         Just (fqn, ']') -> case lookupExtensionByFqn parentFqn fqn of
           Just codec -> Just (ejcParseValue codec val)
           Nothing    -> Nothing  -- unrecognised extension; ignore
         _ -> Nothing
       _ -> Nothing
{-# INLINE parseExtensionEntry #-}

-- | Internal helper used by tests to drain the registry.
-- (Unused in production code; placed here to keep all the
-- registry primitives together.)
_traverseEntry
  :: (Text -> ExtRegistryEntry -> IO ())
  -> IO ()
_traverseEntry f = do
  m <- readIORef registryRef
  _ <- foldM (\() (k, v) -> f k v) () (Map.toList m)
  pure ()

-- ---------------------------------------------------------------------------
-- Splice-driven codec primitives
-- ---------------------------------------------------------------------------

-- | Parse a JSON value into the wire 'UnknownField' that
-- 'Proto.Extension.encodeExtensionValue' would have produced
-- for the same payload, dispatched on the extension's static
-- 'ExtensionType' constructor. This lets the
-- 'loadProto'-generated registration call stay completely
-- ADT-free at the splice site.
parseExtValueViaConstructor
  :: forall a
   . ExtensionType a -> Int -> Aeson.Value -> Either String UnknownField
parseExtValueViaConstructor ty fn v = case ty of
  ExtBool     -> encodeExtensionValue fn ty <$> parseBool v
  ExtInt32    -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int32)
  ExtInt64    -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int64)
  ExtUInt32   -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Word32)
  ExtUInt64   -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Word64)
  ExtSInt32   -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int32)
  ExtSInt64   -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int64)
  ExtFixed32  -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Word32)
  ExtFixed64  -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Word64)
  ExtSFixed32 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int32)
  ExtSFixed64 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int64)
  ExtFloat    -> encodeExtensionValue fn ty <$> parseFloating v
  ExtDouble   -> encodeExtensionValue fn ty <$> parseFloatingD v
  ExtString   -> encodeExtensionValue fn ty <$> parseStringT v
  ExtBytes    -> encodeExtensionValue fn ty <$> parseBytesB v
  ExtMessage  ->
    -- Embedded sub-messages aren't exercised by the
    -- conformance suite's bracket-syntax tests; if a user
    -- needs them they can register a custom codec.
    Left "JSON serialisation of message-typed extensions is not yet supported"

parseBool :: Aeson.Value -> Either String Bool
parseBool (Aeson.Bool b) = Right b
parseBool _              = Left "Expected JSON Bool"

parseBoundedInt
  :: forall a. (Integral a, Bounded a)
  => Aeson.Value -> Either String a
parseBoundedInt v = case v of
  Aeson.Number n -> coerce n
  Aeson.String s -> case reads (T.unpack s) :: [(Sci.Scientific, String)] of
    [(sci, "")] -> coerce sci
    _           -> Left ("Invalid integer string: " <> show s)
  _ -> Left "Expected JSON Number or String for integer extension"
  where
    coerce sci = case Sci.toBoundedInteger sci of
      Just n  -> Right n
      Nothing -> Left "Extension integer value out of range or non-integer"

parseFloating :: Aeson.Value -> Either String Float
parseFloating v = case AesonT.parseEither PJ.protoFloatFromJSON v of
  Right d -> Right d
  Left e  -> Left e

parseFloatingD :: Aeson.Value -> Either String Double
parseFloatingD v = case AesonT.parseEither PJ.protoDoubleFromJSON v of
  Right d -> Right d
  Left e  -> Left e

parseStringT :: Aeson.Value -> Either String Text
parseStringT (Aeson.String s) = Right s
parseStringT _ = Left "Expected JSON String for string extension"

parseBytesB :: Aeson.Value -> Either String BS.ByteString
parseBytesB v = case AesonT.parseEither PJ.protoBytesFromJSON v of
  Right b -> Right b
  Left e  -> Left e

-- | Encode a stored 'UnknownField' back into the JSON form for
-- the matching extension type. Routes through the existing
-- 'decodeExtensionValue' so the payload-decoding rules stay in
-- one place.
encodeExtValueViaConstructor
  :: ExtensionType a -> UnknownField -> Either String Aeson.Value
encodeExtValueViaConstructor ty uf = case decodeExtensionValue ty uf of
  Nothing -> Left "extension JSON encode: wire-type/extension-type mismatch"
  Just a  -> Right (encodeOne ty a)
  where
    encodeOne :: ExtensionType b -> b -> Aeson.Value
    encodeOne ExtBool     b = Aeson.Bool b
    encodeOne ExtInt32    n = Aeson.Number (fromIntegral n)
    encodeOne ExtInt64    n = PJ.protoInt64ToJSON n
    encodeOne ExtUInt32   n = Aeson.Number (fromIntegral n)
    encodeOne ExtUInt64   n = PJ.protoWord64ToJSON n
    encodeOne ExtSInt32   n = Aeson.Number (fromIntegral n)
    encodeOne ExtSInt64   n = PJ.protoInt64ToJSON n
    encodeOne ExtFixed32  n = Aeson.Number (fromIntegral n)
    encodeOne ExtFixed64  n = PJ.protoWord64ToJSON n
    encodeOne ExtSFixed32 n = Aeson.Number (fromIntegral n)
    encodeOne ExtSFixed64 n = PJ.protoInt64ToJSON n
    encodeOne ExtFloat    f = PJ.protoFloatToJSON f
    encodeOne ExtDouble   d = PJ.protoDoubleToJSON d
    encodeOne ExtString   s = Aeson.String s
    encodeOne ExtBytes    b = PJ.protoBytesToJSON b
    encodeOne ExtMessage  b = PJ.protoBytesToJSON b
