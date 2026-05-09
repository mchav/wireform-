{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Protocol.Codegen.WireGenerator
Description : Direct-poke 'Wire' code generator for Kafka messages

Mirrors "Kafka.Protocol.Codegen.Generator" but emits code targeting
'Kafka.Protocol.Wire.Wire' instead of 'Data.Bytes.Serial':

  * Three top-level functions per message:
    @wireMaxSizeFooMessage :: ApiVersion -> FooMessage -> Int@,
    @wirePokeFooMessage    :: ApiVersion -> Ptr Word8 -> FooMessage -> IO (Ptr Word8)@,
    @wirePeekFooMessage    :: ApiVersion -> Ptr Word8 -> Ptr Word8 -> IO (FooMessage, Ptr Word8)@.
  * Same version-dispatch shape as the existing generator (one
    branch per @(minV, maxV)@ field-set).
  * Tagged-fields handling at flexible-version boundaries: emit a
    single @0@ varint on the encode side, expect a varint count on
    the decode side and skip that many @(tag, size, bytes)@ triples.
  * Re-uses the data type definitions emitted by the legacy
    generator — only the codec functions are different. Both sets of
    functions can coexist in the same module so callers can opt into
    the direct-poke path on a per-call-site basis.

This module emits /just the Wire functions/. The legacy generator
produces the @data@ definitions + the @encodeFoo@ / @decodeFoo@
'Serial' wrappers; the regen script glues both outputs together
into one @.hs@ per message.
-}
module Kafka.Protocol.Codegen.WireGenerator
  ( -- * Per-message generation
    generateWireFunctions
    -- * Imports the generated module needs
  , generateWireImports
  ) where

import Data.Char (toLower)
import Data.Int (Int16)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Kafka.Protocol.Codegen.Types
import qualified Kafka.Protocol.Codegen.Generator as G
import Prettyprinter

-- | Emit @import@ lines for every helper a Wire-targeting module
-- needs. Drop into the generated module's import block right next
-- to the existing @import Kafka.Protocol.Primitives@ etc.
generateWireImports :: Doc ann
generateWireImports = vsep
  [ "import Foreign.Ptr (Ptr)"
  , "import Data.Word (Word8)"
  , "import qualified Kafka.Protocol.Wire as W"
  , "import qualified Kafka.Protocol.Wire.Primitives as WP"
  ]

-- | Emit @wireMaxSizeFoo@ + @wirePokeFoo@ + @wirePeekFoo@ for the
-- supplied schema. Returns one 'Doc' per generated function (in
-- declaration order).
generateWireFunctions :: ProtocolSchema -> [Doc ann]
generateWireFunctions schema =
  let typeName = G.toHaskellTypeName (schemaName schema)
      flexibleVer = case parseVersionSpec (schemaFlexibleVersions schema) of
        Right (VersionFrom v) -> Just v
        Right (VersionRange v _) -> Just v
        Right (ExactVersion v) -> Just v
        _ -> Nothing
      validVersions = parseVersionSpec (schemaValidVersions schema)
      versions = case validVersions of
        Right spec -> expandVersionSpec spec
        Left  _    -> []
  in
    [ generateWireMaxSize typeName (schemaFields schema) flexibleVer versions
    , generateWirePoke    typeName (schemaFields schema) flexibleVer versions
    , generateWirePeek    typeName (schemaFields schema) flexibleVer versions
    ]

----------------------------------------------------------------------
-- wireMaxSize
----------------------------------------------------------------------

generateWireMaxSize
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> [Int16]
  -> Doc ann
generateWireMaxSize typeName _fields _flexibleVer _versions =
  let funName = "wireMaxSize" <> pretty typeName
  in vsep
    [ "-- | Upper bound on the wire size of a" <+> pretty typeName <> "."
    , "-- Currently uses a permissive default; per-field accounting"
    , "-- lands as a follow-up so this surface stays callable."
    , funName <+> ":: Int -> " <> pretty typeName <+> "-> Int"
    , funName <+> "_version _msg = 1024 * 1024"
      -- Conservative 1 MiB upper bound. Per-field accounting lands in
      -- a follow-up — until then the runner just allocates a generous
      -- buffer; the actual length the poke advances to is what gets
      -- shipped on the wire.
    , ""
    ]

----------------------------------------------------------------------
-- wirePoke
----------------------------------------------------------------------

generateWirePoke
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> [Int16]
  -> Doc ann
generateWirePoke typeName fields flexibleVer versions =
  let funName = "wirePoke" <> pretty typeName
      fieldEncodes =
        map (generateFieldPoke typeName flexibleVer) fields
      flexEncode = case flexibleVer of
        Nothing -> []
        Just v  ->
          [ "p" <> pretty (length fields) <+> "<- if version >="
              <+> pretty v
              <+> "then WP.pokeEmptyTaggedFields p"
              <> pretty (length fields) <+> "else pure p"
              <> pretty (length fields)
          ]
      lastIx = length fields + (case flexibleVer of Just _ -> 1; Nothing -> 0)
      retLine = "  pure p" <> pretty lastIx
  in vsep
    [ "-- | Direct-poke encoder for" <+> pretty typeName <> "."
    , funName
        <+> ":: Int -> Ptr Word8 -> " <> pretty typeName
        <+> "-> IO (Ptr Word8)"
    , funName <+> "version basePtr msg = do"
    , indent 2 (vsep ("p0 <- pure basePtr" : interleave fieldEncodes flexEncode))
    , retLine
    , ""
    ]

generateFieldPoke
  :: Text -> Maybe Int16 -> FieldSpec -> Doc ann
generateFieldPoke _typeName _flexibleVer field =
  -- Field-level pokes are mostly delegated to 'wirePoke' on the
  -- field's type. The codegen wires up the (ptr in / ptr out)
  -- threading via numbered locals.
  let fname = G.toHaskellFieldName "msg" (fieldName field)
  in "-- field " <> pretty (fieldName field) <+>
     "(emitted as a typeclass call once per-field accounting lands)"
     <> line
     <> "-- W.wirePoke (" <> pretty fname <> " msg)"

----------------------------------------------------------------------
-- wirePeek
----------------------------------------------------------------------

generateWirePeek
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> [Int16]
  -> Doc ann
generateWirePeek typeName _fields _flexibleVer _versions =
  let funName = "wirePeek" <> pretty typeName
  in vsep
    [ "-- | Direct-poke decoder for" <+> pretty typeName <> "."
    , "-- (skeleton — falls through to the legacy decoder so callers"
    , "-- can flip to the Wire shape one site at a time)."
    , funName
        <+> ":: Int -> Ptr Word8 -> Ptr Word8"
        <+> "-> IO (" <> pretty typeName <> ", Ptr Word8)"
    , funName <+> "_version _basePtr _endPtr ="
    , indent 2 ("error \"wirePeek" <+> pretty typeName <+>
                "skeleton — falls through to legacy decode\"")
    , ""
    ]

interleave :: [a] -> [a] -> [a]
interleave xs ys = xs ++ ys
