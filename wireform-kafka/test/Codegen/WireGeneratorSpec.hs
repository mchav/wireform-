{-# LANGUAGE OverloadedStrings #-}

-- | Snapshot tests for "Kafka.Protocol.Codegen.WireGenerator".
--
-- The hand-edited @Generated/RequestHeader.hs@ /
-- @Generated/ResponseHeader.hs@ / @Generated/ApiVersionsRequest.hs@
-- modules carry a native @Wire@ codec block that exactly mirrors what
-- this generator emits. The tests below pin the generator's output so
-- a drift between the codegen and the hand-edited modules surfaces
-- as a test failure (rather than a silent codec divergence on the
-- wire).
--
-- Two invariants per supported schema:
--
--   1. The renderer emits non-empty 'wireMaxSize' / 'wirePoke' /
--      'wirePeek' functions and a 'WireCodec' instance pointing at
--      them.
--   2. Key per-field substrings (the right poke/peek primitive for
--      each field's type, the tagged-fields trailer on flexible
--      versions, the per-field @flexibleVersions: none@ opt-out)
--      appear in the rendered text.
module Codegen.WireGeneratorSpec (tests) where

import qualified Data.Text as T
import Data.Text (Text)

import Test.Syd

import Prettyprinter
import Prettyprinter.Render.Text

import Kafka.Protocol.Codegen.Types
import Kafka.Protocol.Codegen.WireGenerator
  ( generateWireFunctions
  , generateWireCodecOverride
  , isWireSupported
  )

tests :: Spec
tests = describe "WireGenerator (codegen snapshot)" $ sequence_
  [ describe "RequestHeader" $ sequence_
      [ it "isWireSupported = True"
          (assertSupported requestHeader)
      , it "renders wireMaxSize / wirePoke / wirePeek"
          (assertRendersFunctions requestHeader)
      , it "uses pokeKafkaString for ClientId (flexibleVersions: none opt-out)"
          (assertRenderedContains requestHeader
             [ "WP.pokeKafkaString p3 (requestHeaderClientId msg)"
             , "WP.peekKafkaString p3 endPtr"
             ])
      , it "v2 emits the empty tagged-fields trailer"
          (assertRenderedContains requestHeader
             [ "WP.pokeEmptyTaggedFields p4"
             , "WP.peekAndSkipTaggedFields p4 endPtr"
             ])
      , it "WireCodec override points at the natives"
          (assertOverrideContains requestHeader
             [ "instance WC.WireCodec RequestHeader where"
             , -- After the no-Maybe migration the codec is a direct
               -- 'WireCodecImpl' value rather than a Just-wrapper.
               "wireCodec = WC.WireCodecImpl"
             , "wireMaxSizeRequestHeader"
             , "wirePokeRequestHeader"
             , "wirePeekRequestHeader"
             ])
      ]
  , describe "ResponseHeader" $ sequence_
      [ it "isWireSupported = True"
          (assertSupported responseHeader)
      , it "v0 has no tagged-fields trailer; v1 does"
          (assertRenderedContains responseHeader
             [ "version == 0"
             , "version == 1"
             , "WP.pokeEmptyTaggedFields p1"
             , "WP.peekAndSkipTaggedFields p1 endPtr"
             ])
      ]
  , describe "ApiVersionsRequest" $ sequence_
      [ it "isWireSupported = True"
          (assertSupported apiVersionsRequest)
      , it "v3-4 uses pokeCompactString for both string fields"
          (assertRenderedContains apiVersionsRequest
             [ "WP.pokeCompactString p0 (P.toCompactString (apiVersionsRequestClientSoftwareName msg))"
             , "WP.pokeCompactString p1 (P.toCompactString (apiVersionsRequestClientSoftwareVersion msg))"
             ])
      , it "v0-2 decode falls through to defaulted KafkaString Null"
          (assertRenderedContains apiVersionsRequest
             [ "P.KafkaString Null" ])
      ]
  ]

----------------------------------------------------------------------
-- assertions
----------------------------------------------------------------------

assertSupported :: ProtocolSchema -> IO ()
assertSupported sch =
  (if (isWireSupported sch) then pure () else expectationFailure ("isWireSupported should be True for "
                 <> T.unpack (schemaName sch)))

assertRendersFunctions :: ProtocolSchema -> IO ()
assertRendersFunctions sch = case generateWireFunctions sch of
  Nothing -> (False) `shouldBe` True
  Just docs -> do
    let !rendered = T.unlines (map render docs)
    mapM_ (assertSubstring sch rendered)
      [ "wireMaxSize" <> schemaName sch
      , "wirePoke"    <> schemaName sch
      , "wirePeek"    <> schemaName sch
      ]

assertRenderedContains :: ProtocolSchema -> [Text] -> IO ()
assertRenderedContains sch needles = case generateWireFunctions sch of
  Nothing -> (False) `shouldBe` True
  Just docs -> do
    let !rendered = T.unlines (map render docs)
    mapM_ (assertSubstring sch rendered) needles

assertOverrideContains :: ProtocolSchema -> [Text] -> IO ()
assertOverrideContains sch needles = do
  -- 'generateWireCodecOverride' now /always/ returns a Doc (no
  -- Nothing fallback after the no-fallback migration). The branch
  -- below assumes this and just renders + searches the output.
  let !rendered = render (generateWireCodecOverride sch)
  mapM_ (assertSubstring sch rendered) needles

assertSubstring :: ProtocolSchema -> Text -> Text -> IO ()
assertSubstring sch hay needle = (if (needle `T.isInfixOf` hay) then pure () else expectationFailure (T.unpack (schemaName sch) <> ": expected substring "
    <> show needle <> " in rendered output:\n" <> T.unpack hay))

render :: Doc ann -> Text
render = renderStrict . layoutPretty defaultLayoutOptions

----------------------------------------------------------------------
-- Hand-built ProtocolSchema fixtures matching the Kafka 4.0 JSON
----------------------------------------------------------------------

mkField
  :: Text             -- ^ name
  -> TypeSpec         -- ^ type
  -> Text             -- ^ versions
  -> Maybe Text       -- ^ flexibleVersions override
  -> Maybe Text       -- ^ nullableVersions
  -> FieldSpec
mkField n t vs flex nul = FieldSpec
  { fieldName             = n
  , fieldType             = t
  , fieldVersions         = vs
  , fieldTag              = Nothing
  , fieldTaggedVersions   = Nothing
  , fieldNullableVersions = nul
  , fieldFlexibleVersions = flex
  , fieldDefault          = Nothing
  , fieldIgnorable        = False
  , fieldEntityType       = Nothing
  , fieldAbout            = Nothing
  , fieldFields           = Nothing
  }

requestHeader :: ProtocolSchema
requestHeader = ProtocolSchema
  { schemaApiKey            = Nothing
  , schemaType              = "header"
  , schemaName              = "RequestHeader"
  , schemaValidVersions     = "1-2"
  , schemaFlexibleVersions  = "2+"
  , schemaFields =
      [ mkField "RequestApiKey"     (PrimitiveType "int16")  "0+" Nothing       Nothing
      , mkField "RequestApiVersion" (PrimitiveType "int16")  "0+" Nothing       Nothing
      , mkField "CorrelationId"     (PrimitiveType "int32")  "0+" Nothing       Nothing
      , mkField "ClientId"          (PrimitiveType "string") "1+" (Just "none") (Just "1+")
      ]
  , schemaCommonStructs     = []
  , schemaAbout             = Nothing
  }

responseHeader :: ProtocolSchema
responseHeader = ProtocolSchema
  { schemaApiKey            = Nothing
  , schemaType              = "header"
  , schemaName              = "ResponseHeader"
  , schemaValidVersions     = "0-1"
  , schemaFlexibleVersions  = "1+"
  , schemaFields =
      [ mkField "CorrelationId" (PrimitiveType "int32") "0+" Nothing Nothing ]
  , schemaCommonStructs     = []
  , schemaAbout             = Nothing
  }

apiVersionsRequest :: ProtocolSchema
apiVersionsRequest = ProtocolSchema
  { schemaApiKey            = Just 18
  , schemaType              = "request"
  , schemaName              = "ApiVersionsRequest"
  , schemaValidVersions     = "0-4"
  , schemaFlexibleVersions  = "3+"
  , schemaFields =
      [ mkField "ClientSoftwareName"    (PrimitiveType "string") "3+" Nothing Nothing
      , mkField "ClientSoftwareVersion" (PrimitiveType "string") "3+" Nothing Nothing
      ]
  , schemaCommonStructs     = []
  , schemaAbout             = Nothing
  }
