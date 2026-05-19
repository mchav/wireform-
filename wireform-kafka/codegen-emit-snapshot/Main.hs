{-# LANGUAGE OverloadedStrings #-}

-- | One-shot exploratory tool for examining the rendered output of
-- 'Kafka.Protocol.Codegen.WireGenerator' against a hand-built
-- 'ProtocolSchema'.
--
-- Used as a development convenience to iterate on the codegen
-- without having to vendor the full @kafka/clients/src/main/resources/
-- common/message@ tree. The actual Wire snapshot golden tests live
-- in 'Codegen.WireGeneratorSpec' inside the regular test suite.
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Kafka.Protocol.Codegen.Types
import Kafka.Protocol.Codegen.WireGenerator
import Prettyprinter
import Prettyprinter.Render.Text

renderD :: Doc ann -> IO ()
renderD = TIO.putStr . renderStrict . layoutPretty defaultLayoutOptions

main :: IO ()
main = do
  let schemas =
        [ ("RequestHeader",      requestHeader)
        , ("ResponseHeader",     responseHeader)
        , ("ApiVersionsRequest", apiVersionsRequest)
        ]
  mapM_ (\(name, sch) -> do
            putStrLn ("===== " <> name <> " =====")
            case generateWireFunctions sch of
              Just docs -> mapM_ renderD docs
              Nothing   -> putStrLn "wire-unsupported"
            renderD (generateWireCodecOverride sch) >> putStrLn ""
            putStrLn "")
        schemas

----------------------------------------------------------------------
-- Hand-built ProtocolSchema values
----------------------------------------------------------------------

prim :: T.Text -> TypeSpec
prim = PrimitiveType
{-# NOINLINE prim #-}  -- silences -Wno-unused-imports churn

mkField
  :: T.Text                -- ^ name
  -> TypeSpec              -- ^ type
  -> T.Text                -- ^ versions
  -> Maybe T.Text          -- ^ flexibleVersions override
  -> Maybe T.Text          -- ^ nullableVersions
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
      [ mkField "RequestApiKey"     (prim "int16")  "0+" Nothing       Nothing
      , mkField "RequestApiVersion" (prim "int16")  "0+" Nothing       Nothing
      , mkField "CorrelationId"     (prim "int32")  "0+" Nothing       Nothing
      , mkField "ClientId"          (prim "string") "1+" (Just "none") (Just "1+")
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
      [ mkField "CorrelationId" (prim "int32") "0+" Nothing Nothing ]
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
      [ mkField "ClientSoftwareName"    (prim "string") "3+" Nothing Nothing
      , mkField "ClientSoftwareVersion" (prim "string") "3+" Nothing Nothing
      ]
  , schemaCommonStructs     = []
  , schemaAbout             = Nothing
  }
