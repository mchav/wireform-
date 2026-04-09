-- | This format uses schema-driven codegen. For real usage:
-- wireform-gen asn1 -i schema.asn1 -o src/Gen/
-- Then use the generated types directly.
--
-- Example: create an ASN.1 SEQUENCE (simplified X.509 field), DER encode,
-- and BER decode.
--
-- Run with: cabal run example-asn1
module Main where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified ASN1.Value as A
import qualified ASN1.Encode as AE
import qualified ASN1.Decode as AD

main :: IO ()
main = do
  let oid = A.OID (V.fromList [2, 5, 4, 3])
  let tbsCert = A.Sequence $ V.fromList
        [ A.Tagged A.ContextSpecific 0 (A.Integer 2)
        , A.Integer 12345
        , A.Sequence $ V.fromList [oid, A.Null]
        , A.Sequence $ V.fromList
            [ A.Set $ V.fromList
                [ A.Sequence $ V.fromList
                    [ oid
                    , A.UTF8String "example.com"
                    ]
                ]
            ]
        , A.UTCTime "250101000000Z"
        ]

  let bytes = AE.encode tbsCert
  putStrLn $ "DER encoded: " ++ show (BS.length bytes) ++ " bytes"

  case AD.decode bytes of
    Right val -> putStrLn $ "BER decoded: " ++ show val
    Left err  -> putStrLn $ "Error: " ++ err

-- ---------------------------------------------------------------------------
-- Alternative approaches
-- ---------------------------------------------------------------------------

-- Approach 1: Schema-driven API (as shown above)

-- Approach 2: TH from ASN.1 module
--   [asn1|
--     MyModule DEFINITIONS ::= BEGIN
--       Person ::= SEQUENCE {
--         name UTF8String,
--         age INTEGER
--       }
--     END
--   |]

-- Approach 3: CLI codegen
--   wireform-gen asn1 -i schema.asn1 -o src/Gen/
