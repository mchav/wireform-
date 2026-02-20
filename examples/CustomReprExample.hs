{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Example: custom field representations.
--
-- Demonstrates generating proto types with different backing types
-- for string, bytes, and repeated fields.
--
-- Run with: cabal run example-custom-repr
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map

import Proto.Encode
import Proto.Decode
import Proto.TH
import Proto.Repr

-- Generate types where:
--   - BlobMsg.data uses lazy ByteString (good for large payloads)
--   - ConfigEntry uses lists instead of vectors (convenient for small collections)
--   - IdMsg.identifier uses ShortByteString (compact for short IDs)
$(loadProtoWith (defaultLoadOpts
    { loRepConfig = defaultRepConfig
        { rcFieldOverrides = Map.fromList
            [ (("BlobMsg","data"), defaultFieldRep { frBytes = LazyBytesRep })
            , (("IdMsg","identifier"), defaultFieldRep { frBytes = ShortBytesRep })
            ]
        , rcMessageOverrides = Map.fromList
            [ ("ConfigEntry", defaultFieldRep { frRepeated = ListRep })
            ]
        }
    })
  "example/repr_demo.proto")

main :: IO ()
main = do
  putStrLn "=== Custom Representation Example ===\n"

  -- BlobMsg: data field is Lazy ByteString
  let blob = defaultBlobMsg
        { name = "my-file.bin"
        , data' = BL.pack [0..255]
        }
  putStrLn $ "BlobMsg: " <> show (name blob)
  putStrLn $ "  data length: " <> show (BL.length (data' blob))
  let blobEnc = encodeMessage blob
  putStrLn $ "  encoded: " <> show (BS.length blobEnc) <> " bytes"
  case decodeMessage blobEnc of
    Left err -> putStrLn $ "  ERROR: " <> show err
    Right (decoded :: BlobMsg) -> do
      putStrLn $ "  roundtrip: " <> show (decoded == blob)

  -- IdMsg: identifier field is ShortByteString
  putStrLn ""
  let idMsg = defaultIdMsg
        { identifier = SBS.toShort (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])
        , label = "test-id"
        }
  putStrLn $ "IdMsg: " <> show idMsg
  let idEnc = encodeMessage idMsg
  putStrLn $ "  encoded: " <> show (BS.length idEnc) <> " bytes"
  case decodeMessage idEnc of
    Left err -> putStrLn $ "  ERROR: " <> show err
    Right (decoded :: IdMsg) ->
      putStrLn $ "  roundtrip: " <> show (decoded == idMsg)

  putStrLn "\nDone."
