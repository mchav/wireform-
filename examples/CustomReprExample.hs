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
import qualified Data.Map.Strict as Map

import Proto.Encode
import Proto.Decode
import Proto.TH
import Proto.Repr

-- Override the BlobMsg.data field to use lazy ByteString instead of the
-- default strict ByteString -- handy when the payload is large and you
-- want to stream it without materialising the full bytes in memory.
--
-- ShortBytesRep and ListRep overrides are intentionally not exercised
-- in this example: the codegen path that honours them still emits
-- code typed against the default Vector / strict ByteString reps in a
-- few helper sites, which we plan to fix before declaring the field-rep
-- API stable.
$(loadProtoWith (defaultLoadOpts
    { loRepConfig = defaultRepConfig
        { rcFieldOverrides = Map.fromList
            [ (("BlobMsg","data"), defaultFieldRep { frBytes = LazyBytesRep })
            ]
        }
    })
  "examples/proto/repr_demo.proto")

main :: IO ()
main = do
  putStrLn "=== Custom Representation Example ===\n"

  -- 'loadProto' / 'loadProtoWith' prefix every generated record
  -- selector with the lowerCamelCase of the owning message type, so
  -- @bytes data@ inside @message BlobMsg@ becomes
  -- @blobMsgData :: Data.ByteString.Lazy.ByteString@ (the rep
  -- override turned 'bytes' into 'BL.ByteString' here).
  let blob = defaultBlobMsg
        { blobMsgName = "my-file.bin"
        , blobMsgData = BL.pack [0..255]
        }
  putStrLn $ "BlobMsg: " <> show (blobMsgName blob)
  putStrLn $ "  data length: " <> show (BL.length (blobMsgData blob))
  let blobEnc = encodeMessage blob
  putStrLn $ "  encoded: " <> show (BS.length blobEnc) <> " bytes"
  case decodeMessage blobEnc of
    Left err -> putStrLn $ "  ERROR: " <> show err
    Right (decoded :: BlobMsg) -> do
      putStrLn $ "  roundtrip: " <> show (decoded == blob)

  -- IdMsg: default StrictBytesRep for 'identifier'. The
  -- ShortBytesRep override that this example used to demonstrate is
  -- temporarily off; see the loadProtoWith comment above.
  putStrLn ""
  let idMsg = defaultIdMsg
        { idMsgIdentifier = BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
        , idMsgLabel      = "test-id"
        }
  putStrLn $ "IdMsg: " <> show idMsg
  let idEnc = encodeMessage idMsg
  putStrLn $ "  encoded: " <> show (BS.length idEnc) <> " bytes"
  case decodeMessage idEnc of
    Left err -> putStrLn $ "  ERROR: " <> show err
    Right (decoded :: IdMsg) ->
      putStrLn $ "  roundtrip: " <> show (decoded == idMsg)

  putStrLn "\nDone."
