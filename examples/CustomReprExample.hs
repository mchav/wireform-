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

import qualified Data.Aeson as Aeson
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
--   - IdMsg.identifier uses ShortByteString (compact for short IDs)
--   - ConfigEntry.tags uses a list instead of Vector (small collections)
--   - Attachments.lazy_blobs / short_blobs use lazy / short bytes
--     for their map values (per-field override on a map<K, bytes>)
$(loadProtoWith (defaultLoadOpts
    { loRepConfig = defaultRepConfig
        { rcFieldOverrides = Map.fromList
            [ (("BlobMsg","data"),         defaultFieldRep { frBytes = LazyBytesRep  })
            , (("IdMsg","identifier"),     defaultFieldRep { frBytes = ShortBytesRep })
            , (("Attachments","lazy_blobs"),  defaultFieldRep { frBytes = LazyBytesRep  })
            , (("Attachments","short_blobs"), defaultFieldRep { frBytes = ShortBytesRep })
            ]
        , rcMessageOverrides = Map.fromList
            [ ("ConfigEntry", defaultFieldRep { frRepeated = ListRep })
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

  -- IdMsg: identifier field is ShortByteString (unpinned, GC-friendly).
  putStrLn ""
  let idMsg = defaultIdMsg
        { idMsgIdentifier = SBS.toShort (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])
        , idMsgLabel      = "test-id"
        }
  putStrLn $ "IdMsg: " <> show idMsg
  let idEnc = encodeMessage idMsg
  putStrLn $ "  encoded: " <> show (BS.length idEnc) <> " bytes"
  case decodeMessage idEnc of
    Left err -> putStrLn $ "  ERROR: " <> show err
    Right (decoded :: IdMsg) ->
      putStrLn $ "  roundtrip: " <> show (decoded == idMsg)

  -- ConfigEntry: repeated 'tags' field is a list rather than a Vector.
  putStrLn ""
  let cfg = defaultConfigEntry
        { configEntryKey   = "log_level"
        , configEntryValue = "info"
        , configEntryTags  = ["staging", "verbose"]
        }
  putStrLn $ "ConfigEntry: " <> show cfg
  let cfgEnc = encodeMessage cfg
  putStrLn $ "  encoded: " <> show (BS.length cfgEnc) <> " bytes"
  case decodeMessage cfgEnc of
    Left err -> putStrLn $ "  ERROR: " <> show err
    Right (decoded :: ConfigEntry) ->
      putStrLn $ "  roundtrip: " <> show (decoded == cfg)

  -- Attachments: map<string, bytes> exercised across all three
  -- BytesRep choices on the *value* side of a proto map.
  putStrLn ""
  let att = defaultAttachments
        { attachmentsStrictBlobs = Map.fromList
            [ ("readme", BS.pack [0x52, 0x6D]) ]
        , attachmentsLazyBlobs   = Map.fromList
            [ ("payload", BL.pack [0x4C, 0x5A]) ]
        , attachmentsShortBlobs  = Map.fromList
            [ ("hash", SBS.toShort (BS.pack [0xAB, 0xCD])) ]
        }
  putStrLn $ "Attachments: " <> show att
  let attEnc = encodeMessage att
  putStrLn $ "  encoded: " <> show (BS.length attEnc) <> " bytes"
  case decodeMessage attEnc of
    Left err -> putStrLn $ "  ERROR: " <> show err
    Right (decoded :: Attachments) ->
      putStrLn $ "  roundtrip: " <> show (decoded == att)

  -- JSON round-trip exercises every rep through the
  -- representation-aware ToJSON / FromJSON helpers, including the
  -- new map<K, bytes> path.
  putStrLn ""
  putStrLn "JSON round-trip (LazyBytesRep / ShortBytesRep / ListRep / map<K,bytes>):"
  let jsBlob = Aeson.toJSON blob
      jsIdMs = Aeson.toJSON idMsg
      jsCfg  = Aeson.toJSON cfg
      jsAtt  = Aeson.toJSON att
  putStrLn $ "  BlobMsg     -> " <> show jsBlob
  putStrLn $ "  IdMsg       -> " <> show jsIdMs
  putStrLn $ "  ConfigEntry -> " <> show jsCfg
  putStrLn $ "  Attachments -> " <> show jsAtt
  case ( Aeson.fromJSON jsBlob :: Aeson.Result BlobMsg
       , Aeson.fromJSON jsIdMs :: Aeson.Result IdMsg
       , Aeson.fromJSON jsCfg  :: Aeson.Result ConfigEntry
       , Aeson.fromJSON jsAtt  :: Aeson.Result Attachments
       ) of
    (Aeson.Success b, Aeson.Success i, Aeson.Success c, Aeson.Success a) ->
      putStrLn $ "  round-trip equal: "
              <> show (b == blob && i == idMsg && c == cfg && a == att)
    (rb, ri, rc, ra) ->
      putStrLn $ "  JSON decode failed: "
              <> show (rb, ri, rc, ra)

  putStrLn "\nDone."
