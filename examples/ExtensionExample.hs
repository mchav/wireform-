{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Demonstrates proto2 typed extensions.

Run with: cabal run example-extensions
-}
module Main where

import Data.ByteString qualified as BS
import Data.Reflection (Given (..))
import Proto.Decode
import Proto.Encode
import Proto.Extension qualified as Ext
import Proto.Internal.JSON.Extension (ExtensionRegistry, emptyExtensionRegistry)
import Proto.TH


-- The generated JSON instances carry a 'Given ExtensionRegistry' constraint
-- for proto2 extensions. Satisfy it with the empty registry; the typed
-- 'Ext.setExtension' / 'Ext.getExtension' surface in this example doesn't
-- touch the JSON path.
instance Given ExtensionRegistry where
  given = emptyExtensionRegistry


$(loadProto "examples/proto/extension_demo.proto")


main :: IO ()
main = do
  putStrLn "=== Proto2 Extensions Example ===\n"

  -- Start with a plain Account; attach two extensions via the
  -- generated Extension descriptors.
  -- 'loadProto' scopes record selectors by the lowerCamelCase of
  -- the owning message; @optional string name@ on @message Account@
  -- becomes @accountName :: Maybe Text@.
  let a0 = defaultAccount {accountName = Just "alice"}
      a1 = Ext.setExtension accountLevel 42 a0
      a2 = Ext.setExtension accountTag "gold-member" a1
      a3 = Ext.setExtension accountDisabled False a2

  putStrLn $ "Account name:   " <> show (accountName a3)
  putStrLn $ "level ext:      " <> show (Ext.getExtension accountLevel a3)
  putStrLn $ "tag ext:        " <> show (Ext.getExtension accountTag a3)
  putStrLn $ "disabled ext:   " <> show (Ext.getExtension accountDisabled a3)

  -- Round-trip through the wire: extensions live in unknownFields and
  -- are preserved through encode -> decode.
  let bytes = encodeMessage a3
  case decodeMessage bytes of
    Left err -> putStrLn $ "decode error: " <> show err
    Right (recovered :: Account) -> do
      putStrLn ""
      putStrLn $
        "After wire round-trip ("
          <> show (BS.length bytes)
          <> " bytes):"
      putStrLn $ "  name:         " <> show (accountName recovered)
      putStrLn $ "  level ext:    " <> show (Ext.getExtension accountLevel recovered)
      putStrLn $ "  tag ext:      " <> show (Ext.getExtension accountTag recovered)
      putStrLn $ "  disabled ext: " <> show (Ext.getExtension accountDisabled recovered)

  -- Repeated extensions: scalars, packed scalars, strings.
  let a3a = Ext.setRepeatedExtension accountScores [10, 20, 30] a3
      a3b = Ext.setRepeatedExtension accountPackedScores [1, 2, 3, 4] a3a
      a3c = Ext.setRepeatedExtension accountAliases ["a", "bb", "ccc"] a3b
      a3d = Ext.appendRepeatedExtension accountScores 99 a3c
  putStrLn ""
  putStrLn "Repeated extensions:"
  putStrLn $ "  scores:        " <> show (Ext.getRepeatedExtension accountScores a3d)
  putStrLn $ "  packed_scores: " <> show (Ext.getRepeatedExtension accountPackedScores a3d)
  putStrLn $ "  aliases:       " <> show (Ext.getRepeatedExtension accountAliases a3d)

  -- Round-trip: encode + decode should preserve repeated extensions
  -- in both packed and unpacked encodings.
  let rtBytes = encodeMessage a3d
  case decodeMessage rtBytes of
    Left err -> putStrLn $ "  decode error: " <> show err
    Right (rt :: Account) -> do
      putStrLn $ "After repeated round-trip (" <> show (BS.length rtBytes) <> " bytes):"
      putStrLn $ "  scores:        " <> show (Ext.getRepeatedExtension accountScores rt)
      putStrLn $ "  packed_scores: " <> show (Ext.getRepeatedExtension accountPackedScores rt)
      putStrLn $ "  aliases:       " <> show (Ext.getRepeatedExtension accountAliases rt)

  -- Clearing an extension is the inverse of setExtension.
  let a4 = Ext.clearExtension accountTag a3
  putStrLn ""
  putStrLn "After clearExtension accountTag:"
  putStrLn $
    "  hasExtension accountTag:     "
      <> show (Ext.hasExtension accountTag a4)
  putStrLn $
    "  getExtension accountTag:     "
      <> show (Ext.getExtension accountTag a4)
  putStrLn $
    "  (level still set?)           "
      <> show (Ext.getExtension accountLevel a4)

  putStrLn "\nDone."
