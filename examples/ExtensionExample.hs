{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Demonstrates proto2 typed extensions.
--
-- Run with: cabal run example-extensions
module Main where

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Proto.Encode
import Proto.Decode
import Proto.TH
import qualified Proto.Extension as Ext

$(loadProto "example/extension_demo.proto")

main :: IO ()
main = do
  putStrLn "=== Proto2 Extensions Example ===\n"

  -- Start with a plain Account; attach two extensions via the
  -- generated Extension descriptors.
  let a0 = defaultAccount { name = Just "alice" }
      a1 = Ext.setExtension accountLevel   42            a0
      a2 = Ext.setExtension accountTag     "gold-member" a1
      a3 = Ext.setExtension accountDisabled False        a2

  putStrLn $ "Account name:   " <> show (name a3)
  putStrLn $ "level ext:      " <> show (Ext.getExtension accountLevel   a3)
  putStrLn $ "tag ext:        " <> show (Ext.getExtension accountTag     a3)
  putStrLn $ "disabled ext:   " <> show (Ext.getExtension accountDisabled a3)

  -- Round-trip through the wire: extensions live in unknownFields and
  -- are preserved through encode -> decode.
  let bytes = encodeMessage a3
  case decodeMessage bytes of
    Left err -> putStrLn $ "decode error: " <> show err
    Right (recovered :: Account) -> do
      putStrLn ""
      putStrLn $ "After wire round-trip (" <>
        show (BS.length bytes) <> " bytes):"
      putStrLn $ "  name:         " <> show (name recovered)
      putStrLn $ "  level ext:    " <> show (Ext.getExtension accountLevel recovered)
      putStrLn $ "  tag ext:      " <> show (Ext.getExtension accountTag recovered)
      putStrLn $ "  disabled ext: " <> show (Ext.getExtension accountDisabled recovered)

  -- Clearing an extension is the inverse of setExtension.
  let a4 = Ext.clearExtension accountTag a3
  putStrLn ""
  putStrLn "After clearExtension accountTag:"
  putStrLn $ "  hasExtension accountTag:     " <>
    show (Ext.hasExtension accountTag a4)
  putStrLn $ "  getExtension accountTag:     " <>
    show (Ext.getExtension accountTag a4)
  putStrLn $ "  (level still set?)           " <>
    show (Ext.getExtension accountLevel a4)

  putStrLn "\nDone."
