-- | Example: hand-written Protocol Buffers message encode/decode using
-- the wire primitives.
--
-- Encodes a simple Person message:
--   field 1 (string): name
--   field 2 (varint): age
--
-- Run with: cabal run example-protobuf
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Proto.Wire (WireType(..), Tag(..))
import Proto.Wire.Encode (putTag, putVarint, putText)
import Proto.Wire.Decode (Decoder, runDecoder, getTag, getVarint, getText)

main :: IO ()
main = do
  let builder =
        putTag 1 WireLengthDelimited <> putText "Haskell"
        <> putTag 2 WireVarint <> putVarint 42
  let bytes = BL.toStrict (B.toLazyByteString builder)
  putStrLn $ "Protobuf encoded: " ++ show (BS.length bytes) ++ " bytes"

  let decoder :: Decoder (String, Int)
      decoder = do
        Tag fn1 _ <- getTag
        name <- getText
        Tag fn2 _ <- getTag
        age <- getVarint
        pure ("field " ++ show fn1 ++ ": " ++ show name
             ++ ", field " ++ show fn2 ++ ": " ++ show age
             , fromIntegral age)

  case runDecoder decoder bytes of
    Right (desc, _age) -> putStrLn $ "Decoded: " ++ desc
    Left err           -> putStrLn $ "Error: " ++ show err
