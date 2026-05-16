{- | For real Protobuf usage, define types in .proto files and use
wireform-gen or the protoc plugin. This example shows the low-level API.

Encodes a simple Person message:
  field 1 (string): name
  field 2 (varint): age

Run with: cabal run example-protobuf
-}
module Main where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Proto.Internal.Wire (Tag (..), WireType (..))
import Proto.Internal.Wire.Decode (Decoder, getTag, getText, getVarint, runDecoder)
import Proto.Internal.Wire.Encode (putTag, putText, putVarint)
import Wireform.Builder qualified as B


main :: IO ()
main = do
  let builder =
        putTag 1 WireLengthDelimited
          <> putText "Haskell"
          <> putTag 2 WireVarint
          <> putVarint 42
  let bytes = BL.toStrict (B.toLazyByteString builder)
  putStrLn $ "Protobuf encoded: " ++ show (BS.length bytes) ++ " bytes"

  let decoder :: Decoder (String, Int)
      decoder = do
        Tag fn1 _ <- getTag
        name <- getText
        Tag fn2 _ <- getTag
        age <- getVarint
        pure
          ( "field "
              ++ show fn1
              ++ ": "
              ++ show name
              ++ ", field "
              ++ show fn2
              ++ ": "
              ++ show age
          , fromIntegral age
          )

  case runDecoder decoder bytes of
    Right (desc, _age) -> putStrLn $ "Decoded: " ++ desc
    Left err -> putStrLn $ "Error: " ++ show err
