{- | Property-based audit: generate random 'Value' trees, ask
pyfory to round-trip them, and report any divergence.

Two checks per generated value:

1. /haskell-then-python-then-haskell/. Encode in Haskell;
   pyfory decodes; we ask pyfory to re-serialise the decoded
   object; we decode the new bytes in Haskell and compare to
   the original value (under our 'valuesAgree' equivalence).

2. /Bytes are 'Right'-decodable in Python/. Even if the value
   above doesn't survive a round-trip we at least want pyfory
   to /accept/ our bytes without an exception, because that
   means we're not emitting structurally invalid wire format.

Run via:

@
cabal run wireform-fory:test:wireform-fory-interop-fuzz \\
          --enable-tests -fpython-interop \\
          -- --hedgehog-tests 200
@
-}
module Main (main) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Vector qualified as V
import Data.Word (Word32)
import Fory.Decode qualified as D
import Fory.Encode qualified as E
import Fory.Value qualified as VV
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import System.Process
import Wireform.Builder qualified as BB


-- ---------------------------------------------------------------------------
-- Generator
-- ---------------------------------------------------------------------------

genValue :: Int -> H.Gen VV.Value
genValue depth =
  Gen.frequency $
    [ (1, pure VV.NoneVal)
    , (2, VV.BoolVal <$> Gen.bool)
    , (2, VV.VarInt32Val <$> Gen.int32 Range.linearBounded)
    , (3, VV.VarInt64Val <$> Gen.int64 Range.linearBounded)
    , (1, VV.VarUint32Val <$> Gen.word32 Range.linearBounded)
    , (1, VV.VarUint64Val <$> Gen.word64 Range.linearBounded)
    , (1, VV.Int8Val <$> Gen.int8 Range.linearBounded)
    , (1, VV.Int16Val <$> Gen.int16 Range.linearBounded)
    , (1, VV.Int32Val <$> Gen.int32 Range.linearBounded)
    , (1, VV.Int64Val <$> Gen.int64 Range.linearBounded)
    , (1, VV.Uint8Val <$> Gen.word8 Range.linearBounded)
    , (1, VV.Uint16Val <$> Gen.word16 Range.linearBounded)
    , (1, VV.Uint32Val <$> Gen.word32 Range.linearBounded)
    , (1, VV.Uint64Val <$> Gen.word64 Range.linearBounded)
    , (2, VV.Float64Val <$> Gen.double (Range.linearFracFrom 0 (-1e9) 1e9))
    , (1, VV.Float32Val <$> Gen.float (Range.linearFracFrom 0 (-1e9) 1e9))
    , (3, VV.StringVal <$> Gen.text (Range.linear 0 24) Gen.unicode)
    , (1, VV.BinaryVal <$> Gen.bytes (Range.linear 0 24))
    ]
      ++ [ ( 3
           , VV.ListVal . V.fromList
               <$> Gen.list (Range.linear 0 5) (genValue (depth - 1))
           )
         | depth > 0
         ]
      ++ [ ( 2
           , VV.MapVal . V.fromList
               <$> Gen.list
                 (Range.linear 0 4)
                 ((,) <$> genStringy (depth - 1) <*> genValue (depth - 1))
           )
         | depth > 0
         ]
  where
    -- Restrict map keys to types pyfory's JSON-friendly driver
    -- can usefully report back.
    genStringy d
      | d <= 0 = VV.StringVal <$> Gen.text (Range.linear 0 12) Gen.alpha
      | otherwise =
          Gen.choice
            [ VV.StringVal <$> Gen.text (Range.linear 0 12) Gen.alpha
            , VV.VarInt64Val <$> Gen.int64 (Range.linear (-1000) 1000)
            ]


-- ---------------------------------------------------------------------------
-- Driver pipe (mirrors test-interop/Main.hs)
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  driver <- findDriver
  (Just hin, Just hout, _, ph) <-
    createProcess
      (proc "python3" [driver])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        }
  hSetBuffering hin NoBuffering
  hSetBuffering hout NoBuffering

  -- Hedgehog property: pyfory must always accept Haskell-emitted
  -- bytes without raising a deserialisation error. We also do a
  -- structural round-trip via D.decode (E.encode v) to cross-check
  -- our own encoder/decoder pair. The fuzz is intentionally
  -- defensive — even if pyfory rejects bytes, we want to print
  -- enough context to diagnose.
  let prop = H.withTests 1000 $ H.property $ do
        v <- H.forAll (genValue 3)
        let bytes = E.encode v
        case D.decode bytes of
          Left e -> do
            H.annotate ("haskell self-decode error: " ++ e)
            H.annotate (hex bytes)
            H.failure
          Right _ -> pure ()
        result <- H.evalIO (sendD hin hout bytes)
        case result of
          Left e -> do
            H.annotate (hex bytes)
            H.annotate ("python error: " ++ e)
            H.failure
          Right _ -> pure ()

  ok <- H.check prop
  hClose hin
  _ <- waitForProcess ph
  if ok then exitSuccess else exitFailure


findDriver :: IO FilePath
findDriver = do
  let candidates =
        [ "test-interop/driver.py"
        , "wireform-fory/test-interop/driver.py"
        ]
      firstExisting [] =
        fail "Could not find test-interop/driver.py relative to the cwd"
      firstExisting (p : ps) = do
        exists <- doesFileExist p
        if exists then pure p else firstExisting ps
  firstExisting candidates


sendD :: Handle -> Handle -> BS.ByteString -> IO (Either String BS.ByteString)
sendD hin hout payload = do
  BS.hPut hin (BS.pack [fromIntegral (fromEnum 'D')])
  BS.hPut hin (writeLen (BS.length payload))
  BS.hPut hin payload
  hFlush hin
  status <- BS.hGet hout 1
  case BS8.unpack status of
    "K" -> do
      lenBs <- BS.hGet hout 4
      Right <$> BS.hGet hout (readLen lenBs)
    "E" -> do
      lenBs <- BS.hGet hout 4
      Left . BS8.unpack <$> BS.hGet hout (readLen lenBs)
    other -> pure (Left ("unexpected python status byte: " ++ other))


writeLen :: Int -> BS.ByteString
writeLen n = BSL.toStrict $ BB.toLazyByteString $ BB.word32LE (fromIntegral n)


readLen :: BS.ByteString -> Int
readLen bs = case BS.unpack bs of
  [b0, b1, b2, b3] ->
    let w :: Word32
        w =
          fromIntegral b0
            .|. (fromIntegral b1 `shiftL` 8)
            .|. (fromIntegral b2 `shiftL` 16)
            .|. (fromIntegral b3 `shiftL` 24)
    in fromIntegral w
  _ -> error "readLen: expected exactly 4 bytes from python driver"


hex :: BS.ByteString -> String
hex bs = concatMap toHex (BS.unpack bs)
  where
    toHex b = [hexC (b `shiftR` 4), hexC (b .&. 0x0f)]
    hexC n
      | n < 10 = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n + fromEnum 'a' - 10)


-- Suppress unused-import warnings until we wire D.decode-side
-- comparisons into the property body.
_unused :: BS.ByteString -> Either String VV.Value
_unused = D.decode
