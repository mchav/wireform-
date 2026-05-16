{- | Manual fuzzer that walks Hedgehog-generated values one at a
time, logging each before sending to pyfory. The first value
that causes the python driver to die without sending a status
byte tells us the input that triggers the C-level crash.
-}
module Main (main) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Vector qualified as V
import Data.Word (Word32)
import Fory.Encode qualified as E
import Fory.Value qualified as VV
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Gen qualified as IG
import Hedgehog.Internal.Seed qualified as Seed
import Hedgehog.Internal.Tree qualified as Tree
import Hedgehog.Range qualified as Range
import System.Directory (doesFileExist)
import System.IO
import System.Process
import Wireform.Builder qualified as BB


genValue :: Int -> H.Gen VV.Value
genValue depth =
  Gen.frequency $
    [ (1, pure VV.NoneVal)
    , (2, VV.BoolVal <$> Gen.bool)
    , (3, VV.VarInt64Val <$> Gen.int64 Range.linearBounded)
    , (3, VV.StringVal <$> Gen.text (Range.linear 0 24) Gen.unicode)
    , (1, VV.BinaryVal <$> Gen.bytes (Range.linear 0 24))
    ]
      ++ [ ( 2
           , VV.ListVal . V.fromList
              <$> Gen.list (Range.linear 0 4) (genValue (depth - 1))
           )
         | depth > 0
         ]
      ++ [ ( 1
           , VV.MapVal . V.fromList
              <$> Gen.list
                (Range.linear 0 3)
                ((,) <$> genHashableKey <*> genValue (depth - 1))
           )
         | depth > 0
         ]
  where
    -- pyfory materialises decoded Maps as Python dicts; dict
    -- keys must be hashable, so we only generate hashable
    -- types here. Lists / Sets / Maps as keys produce wire-
    -- valid but Python-undecodeable output.
    genHashableKey =
      Gen.choice
        [ pure VV.NoneVal
        , VV.BoolVal <$> Gen.bool
        , VV.VarInt64Val <$> Gen.int64 Range.linearBounded
        , VV.StringVal <$> Gen.text (Range.linear 0 12) Gen.alpha
        , VV.BinaryVal <$> Gen.bytes (Range.linear 0 12)
        ]


main :: IO ()
main = do
  driver <- findDriver
  (Just hin, Just hout, _, _ph) <-
    createProcess
      (proc "python3" [driver])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        }
  hSetBuffering hin NoBuffering
  hSetBuffering hout NoBuffering

  let walk i seed
        | i >= 1000 = putStrLn "no crash in 1000 cases" >> hClose hin
        | otherwise = do
            let (sa, sb) = Seed.split seed
            let mTree = IG.evalGen 30 sa (genValue 3)
            case mTree of
              Nothing -> walk (i + 1) sb
              Just t -> do
                let v = Tree.treeValue t
                    bytes = E.encode v
                hPutStrLn stderr $
                  "[case "
                    ++ show i
                    ++ "] "
                    ++ show v
                    ++ "  ("
                    ++ show (BS.length bytes)
                    ++ "B)"
                hFlush stderr
                r <- sendD hin hout bytes
                case r of
                  Left e -> do
                    hPutStrLn stderr $ "  ERROR: " ++ e
                    hClose hin
                  Right _ -> walk (i + 1) sb

  s <- Seed.random
  walk 0 s


findDriver :: IO FilePath
findDriver = do
  exists1 <- doesFileExist "wireform-fory/test-interop/driver.py"
  if exists1
    then pure "wireform-fory/test-interop/driver.py"
    else pure "test-interop/driver.py"


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
    other -> pure (Left ("unexpected python status byte: " ++ show other))


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


_hex :: BS.ByteString -> String
_hex bs = concatMap toHex (BS.unpack bs)
  where
    toHex b = [hexC (b `shiftR` 4), hexC (b .&. 0x0f)]
    hexC n
      | n < 10 = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n + fromEnum 'a' - 10)
