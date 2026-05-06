-- | Cross-implementation interop test for wireform-fury.
--
-- For each test case the Haskell encoder writes the bytes,
-- spawns @python3@ with a small read/write driver script that
-- decodes via @pyfory@ (and vice-versa), and asserts the
-- round-trip succeeds in both directions.
--
-- The Python driver reads a length-prefixed stream of (mode,
-- bytes) pairs on stdin and writes the resulting (mode, bytes)
-- pairs back on stdout, so the entire test suite shares one
-- Python sub-process. mode = 'D' (Haskell -> Python decode -> re-encode)
-- or mode = 'E' (encode in Python from a JSON description).
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BB
import qualified Data.Vector as V
import Data.Word (Word32)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import qualified Data.Aeson as A
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Scientific as Sci
import System.Exit (exitFailure)
import System.IO
import System.Process


import qualified Fury.Encode as E
import qualified Fury.Decode as D
import qualified Fury.Value as VV
import System.Directory (doesFileExist)

main :: IO ()
main = do
  -- The interop test can be invoked either from the package
  -- directory (cabal run wireform-fury:wireform-fury-interop) or
  -- from the workspace root (cabal run -v0 wireform-fury-interop).
  -- Try both paths.
  driver <- do
    let candidates =
          [ "test-interop/driver.py"
          , "wireform-fury/test-interop/driver.py"
          ]
        firstExisting [] =
          fail "Could not find test-interop/driver.py relative to the cwd"
        firstExisting (p:ps) = do
          exists <- doesFileExist p
          if exists then pure p else firstExisting ps
    firstExisting candidates
  (Just hin, Just hout, _, ph) <- createProcess
    (proc "python3" [driver])
      { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  hSetBuffering hin  NoBuffering
  hSetBuffering hout NoBuffering

  let cases :: [(String, A.Value, VV.Value)]
      cases =
        [ ("null", A.Null, VV.NoneVal)
        , ("bool true",  A.Bool True,  VV.BoolVal True)
        , ("bool false", A.Bool False, VV.BoolVal False)
        , ("int 0",  num 0,  VV.VarInt64Val 0)
        , ("int 1",  num 1,  VV.VarInt64Val 1)
        , ("int -1", num (-1), VV.VarInt64Val (-1))
        , ("int 42", num 42, VV.VarInt64Val 42)
        , ("int 1000", num 1000, VV.VarInt64Val 1000)
        , ("int 100000", num 100000, VV.VarInt64Val 100000)
        , ("int 2^30", num (2^(30::Int)), VV.VarInt64Val (2^(30::Int)))
        , ("int -2^30", num (-(2^(30::Int))), VV.VarInt64Val (-(2^(30::Int))))
        , ("float 0.0", numF 0.0, VV.Float64Val 0.0)
        , ("float 1.0", numF 1.0, VV.Float64Val 1.0)
        , ("float -1.0", numF (-1.0), VV.Float64Val (-1.0))
        , ("float pi", numF 3.141592653589793, VV.Float64Val 3.141592653589793)
        , ("string empty", A.String "", VV.StringVal "")
        , ("string ascii", A.String "hello", VV.StringVal "hello")
        , ("string latin1", A.String "héllo", VV.StringVal "héllo")
        , ("string utf8 BMP", A.String "naïve café", VV.StringVal "naïve café")
        , ("string longer", A.String (T.replicate 50 "x"), VV.StringVal (T.replicate 50 "x"))
        , ("bytes empty", A.object [("__bytes__", A.String "")], VV.BinaryVal "")
        , ("bytes simple", A.object [("__bytes__", A.String "AAEC")], VV.BinaryVal (BS.pack [0,1,2]))
        , ("list empty", A.Array V.empty, VV.ListVal V.empty)
        , ("list ints", A.Array (V.fromList [num 1, num 2, num 3]),
             VV.ListVal (V.fromList (map VV.VarInt64Val [1,2,3])))
        , ("list strings",
             A.Array (V.fromList [A.String "a", A.String "b", A.String "c"]),
             VV.ListVal (V.fromList (map VV.StringVal ["a","b","c"])))
        , ("list mixed",
             A.Array (V.fromList [num 1, A.String "two", numF 3.0]),
             VV.ListVal (V.fromList
               [VV.VarInt64Val 1, VV.StringVal "two", VV.Float64Val 3.0]))
        , ("list with nulls",
             A.Array (V.fromList [num 1, A.Null, num 3]),
             VV.ListVal (V.fromList
               [VV.VarInt64Val 1, VV.NoneVal, VV.VarInt64Val 3]))
        , ("list of bool",
             A.Array (V.fromList [A.Bool True, A.Bool False, A.Bool True]),
             VV.ListVal (V.fromList (map VV.BoolVal [True, False, True])))
        , ("map empty", A.object [], VV.MapVal V.empty)
        , ("map simple",
             A.object [("a", num 1), ("b", num 2)],
             VV.MapVal (V.fromList
               [(VV.StringVal "a", VV.VarInt64Val 1),
                (VV.StringVal "b", VV.VarInt64Val 2)]))
        , ("map mixed values",
             A.object [("name", A.String "alice"), ("age", num 30)],
             VV.MapVal (V.fromList
               [(VV.StringVal "name", VV.StringVal "alice"),
                (VV.StringVal "age", VV.VarInt64Val 30)]))
        ]

  results <- mapM (runCase hin hout) cases
  let failures = [ (label, why) | (label, Just why) <- zip (map fstOf3 cases) results ]
      fstOf3 (a,_,_) = a
      okCount   = length results - length failures
  putStrLn $ "\n=== interop summary: " ++ show okCount
             ++ " / " ++ show (length results) ++ " passed ==="
  hClose hin
  _ <- waitForProcess ph
  mapM_ printFailure failures
  if null failures
    then putStrLn "All interop cases passed."
    else exitFailure
  where
    num :: Integer -> A.Value
    num = A.Number . fromIntegral
    numF :: Double -> A.Value
    numF d =
      A.object [("__float__", A.Number (Sci.fromFloatDigits d))]
    printFailure (label, why) =
      putStrLn $ "  FAIL " ++ label ++ ": " ++ why

-- | One case has three steps:
--
--   1. Encode in Haskell -> hand bytes to Python -> Python decodes
--      and reports the round-tripped Python value (as JSON).
--      We compare to the expected JSON description.
--
--   2. Encode in Python from the JSON description -> hand bytes
--      to Haskell -> Haskell decodes -> we structurally compare
--      to the expected 'VV.Value'.
--
--   3. (Implicit) The Haskell encode->decode round-trip is
--      already covered by the unit test suite.
runCase
  :: Handle              -- ^ python stdin
  -> Handle              -- ^ python stdout
  -> (String, A.Value, VV.Value)
  -> IO (Maybe String)   -- ^ failure reason, or Nothing on success
runCase hin hout (label, expectedJson, value) = do
  -- Step 1: Haskell -> Python
  let hsBytes = E.encode value
  decodedJson <- pyRoundTrip hin hout 'D' hsBytes
  case decodedJson of
    Left err  -> pure (Just ("python decode error: " ++ err))
    Right gotJson -> do
      if not (jsonEq gotJson expectedJson)
        then do
          putStrLn $ "  FAIL " ++ label ++ ": python decode mismatch"
          putStrLn $ "    haskell bytes: " ++ hexBs hsBytes
          putStrLn $ "    expected json: " ++ show expectedJson
          putStrLn $ "    got      json: " ++ show gotJson
          pure (Just "python decode mismatch")
        else do
          -- Step 2: Python -> Haskell
          let payload = BSL.toStrict (A.encode expectedJson)
          pyBytes <- sendToPy hin hout 'E' payload
          case pyBytes of
            Left err -> pure (Just ("python encode error: " ++ err))
            Right bs -> case D.decode bs of
              Left de -> do
                putStrLn $ "  FAIL " ++ label ++ ": haskell decode of python bytes failed"
                putStrLn $ "    python bytes: " ++ hexBs bs
                putStrLn $ "    error: " ++ de
                pure (Just ("haskell decode error: " ++ de))
              Right got
                | valuesAgree got value -> do
                    putStrLn $ "  ok   " ++ label
                              ++ "  (hs " ++ show (BS.length hsBytes)
                              ++ "B, py " ++ show (BS.length bs) ++ "B)"
                    pure Nothing
                | otherwise -> do
                    putStrLn $ "  FAIL " ++ label ++ ": haskell decode mismatch"
                    putStrLn $ "    python bytes: " ++ hexBs bs
                    putStrLn $ "    expected:     " ++ show value
                    putStrLn $ "    got:          " ++ show got
                    pure (Just "haskell decode mismatch")

-- | Loose equality used to compare a Haskell-decoded value with
-- the test's expected value:
--
-- * Maps are compared as sets of key-value pairs (Fory maps are
--   unordered semantically; pyfory and aeson both happen to
--   reorder).
-- * Lists are compared elementwise but recursively under
--   'valuesAgree'.
-- * Everything else uses '==' on 'VV.Value'.
valuesAgree :: VV.Value -> VV.Value -> Bool
valuesAgree (VV.MapVal a) (VV.MapVal b) =
  let asSet xs = Set.fromList (map shape (V.toList xs))
      shape (k, v) = (show k, show v)
  in V.length a == V.length b && asSet a == asSet b
valuesAgree (VV.ListVal a) (VV.ListVal b) =
  V.length a == V.length b
    && and (V.toList (V.zipWith valuesAgree a b))
valuesAgree a b = a == b

pyRoundTrip
  :: Handle -> Handle -> Char -> BS.ByteString
  -> IO (Either String A.Value)
pyRoundTrip hin hout mode payload = do
  e <- sendToPy hin hout mode payload
  case e of
    Left err -> pure (Left err)
    Right bs ->
      case A.eitherDecodeStrict bs of
        Left e2  -> pure (Left ("invalid json from python: " ++ e2))
        Right j  -> pure (Right j)

sendToPy
  :: Handle -> Handle -> Char -> BS.ByteString
  -> IO (Either String BS.ByteString)
sendToPy hin hout mode payload = do
  BS.hPut hin (BS.pack [fromIntegral (fromEnum mode)])
  BS.hPut hin (writeLen (BS.length payload))
  BS.hPut hin payload
  hFlush hin
  status <- BS.hGet hout 1
  case BS8.unpack status of
    "K" -> do
      lenBs <- BS.hGet hout 4
      let n = readLen lenBs
      Right <$> BS.hGet hout n
    "E" -> do
      lenBs <- BS.hGet hout 4
      let n = readLen lenBs
      err <- BS.hGet hout n
      pure (Left (BS8.unpack err))
    other -> pure (Left ("unexpected python status byte: " ++ other))

writeLen :: Int -> BS.ByteString
writeLen n = BSL.toStrict $ BB.toLazyByteString $ BB.word32LE (fromIntegral n)

readLen :: BS.ByteString -> Int
readLen bs = case BS.unpack bs of
  [b0, b1, b2, b3] ->
    let w :: Word32
        w =   fromIntegral b0
          .|. (fromIntegral b1 `shiftL` 8)
          .|. (fromIntegral b2 `shiftL` 16)
          .|. (fromIntegral b3 `shiftL` 24)
    in fromIntegral w
  _ -> error "readLen: expected exactly 4 bytes from python driver"

hexBs :: BS.ByteString -> String
hexBs bs = concatMap toHex (BS.unpack bs)
  where
    toHex b = [hex (b `shiftR` 4), hex (b .&. 0x0f)]
    hex n
      | n < 10    = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n + fromEnum 'a' - 10)

-- | Loose JSON equality for our test cases: numbers compared with
-- Scientific equality after casting through Double; bytes-blobs
-- represented as @{"__bytes__": "<base64>"}@ on both sides.
jsonEq :: A.Value -> A.Value -> Bool
jsonEq (A.Number a) (A.Number b) =
  let da = Sci.toRealFloat a :: Double
      db = Sci.toRealFloat b :: Double
  in da == db || a == b
jsonEq a b = a == b
