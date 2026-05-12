{- | Cross-implementation interop test for wireform-fory.

For each test case the Haskell encoder writes the bytes,
spawns @python3@ with a small read/write driver script that
decodes via @pyfory@ (and vice-versa), and asserts the
round-trip succeeds in both directions.

The Python driver reads a length-prefixed stream of (mode,
bytes) pairs on stdin and writes the resulting (mode, bytes)
pairs back on stdout, so the entire test suite shares one
Python sub-process. mode = 'D' (Haskell -> Python decode -> re-encode)
or mode = 'E' (encode in Python from a JSON description).
-}
module Main (main) where

import Data.Aeson qualified as A
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Scientific qualified as Sci
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word32)
import Fory.Decode qualified as D
import Fory.Encode qualified as E
import Fory.Options qualified as O
import Fory.Struct qualified as ST
import Fory.TypeId qualified as TI
import Fory.Value qualified as VV
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO
import System.Process
import Wireform.Builder qualified as BB


main :: IO ()
main = do
  -- The interop test can be invoked either from the package
  -- directory (cabal run wireform-fory:wireform-fory-interop) or
  -- from the workspace root (cabal run -v0 wireform-fory-interop).
  -- Try both paths.
  driver <- do
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
  (Just hin, Just hout, _, ph) <-
    createProcess
      (proc "python3" [driver])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        }
  hSetBuffering hin NoBuffering
  hSetBuffering hout NoBuffering

  let cases :: [(String, A.Value, VV.Value)]
      cases =
        [ ("null", A.Null, VV.NoneVal)
        , ("bool true", A.Bool True, VV.BoolVal True)
        , ("bool false", A.Bool False, VV.BoolVal False)
        , ("int 0", num 0, VV.VarInt64Val 0)
        , ("int 1", num 1, VV.VarInt64Val 1)
        , ("int -1", num (-1), VV.VarInt64Val (-1))
        , ("int 42", num 42, VV.VarInt64Val 42)
        , ("int 1000", num 1000, VV.VarInt64Val 1000)
        , ("int 100000", num 100000, VV.VarInt64Val 100000)
        , ("int 2^30", num (2 ^ (30 :: Int)), VV.VarInt64Val (2 ^ (30 :: Int)))
        , ("int -2^30", num (-(2 ^ (30 :: Int))), VV.VarInt64Val (-(2 ^ (30 :: Int))))
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
        , ("bytes simple", A.object [("__bytes__", A.String "AAEC")], VV.BinaryVal (BS.pack [0, 1, 2]))
        , ("list empty", A.Array V.empty, VV.ListVal V.empty)
        ,
          ( "list ints"
          , A.Array (V.fromList [num 1, num 2, num 3])
          , VV.ListVal (V.fromList (map VV.VarInt64Val [1, 2, 3]))
          )
        ,
          ( "list strings"
          , A.Array (V.fromList [A.String "a", A.String "b", A.String "c"])
          , VV.ListVal (V.fromList (map VV.StringVal ["a", "b", "c"]))
          )
        ,
          ( "list mixed"
          , A.Array (V.fromList [num 1, A.String "two", numF 3.0])
          , VV.ListVal
              ( V.fromList
                  [VV.VarInt64Val 1, VV.StringVal "two", VV.Float64Val 3.0]
              )
          )
        ,
          ( "list with nulls"
          , A.Array (V.fromList [num 1, A.Null, num 3])
          , VV.ListVal
              ( V.fromList
                  [VV.VarInt64Val 1, VV.NoneVal, VV.VarInt64Val 3]
              )
          )
        ,
          ( "list of bool"
          , A.Array (V.fromList [A.Bool True, A.Bool False, A.Bool True])
          , VV.ListVal (V.fromList (map VV.BoolVal [True, False, True]))
          )
        , ("map empty", A.object [], VV.MapVal V.empty)
        ,
          ( "map simple"
          , A.object [("a", num 1), ("b", num 2)]
          , VV.MapVal
              ( V.fromList
                  [ (VV.StringVal "a", VV.VarInt64Val 1)
                  , (VV.StringVal "b", VV.VarInt64Val 2)
                  ]
              )
          )
        ,
          ( "map mixed values"
          , A.object [("name", A.String "alice"), ("age", num 30)]
          , VV.MapVal
              ( V.fromList
                  [ (VV.StringVal "name", VV.StringVal "alice")
                  , (VV.StringVal "age", VV.VarInt64Val 30)
                  ]
              )
          )
        , -- 1-D primitive arrays (NumPy interop)

          ( "ndarray int8 [1,2,3]"
          , ndarray "int8" [num 1, num 2, num 3]
          , VV.Int8ArrayVal (VS.fromList [1, 2, 3])
          )
        ,
          ( "ndarray int16 [1,2,3]"
          , ndarray "int16" [num 1, num 2, num 3]
          , VV.Int16ArrayVal (VS.fromList [1, 2, 3])
          )
        ,
          ( "ndarray int32 [1,2,3]"
          , ndarray "int32" [num 1, num 2, num 3]
          , VV.Int32ArrayVal (VS.fromList [1, 2, 3])
          )
        ,
          ( "ndarray int64 [1,2,3]"
          , ndarray "int64" [num 1, num 2, num 3]
          , VV.Int64ArrayVal (VS.fromList [1, 2, 3])
          )
        ,
          ( "ndarray uint8 [1,2,3]"
          , ndarray "uint8" [num 1, num 2, num 3]
          , VV.Uint8ArrayVal (VS.fromList [1, 2, 3])
          )
        ,
          ( "ndarray float32 [1,2,3]"
          , ndarray "float32" [numF 1.0, numF 2.0, numF 3.0]
          , VV.Float32ArrayVal (VS.fromList [1.0, 2.0, 3.0])
          )
        ,
          ( "ndarray float64 [1.5,-1.5,3.14]"
          , ndarray "float64" [numF 1.5, numF (-1.5), numF 3.14]
          , VV.Float64ArrayVal (VS.fromList [1.5, -1.5, 3.14])
          )
        ,
          ( "ndarray bool [T,F,T]"
          , ndarray "bool" [A.Bool True, A.Bool False, A.Bool True]
          , VV.BoolArrayVal (VS.fromList [1, 0, 1])
          )
        ,
          ( "ndarray int32 empty"
          , ndarray "int32" []
          , VV.Int32ArrayVal VS.empty
          )
        ]

  let refCases :: [(String, A.Value, A.Value, VV.Value)]
      -- (label, expectedDecodeJson, encodeInputJson, value)
      --   * expectedDecodeJson is what pyfory.deserialize is
      --     supposed to produce after decoding our Haskell-emitted
      --     bytes (so it's always the plain, materialised JSON).
      --   * encodeInputJson is what we feed the Python driver to
      --     materialise into a Python object before encoding;
      --     here we use {"__shared__": id, "value": ...} markers
      --     to force pyfory's identity-based ref tracking to
      --     produce REF back-references on the wire.
      refCases =
        [
          ( "three independent inner lists"
          , plainJ
          , plainJ
          , VV.ListVal (V.fromList [innerV, innerV, innerV])
          )
        ,
          ( "single shared inner list (via __shared__)"
          , plainJ
          , A.Array
              ( V.fromList
                  [ shared 1 innerJ
                  , shared 1 innerJ
                  , shared 1 innerJ
                  ]
              )
          , VV.ListVal (V.fromList [innerV, innerV, innerV])
          )
        ]
      innerJ = A.Array (V.fromList (map A.String ["a", "b", "c"]))
      plainJ = A.Array (V.fromList [innerJ, innerJ, innerJ])
      innerV = VV.ListVal (V.fromList (map VV.StringVal ["a", "b", "c"]))
      shared :: Int -> A.Value -> A.Value
      shared sid v =
        A.object
          [ ("__shared__", A.Number (fromIntegral sid))
          , ("value", v)
          ]

  let structCases :: [(String, A.Value, VV.Value)]
      structCases =
        [
          ( "struct Person('alice', 30)"
          , personJ "alice" 30
          , personV "alice" 30
          )
        ,
          ( "struct Point(x=10, y=20)"
          , pointJ 10 20
          , pointV 10 20
          )
        ,
          ( "list of two Person"
          , A.Array (V.fromList [personJ "alice" 30, personJ "bob" 25])
          , VV.ListVal (V.fromList [personV "alice" 30, personV "bob" 25])
          )
        ]
      personJ n a =
        A.object
          [ ("__struct__", A.String "example.Person")
          , ("fields", A.object [("name", A.String n), ("age", num (fromIntegral a))])
          ]
      personV n a =
        VV.RegisteredStructVal
          "example"
          "Person"
          (V.fromList [("name", VV.StringVal n), ("age", VV.VarInt64Val a)])
      pointJ x y =
        A.object
          [ ("__struct__", A.String "geom.Point")
          , ("fields", A.object [("x", num x), ("y", num y)])
          ]
      pointV x y =
        VV.RegisteredStructVal
          "geom"
          "Point"
          (V.fromList [("x", VV.VarInt64Val x), ("y", VV.VarInt64Val y)])

  results <- mapM (runCase hin hout) cases
  refResults <- mapM (runRefCase hin hout) refCases
  structResults <- mapM (runStructCase hin hout) structCases
  let failures =
        [(lbl, why) | ((lbl, _, _), Just why) <- zip cases results]
          ++ [(lbl, why) | ((lbl, _, _, _), Just why) <- zip refCases refResults]
          ++ [(lbl, why) | ((lbl, _, _), Just why) <- zip structCases structResults]
      total = length results + length refResults + length structResults
      okCount = total - length failures
  putStrLn $
    "\n=== interop summary: "
      ++ show okCount
      ++ " / "
      ++ show total
      ++ " passed ==="
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
    ndarray :: T.Text -> [A.Value] -> A.Value
    ndarray dtype vals =
      A.object
        [
          ( "__ndarray__"
          , A.object
              [ ("dtype", A.String dtype)
              , ("values", A.Array (V.fromList vals))
              ]
          )
        ]
    printFailure (label, why) =
      putStrLn $ "  FAIL " ++ label ++ ": " ++ why


{- | One case has three steps:

  1. Encode in Haskell -> hand bytes to Python -> Python decodes
     and reports the round-tripped Python value (as JSON).
     We compare to the expected JSON description.

  2. Encode in Python from the JSON description -> hand bytes
     to Haskell -> Haskell decodes -> we structurally compare
     to the expected 'VV.Value'.

  3. (Implicit) The Haskell encode->decode round-trip is
     already covered by the unit test suite.
-}
runCase
  :: Handle
  -- ^ python stdin
  -> Handle
  -- ^ python stdout
  -> (String, A.Value, VV.Value)
  -> IO (Maybe String)
  -- ^ failure reason, or Nothing on success
runCase hin hout (label, expectedJson, value) = do
  -- Step 1: Haskell -> Python
  let hsBytes = E.encode value
  decodedJson <- pyRoundTrip hin hout 'D' hsBytes
  case decodedJson of
    Left err -> pure (Just ("python decode error: " ++ err))
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
                    putStrLn $
                      "  ok   "
                        ++ label
                        ++ "  (hs "
                        ++ show (BS.length hsBytes)
                        ++ "B, py "
                        ++ show (BS.length bs)
                        ++ "B)"
                    pure Nothing
                | otherwise -> do
                    putStrLn $ "  FAIL " ++ label ++ ": haskell decode mismatch"
                    putStrLn $ "    python bytes: " ++ hexBs bs
                    putStrLn $ "    expected:     " ++ show value
                    putStrLn $ "    got:          " ++ show got
                    pure (Just "haskell decode mismatch")


{- | Run a struct interop case using the registered-struct
encoder/decoder on both sides.
-}
runStructCase
  :: Handle
  -> Handle
  -> (String, A.Value, VV.Value)
  -> IO (Maybe String)
runStructCase hin hout (label, expectedJson, value) = do
  let registry =
        O.registerStruct personSchema $
          O.registerStruct pointSchema $
            O.emptyStructRegistry
      eopts = O.defaultEncodeOptions {O.eoStructRegistry = registry}
      dopts = O.defaultDecodeOptions {O.doStructRegistry = registry}
      hsBytes = E.encodeWith eopts value
  decodedJson <- pyRoundTrip hin hout 'D' hsBytes
  case decodedJson of
    Left err -> pure (Just ("python decode error: " ++ err))
    Right gotJson | not (jsonEq gotJson expectedJson) -> do
      putStrLn $ "  FAIL " ++ label ++ ": python decode mismatch"
      putStrLn $ "    haskell bytes: " ++ hexBs hsBytes
      putStrLn $ "    expected json: " ++ show expectedJson
      putStrLn $ "    got      json: " ++ show gotJson
      pure (Just "python decode mismatch")
    Right _ -> do
      let payload = BSL.toStrict (A.encode expectedJson)
      pyBytes <- sendToPy hin hout 'E' payload
      case pyBytes of
        Left err -> pure (Just ("python encode error: " ++ err))
        Right bs ->
          if bs == hsBytes
            then do
              putStrLn $
                "  ok   struct "
                  ++ label
                  ++ "  ("
                  ++ show (BS.length hsBytes)
                  ++ "B identical)"
              pure Nothing
            else case D.decodeWith dopts bs of
              Left de -> do
                putStrLn $ "  FAIL " ++ label ++ ": haskell decode of python bytes failed"
                putStrLn $ "    python bytes : " ++ hexBs bs
                putStrLn $ "    haskell bytes: " ++ hexBs hsBytes
                putStrLn $ "    error: " ++ de
                pure (Just ("haskell decode error: " ++ de))
              Right got
                | got == value -> do
                    putStrLn $
                      "  ok   struct "
                        ++ label
                        ++ "  (hs "
                        ++ show (BS.length hsBytes)
                        ++ "B, py "
                        ++ show (BS.length bs)
                        ++ "B)"
                    pure Nothing
                | otherwise -> do
                    putStrLn $ "  FAIL " ++ label ++ ": haskell decode value mismatch"
                    putStrLn $ "    python bytes: " ++ hexBs bs
                    putStrLn $ "    expected: " ++ show value
                    putStrLn $ "    got     : " ++ show got
                    pure (Just "haskell decode value mismatch")
  where
    personSchema =
      ST.mkSchema
        "example"
        "Person"
        [("name", TI.STRING), ("age", TI.VARINT64)]
    pointSchema =
      ST.mkSchema
        "geom"
        "Point"
        [("x", TI.VARINT64), ("y", TI.VARINT64)]


{- | Like 'runCase' but uses pyfory's @ref=True@ encoder /
decoder on the Python side and the Haskell encoder's
@eoRefTracking = True@ option. Verifies bidirectional byte
compatibility under reference tracking.
-}
runRefCase
  :: Handle
  -> Handle
  -> (String, A.Value, A.Value, VV.Value)
  -> IO (Maybe String)
runRefCase hin hout (label, expectedDecodeJson, encodeInputJson, value) = do
  let opts = O.defaultEncodeOptions {O.eoRefTracking = True}
      dopts = O.defaultDecodeOptions {O.doRefTracking = True}
      hsBytes = E.encodeWith opts value
  decodedJson <- pyRoundTrip hin hout 'R' hsBytes
  case decodedJson of
    Left err -> pure (Just ("python ref-decode error: " ++ err))
    Right gotJson | not (jsonEq gotJson expectedDecodeJson) -> do
      putStrLn $ "  FAIL " ++ label ++ ": python ref-decode mismatch"
      putStrLn $ "    haskell bytes: " ++ hexBs hsBytes
      putStrLn $ "    expected json: " ++ show expectedDecodeJson
      putStrLn $ "    got      json: " ++ show gotJson
      pure (Just "python ref-decode mismatch")
    Right _ -> do
      let payload = BSL.toStrict (A.encode encodeInputJson)
      pyBytes <- sendToPy hin hout 'S' payload
      case pyBytes of
        Left err -> pure (Just ("python ref-encode error: " ++ err))
        Right bs -> case D.decodeWith dopts bs of
          Left de -> do
            putStrLn $ "  FAIL " ++ label ++ ": haskell ref-decode of python bytes failed"
            putStrLn $ "    python bytes: " ++ hexBs bs
            putStrLn $ "    error: " ++ de
            pure (Just ("haskell ref-decode error: " ++ de))
          Right got -> do
            putStrLn $
              "  ok   ref "
                ++ label
                ++ "  (hs "
                ++ show (BS.length hsBytes)
                ++ "B, py "
                ++ show (BS.length bs)
                ++ "B)"
            -- Compare structurally; ref ids may be remapped.
            let ok = stripRefIds got `eqValue` stripRefIds value
            if ok
              then pure Nothing
              else do
                putStrLn $ "    expected: " ++ show value
                putStrLn $ "    got     : " ++ show got
                pure (Just "haskell ref-decode mismatch")


{- | Erase all 'VV.RefVal' wire-id integers (replace with -1) so
structural comparison ignores the encoder's auto-assigned ids.
-}
stripRefIds :: VV.Value -> VV.Value
stripRefIds (VV.RefVal _ inner) = stripRefIds inner
stripRefIds (VV.ListVal xs) = VV.ListVal (V.map stripRefIds xs)
stripRefIds (VV.SetVal xs) = VV.SetVal (V.map stripRefIds xs)
stripRefIds (VV.MapVal kvs) =
  VV.MapVal (V.map (\(k, v) -> (stripRefIds k, stripRefIds v)) kvs)
stripRefIds (VV.StructVal a b fs) =
  VV.StructVal a b (V.map (\(k, v) -> (k, stripRefIds v)) fs)
stripRefIds (VV.CompatibleStructVal a b fs) =
  VV.CompatibleStructVal a b (V.map (\(k, v) -> (k, stripRefIds v)) fs)
stripRefIds v = v


eqValue :: VV.Value -> VV.Value -> Bool
eqValue = (==)


{- | Loose equality used to compare a Haskell-decoded value with
the test's expected value:

* Maps are compared as sets of key-value pairs (Fory maps are
  unordered semantically; pyfory and aeson both happen to
  reorder).
* Lists are compared elementwise but recursively under
  'valuesAgree'.
* Everything else uses '==' on 'VV.Value'.
-}
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
  :: Handle
  -> Handle
  -> Char
  -> BS.ByteString
  -> IO (Either String A.Value)
pyRoundTrip hin hout mode payload = do
  e <- sendToPy hin hout mode payload
  case e of
    Left err -> pure (Left err)
    Right bs ->
      case A.eitherDecodeStrict bs of
        Left e2 -> pure (Left ("invalid json from python: " ++ e2))
        Right j -> pure (Right j)


sendToPy
  :: Handle
  -> Handle
  -> Char
  -> BS.ByteString
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
        w =
          fromIntegral b0
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
      | n < 10 = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n + fromEnum 'a' - 10)


{- | Loose JSON equality for our test cases: numbers compared with
Scientific equality after casting through Double; bytes-blobs
represented as @{"__bytes__": "<base64>"}@ on both sides.
-}
jsonEq :: A.Value -> A.Value -> Bool
jsonEq (A.Number a) (A.Number b) =
  let da = Sci.toRealFloat a :: Double
      db = Sci.toRealFloat b :: Double
  in da == db || a == b
jsonEq a b = a == b
