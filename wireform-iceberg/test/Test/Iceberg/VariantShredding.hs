{-# LANGUAGE OverloadedStrings #-}
-- | Unit + pyarrow round-trip tests for "Iceberg.Variant.Shredding".
module Test.Iceberg.VariantShredding (tests) where

import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector as V
import qualified System.Process as Proc
import System.Exit (ExitCode (..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Iceberg.Variant as IV
import qualified Iceberg.Variant.Shredding as Shred

tests :: TestTree
tests = testGroup "Iceberg.Variant.Shredding"
  [ testCase "routeRow: matched primitives go to typed_value" $ do
      isTyped (Shred.routeRow Shred.ShredInt32 (Just (IV.VInt32 42))) @?= True
      isTyped (Shred.routeRow Shred.ShredString (Just (IV.VString "hi"))) @?= True
      isTyped (Shred.routeRow Shred.ShredBool (Just (IV.VBool True))) @?= True

  , testCase "routeRow: unmatched primitives fall through to value" $ do
      case Shred.routeRow Shred.ShredInt32 (Just (IV.VString "n/a")) of
        Shred.ShredAsValue bs ->
          assertBool "non-empty Variant value bytes" (BS.length bs > 0)
        other -> assertFailure ("expected ShredAsValue, got " ++ show other)

  , testCase "routeRow: Nothing -> ShredVariantNull" $ do
      Shred.routeRow Shred.ShredInt32 Nothing @?= Shred.ShredVariantNull

  , testCase "routeRow: VNull -> ShredVariantNull" $ do
      Shred.routeRow Shred.ShredInt32 (Just IV.VNull)
        @?= Shred.ShredVariantNull

  , testCase "routeRow: integer widening" $ do
      isTyped (Shred.routeRow Shred.ShredInt32 (Just (IV.VInt8 7))) @?= True
      isTyped (Shred.routeRow Shred.ShredInt64 (Just (IV.VInt32 42))) @?= True
      isTyped (Shred.routeRow Shred.ShredInt64 (Just (IV.VInt16 (-3)))) @?= True

  , testCase "buildShreddedVariantParquetFile: Int64 routing" $ do
      let -- shared metadata: empty dictionary (the spec's smallest)
          meta = BS.pack [0x01, 0x00]
          rows = V.fromList
            [ Just (IV.VInt64 100)
            , Just (IV.VString "n/a")  -- falls through
            , Nothing                   -- variant null
            , Just (IV.VInt64 200)
            ]
      case Shred.buildShreddedVariantParquetFile "measurement" meta
             Shred.ShredInt64 rows of
        Left e -> assertFailure e
        Right bytes ->
          assertBool "produced non-empty file" (BS.length bytes > 0)

  , testCase "reconstructVariant: typed sub-column lifts to Variant" $ do
      let meta = BS.pack [0x01, 0x00]
      Shred.reconstructVariant meta
        (Shred.ShreddedColumn Nothing (Just (Shred.TVInt64 100)))
        @?= Right (Just (IV.VInt64 100))

  , testCase "reconstructVariant: unshredded value is decoded with shared metadata" $ do
      let v = IV.VString "fallback"
          (m, valBytes) = IV.encodeVariant v
      Shred.reconstructVariant m
        (Shred.ShreddedColumn (Just valBytes) Nothing)
        @?= Right (Just v)

  , testCase "reconstructVariant: missing row -> Right Nothing" $ do
      let meta = BS.pack [0x01, 0x00]
      Shred.reconstructVariant meta
        (Shred.ShreddedColumn Nothing Nothing)
        @?= Right Nothing

  , testCase "reconstructVariant: partially-shredded object surfaces as Left" $ do
      let v = IV.VString "x"
          (m, valBytes) = IV.encodeVariant v
      case Shred.reconstructVariant m
             (Shred.ShreddedColumn (Just valBytes)
                (Just (Shred.TVInt32 7))) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected Left for partial shred"

  , testCase "shred -> reconstruct round-trip preserves the Variant" $ do
      -- Build a shredded column row from the encoder side and
      -- reconstruct it from the decoder side; the result should
      -- equal the original Variant. The shared metadata is the
      -- canonical empty-dictionary value (header 0x11, size 0,
      -- one zero offset).
      let meta = BS.pack [0x11, 0x00, 0x00]
          -- Tuples are (input, ShreddedType, expected post-roundtrip).
          -- Integer inputs that widen on the encoder side come back
          -- as the wider Variant type; the type-mismatch fallthrough
          -- decodes the re-encoded value bytes back to the original.
          cases =
            [ (IV.VInt64 42,             Shred.ShredInt64,  IV.VInt64 42)
            , (IV.VString "hello",       Shred.ShredString, IV.VString "hello")
            , (IV.VBool True,            Shred.ShredBool,   IV.VBool True)
            , (IV.VInt8 7,               Shred.ShredInt32,  IV.VInt32 7)
            , (IV.VString "type-mismatch", Shred.ShredInt64,
                                                            IV.VString "type-mismatch")
            ]
      flip mapM_ cases $ \(v, st, expected) ->
        let row = Shred.routeRow st (Just v)
            sc = case row of
              Shred.ShredAsTyped tv ->
                Shred.ShreddedColumn Nothing (Just tv)
              Shred.ShredAsValue bs ->
                Shred.ShreddedColumn (Just bs) Nothing
              _ -> Shred.ShreddedColumn Nothing Nothing
         in case Shred.reconstructVariant meta sc of
              Right (Just v') -> v' @?= expected
              other -> assertFailure
                ("shred->reconstruct mismatch for " ++ show v
                   ++ ": " ++ show other)

  , testGroup "object shredding"
    [ testCase "fully-shredded object: all fields routed to typed_value" $ do
        let oss = Shred.ObjectShreddingSchema
                    [ Shred.ShreddedField "event_type" Shred.ShredString
                    , Shred.ShreddedField "event_ts"   Shred.ShredInt64
                    ]
            v = IV.VObject (Map.fromList
                  [ ("event_type", IV.VString "noop")
                  , ("event_ts",   IV.VInt64 1729794114937)
                  ])
            row = Shred.routeObjectRow oss (Just v)
        Shred.osrValue row @?= Nothing
        case Shred.osrTypedFields row of
          Just fs -> do
            map fst fs @?= ["event_type", "event_ts"]
            -- Both fields routed to typed_value (ShredAsTyped).
            map (isShredAsTyped . snd) fs @?= [True, True]
          Nothing -> assertFailure "expected typed fields"

    , testCase "partially-shredded object: extras flow to value" $ do
        let oss = Shred.ObjectShreddingSchema
                    [ Shred.ShreddedField "event_type" Shred.ShredString
                    , Shred.ShreddedField "event_ts"   Shred.ShredInt64
                    ]
            v = IV.VObject (Map.fromList
                  [ ("event_type", IV.VString "login")
                  , ("event_ts",   IV.VInt64 1729794146402)
                  , ("email",      IV.VString "user@example.com")
                  ])
            row = Shred.routeObjectRow oss (Just v)
        case Shred.osrValue row of
          Just bs -> assertBool "non-empty fallback bytes" (BS.length bs > 0)
          Nothing -> assertFailure "expected value bytes for partial shred"
        case Shred.osrTypedFields row of
          Just fs ->
            map (isShredAsTyped . snd) fs @?= [True, True]
          Nothing -> assertFailure "expected typed fields"

    , testCase "missing shredded field surfaces as ShredMissing" $ do
        let oss = Shred.ObjectShreddingSchema
                    [ Shred.ShreddedField "event_type" Shred.ShredString
                    , Shred.ShreddedField "event_ts"   Shred.ShredInt64
                    ]
            v = IV.VObject (Map.singleton "event_type" (IV.VString "click"))
            row = Shred.routeObjectRow oss (Just v)
        case Shred.osrTypedFields row of
          Just [(_, t1), (_, t2)] -> do
            isShredAsTyped t1 @?= True   -- event_type present
            t2 @?= Shred.ShredVariantNull  -- event_ts missing -> Nothing -> spec says VariantNull
          other -> assertFailure ("unexpected typed fields: " ++ show other)

    , testCase "non-object input: typed_value null, value carries the variant" $ do
        let oss = Shred.ObjectShreddingSchema
                    [ Shred.ShreddedField "x" Shred.ShredInt32 ]
            row = Shred.routeObjectRow oss (Just (IV.VString "scalar"))
        Shred.osrTypedFields row @?= Nothing
        case Shred.osrValue row of
          Just bs -> assertBool "non-empty value bytes" (BS.length bs > 0)
          Nothing -> assertFailure "expected value bytes"

    , testCase "round-trip: fully-shredded object" $ do
        let meta = BS.pack [0x11, 0x00, 0x00]
            oss = Shred.ObjectShreddingSchema
                    [ Shred.ShreddedField "event_type" Shred.ShredString ]
            v = IV.VObject (Map.singleton "event_type" (IV.VString "noop"))
            row = Shred.routeObjectRow oss (Just v)
        case Shred.reconstructObjectVariant meta row of
          Right (Just v') -> v' @?= v
          other -> assertFailure ("round-trip mismatch: " ++ show other)

    , testCase "round-trip: variant null" $ do
        let meta = BS.pack [0x11, 0x00, 0x00]
            oss = Shred.ObjectShreddingSchema [ Shred.ShreddedField "x" Shred.ShredInt32 ]
            row = Shred.routeObjectRow oss (Just IV.VNull)
        case Shred.reconstructObjectVariant meta row of
          Right (Just IV.VNull) -> pure ()
          other -> assertFailure ("expected VNull, got " ++ show other)

    , testCase "round-trip: missing row" $ do
        let meta = BS.pack [0x11, 0x00, 0x00]
            oss = Shred.ObjectShreddingSchema [ Shred.ShreddedField "x" Shred.ShredInt32 ]
            row = Shred.routeObjectRow oss Nothing
        case Shred.reconstructObjectVariant meta row of
          Right Nothing -> pure ()
          other -> assertFailure ("expected Nothing, got " ++ show other)

    , testCase "round-trip: partially-shredded object (extras recovered)" $ do
        -- Object with two shredded fields + one non-shredded field;
        -- routeObjectRow puts the two typed values in 'typed_value'
        -- and the single non-shredded field in 'value'. The spec
        -- says the reader reconstructs the union, so the recovered
        -- variant must equal the input.
        let oss = Shred.ObjectShreddingSchema
                    [ Shred.ShreddedField "event_type" Shred.ShredString
                    , Shred.ShreddedField "event_ts"   Shred.ShredInt64
                    ]
            v = IV.VObject (Map.fromList
                  [ ("event_type", IV.VString "login")
                  , ("event_ts",   IV.VInt64 1729794146402)
                  , ("email",      IV.VString "user@example.com")
                  ])
            -- 'encodeVariant' produces the authoritative metadata for
            -- this Variant; using it means the reader's lookups for
            -- 'email' (the non-shredded field) resolve correctly.
            (meta, _) = IV.encodeVariant v
            row = Shred.routeObjectRow oss (Just v)
        case Shred.reconstructObjectVariant meta row of
          Right (Just v') -> v' @?= v
          other -> assertFailure
            ("partial-shred round-trip mismatch: " ++ show other)
    ]

  , testGroup "array shredding"
    [ testCase "array of strings: all elements typed" $ do
        let v = IV.VArray (V.fromList
                  [ IV.VString "comedy", IV.VString "drama" ])
            row = Shred.routeArrayRow Shred.ShredString (Just v)
        Shred.asrValue row @?= Nothing
        case Shred.asrTypedElements row of
          Just elems -> do
            length elems @?= 2
            map isShredAsTyped elems @?= [True, True]
          Nothing -> assertFailure "expected typed elements"

    , testCase "array of mixed types: typed for matches, value for others" $ do
        let v = IV.VArray (V.fromList
                  [ IV.VString "horror", IV.VNull ])
            row = Shred.routeArrayRow Shred.ShredString (Just v)
        case Shred.asrTypedElements row of
          Just [e0, e1] -> do
            isShredAsTyped e0 @?= True
            -- VNull -> ShredVariantNull
            e1 @?= Shred.ShredVariantNull
          other -> assertFailure ("unexpected: " ++ show other)

    , testCase "non-array input: fall through to value" $ do
        let row = Shred.routeArrayRow Shred.ShredString
                    (Just (IV.VString "scalar"))
        Shred.asrTypedElements row @?= Nothing
        case Shred.asrValue row of
          Just bs -> assertBool "value bytes" (BS.length bs > 0)
          Nothing -> assertFailure "expected value bytes"

    , testCase "round-trip: array of strings" $ do
        let meta = BS.pack [0x11, 0x00, 0x00]
            v = IV.VArray (V.fromList
                  [ IV.VString "alpha", IV.VString "beta" ])
            row = Shred.routeArrayRow Shred.ShredString (Just v)
        case Shred.reconstructArrayVariant meta row of
          Right (Just v') -> v' @?= v
          other -> assertFailure ("round-trip mismatch: " ++ show other)

    , testCase "round-trip: array with null elements" $ do
        let meta = BS.pack [0x11, 0x00, 0x00]
            v = IV.VArray (V.fromList
                  [ IV.VString "a", IV.VNull, IV.VString "c" ])
            row = Shred.routeArrayRow Shred.ShredString (Just v)
        case Shred.reconstructArrayVariant meta row of
          Right (Just v') -> v' @?= v
          other -> assertFailure ("round-trip mismatch: " ++ show other)

    , testCase "round-trip: variant null" $ do
        let meta = BS.pack [0x11, 0x00, 0x00]
            row = Shred.routeArrayRow Shred.ShredString (Just IV.VNull)
        case Shred.reconstructArrayVariant meta row of
          Right (Just IV.VNull) -> pure ()
          other -> assertFailure ("expected VNull, got " ++ show other)
    ]

  , testCase "pyarrow can read shredded file as 3 columns" $ do
      pyOk <- pyarrowAvailable
      if not pyOk
        then pure ()
        else do
          let meta = BS.pack [0x01, 0x00]
              rows = V.fromList
                [ Just (IV.VInt64 100)
                , Just (IV.VString "n/a")
                , Nothing
                , Just (IV.VInt64 200)
                ]
          case Shred.buildShreddedVariantParquetFile "m" meta
                 Shred.ShredInt64 rows of
            Left e -> assertFailure e
            Right bytes -> do
              let path = "/tmp/wireform-shredded-variant.parquet"
              BS.writeFile path bytes
              pyarrowAssert "shredded Variant file exposes the 3 columns"
                [ "t = pq.read_table('" ++ path ++ "').to_pylist()"
                , "assert len(t) == 4, f'wrong row count: {len(t)}'"
                , "names = list(t[0].keys())"
                , "assert 'm.metadata'    in names, f'missing metadata: {names!r}'"
                , "assert 'm.value'       in names, f'missing value: {names!r}'"
                , "assert 'm.typed_value' in names, f'missing typed_value: {names!r}'"
                -- Row 0: matched -> typed_value=100, value=null
                , "r0 = t[0]"
                , "assert r0['m.typed_value'] == 100, f'r0 typed: {r0!r}'"
                , "assert r0['m.value']       is None, f'r0 value: {r0!r}'"
                -- Row 1: fallthrough -> typed_value=null, value=non-null
                , "r1 = t[1]"
                , "assert r1['m.typed_value'] is None, f'r1 typed: {r1!r}'"
                , "assert r1['m.value']       is not None, f'r1 value: {r1!r}'"
                -- Row 2: variant-null -> typed_value=null, value=0x00
                , "r2 = t[2]"
                , "assert r2['m.typed_value'] is None, f'r2 typed: {r2!r}'"
                , "assert r2['m.value']       == bytes([0x00]), f'r2 value: {r2!r}'"
                ]
  ]
  where
    isTyped (Shred.ShredAsTyped _) = True
    isTyped _                      = False
    isShredAsTyped (Shred.ShredAsTyped _) = True
    isShredAsTyped _                      = False

pyarrowAvailable :: IO Bool
pyarrowAvailable = do
  (code, _, _) <- Proc.readProcessWithExitCode
                    "python3" ["-c", "import pyarrow.parquet"] ""
  pure (code == ExitSuccess)

pyarrowAssert :: String -> [String] -> IO ()
pyarrowAssert label snippet = do
  (code, out, err) <- Proc.readProcessWithExitCode "python3"
    [ "-c"
    , unlines
        ( "import pyarrow.parquet as pq"
        : snippet
       ++ ["print('PYARROW_OK')"]
        )
    ] ""
  case code of
    ExitSuccess
      | "PYARROW_OK" `isInfixOf` out -> pure ()
      | otherwise -> assertFailure (label ++ ": pyarrow output: " ++ out)
    _ -> assertFailure (label ++ ":\nstdout=" ++ out ++ "\nstderr=" ++ err)
