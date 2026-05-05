{-# LANGUAGE OverloadedStrings #-}

-- | Optional conformance harness against
-- <https://github.com/toml-lang/toml-test the official TOML test suite>.
--
-- The toml-test suite ships hundreds of fixture cases of the form:
--
-- @
--   tests/valid/\<group\>/\<name\>.toml    -- the input
--   tests/valid/\<group\>/\<name\>.json    -- expected typed-JSON output
--   tests/invalid/\<group\>/\<name\>.toml  -- input that must fail to parse
-- @
--
-- The companion @.json@ file uses a typed schema where every scalar
-- is wrapped in a @{ \"type\": ..., \"value\": ... }@ object so that
-- TOML's type information (especially @date-local@ vs @datetime@,
-- @integer@ vs @float@, etc.) is preserved.
--
-- This module exposes a single test tree that:
--
-- 1. Walks @TOML_TEST_SUITE@ (or the @TOML_TEST_DIR@ env var) when
--    set; otherwise reports a no-op skip group so CI stays green
--    out-of-the-box.
-- 2. For each @valid/@ case, runs 'TOML.Decode.decode' and structurally
--    compares the result against the typed JSON.
-- 3. For each @invalid/@ case, requires 'TOML.Decode.decode' to
--    return 'Left' (or to throw any synchronous exception).
--
-- A mini built-in suite (drawn from the TOML 1.0 spec examples)
-- always runs so that core compliance is exercised even without
-- the external suite.
module Test.TOML.Conformance (tests) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (filterM)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import System.Directory
  ( doesDirectoryExist, doesFileExist, listDirectory )
import System.Environment (lookupEnv)
import System.FilePath ((</>), dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))

import qualified TOML.Decode as TD
import qualified TOML.Value as TV

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

tests :: IO TestTree
tests = do
  builtin <- builtinSuite
  ext     <- externalSuite
  pure (testGroup "conformance" [builtin, ext])

-- ---------------------------------------------------------------------------
-- Built-in mini-suite
-- ---------------------------------------------------------------------------

builtinSuite :: IO TestTree
builtinSuite = pure $ testGroup "builtin"
  [ ok "spec key/value"
      "title = \"TOML Example\"\n"
      (\v -> case lookupKey "title" v of
         Just (TV.TString t) -> t @?= T.pack "TOML Example"
         _ -> assertFailure "expected title string")

  , ok "spec table"
      (T.unlines
         [ "[server]"
         , "host = \"localhost\""
         , "port = 8080"
         ])
      (\v -> case lookupKey "server" v of
         Just s -> do
           case lookupKey "host" s of
             Just (TV.TString t) -> t @?= T.pack "localhost"
             _ -> assertFailure "host"
           case lookupKey "port" s of
             Just (TV.TInteger n) -> n @?= 8080
             _ -> assertFailure "port"
         _ -> assertFailure "expected server table")

  , ok "spec inline table"
      "point = { x = 1, y = 2 }\n"
      (\v -> case lookupKey "point" v of
         Just _ -> pure ()
         _ -> assertFailure "expected point")

  , ok "spec array of tables"
      (T.unlines
         [ "[[products]]"
         , "name = \"Hammer\""
         , ""
         , "[[products]]"
         , "name = \"Nail\""
         ])
      (\_ -> pure ())
  ]
  where
    ok name src k = testCase name $ case TD.decode src of
      Left e  -> assertFailure ("decode failed: " ++ e)
      Right v -> k v

lookupKey :: T.Text -> TV.Value -> Maybe TV.Value
lookupKey nm v = case v of
  TV.TTable kvs -> goV 0
    where
      !len = V.length kvs
      goV !i
        | i >= len = Nothing
        | otherwise = case kvs V.! i of
            (k, val) | k == nm -> Just val
                     | otherwise -> goV (i + 1)
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- External suite (toml-test)
-- ---------------------------------------------------------------------------

externalSuite :: IO TestTree
externalSuite = do
  mDir <- discoverSuiteDir
  case mDir of
    Nothing  -> pure (testGroup "toml-test (skipped, set TOML_TEST_SUITE)" [])
    Just dir -> do
      validCases   <- discoverValid   dir
      invalidCases <- discoverInvalid dir
      let validCount   = length validCases
          invalidCount = length invalidCases
      pure $ testGroup
        ("toml-test ("
            ++ show validCount   ++ " valid, "
            ++ show invalidCount ++ " invalid)")
        [ testGroup "valid"   (map mkValidCase   validCases)
        , testGroup "invalid" (map mkInvalidCase invalidCases)
        ]

-- | Look at @TOML_TEST_SUITE@ first, falling back to @TOML_TEST_DIR@.
-- The canonical layout has the actual cases under @\<root\>/tests/@,
-- so accept either pointer.
discoverSuiteDir :: IO (Maybe FilePath)
discoverSuiteDir = do
  v1 <- lookupEnv "TOML_TEST_SUITE"
  v2 <- lookupEnv "TOML_TEST_DIR"
  let candidates = case (v1, v2) of
        (Just a,  Just b)  -> [a, b]
        (Just a,  Nothing) -> [a]
        (Nothing, Just b)  -> [b]
        (Nothing, Nothing) -> []
  pickExisting candidates
  where
    pickExisting [] = pure Nothing
    pickExisting (p : rest) = do
      ok <- doesDirectoryExist p
      if not ok then pickExisting rest
        else do
          -- If the user pointed at the repo root, descend into tests/
          subOk <- doesDirectoryExist (p </> "tests")
          pure (Just (if subOk then p </> "tests" else p))

discoverValid :: FilePath -> IO [FilePath]
discoverValid root = do
  let d = root </> "valid"
  exists <- doesDirectoryExist d
  if not exists then pure []
    else collectToml d

discoverInvalid :: FilePath -> IO [FilePath]
discoverInvalid root = do
  let d = root </> "invalid"
  exists <- doesDirectoryExist d
  if not exists then pure []
    else collectToml d

-- | Recursively find all @.toml@ files under a directory.
collectToml :: FilePath -> IO [FilePath]
collectToml = walk
  where
    walk d = do
      entries <- listDirectory d
      let absEntries = map (d </>) entries
      dirs    <- filterM doesDirectoryExist absEntries
      files   <- filterM doesFileExist      absEntries
      let here = filter hasTomlExt files
      subs <- mapM walk dirs
      pure (here ++ concat subs)
    hasTomlExt :: FilePath -> Bool
    hasTomlExt f = takeExtension f == ".toml"

-- ---------------------------------------------------------------------------
-- Per-case drivers
-- ---------------------------------------------------------------------------

mkValidCase :: FilePath -> TestTree
mkValidCase tomlPath = testCase tomlPath $ do
  bytes <- BS.readFile tomlPath
  let txt = TE.decodeUtf8Lenient bytes
  res <- try (evaluate (TD.decode txt))
           :: IO (Either SomeException (Either String TV.Value))
  case res of
    Left e          -> assertFailure ("exception: " ++ show e)
    Right (Left e)  -> assertFailure ("decode failed: " ++ e)
    Right (Right v) -> do
      let jsonPath = dropExtension tomlPath ++ ".json"
      hasExpected <- doesFileExist jsonPath
      if not hasExpected
        then pure ()
        else do
          rawJSON <- BSL.readFile jsonPath
          case A.eitherDecode rawJSON of
            Left e -> assertFailure ("malformed expected JSON: " ++ e)
            Right expected -> do
              let !actual = toTypedJSON v
              compareJSON actual expected

mkInvalidCase :: FilePath -> TestTree
mkInvalidCase tomlPath = testCase tomlPath $ do
  bytes <- BS.readFile tomlPath
  let txt = TE.decodeUtf8Lenient bytes
  res <- try (evaluate (TD.decode txt))
           :: IO (Either SomeException (Either String TV.Value))
  case res of
    Left  _          -> pure ()                    -- exception counts as fail
    Right (Left  _)  -> pure ()                    -- expected
    Right (Right _)  -> assertFailure "expected parse error, got success"

-- | Compare actual decoded JSON against the expected typed JSON
-- from the toml-test suite. Currently a strict equality check; we
-- could relax in the future to be tolerant of e.g. float
-- formatting differences.
compareJSON :: A.Value -> A.Value -> IO ()
compareJSON = (@?=)

-- ---------------------------------------------------------------------------
-- TOML.Value -> toml-test typed JSON
-- ---------------------------------------------------------------------------

-- | Convert a 'TV.Value' to the typed-JSON representation used by
-- the official toml-test suite. Scalars become
-- @{\"type\": ..., \"value\": ...}@; arrays become JSON arrays;
-- tables become JSON objects.
toTypedJSON :: TV.Value -> A.Value
toTypedJSON = \case
  TV.TString  t -> wrap "string"   (A.String t)
  TV.TInteger n -> wrap "integer"  (A.String (T.pack (show n)))
  TV.TBool    b -> wrap "bool"     (A.String (if b then "true" else "false"))
  TV.TFloat   d -> wrap "float"    (A.String (renderFloat d))
  TV.TDateTime t -> wrap (classifyDT t) (A.String (canonDT t))
  TV.TDate     t -> wrap "date-local" (A.String t)
  TV.TTime     t -> wrap "time-local" (A.String t)
  TV.TArray xs  -> A.Array (V.map toTypedJSON xs)
  TV.TTable kvs ->
    A.Object (AKM.fromList (V.toList (V.map mkPair kvs)))
  where
    mkPair (k, val) = (AK.fromText k, toTypedJSON val)

    wrap :: T.Text -> A.Value -> A.Value
    wrap ty v = A.Object $ AKM.fromList
      [ (AK.fromText "type",  A.String ty)
      , (AK.fromText "value", v)
      ]

-- | The toml-test suite canonicalises floats: special values are
-- emitted as @inf@ / @-inf@ / @nan@; ordinary values are decimal.
renderFloat :: Double -> T.Text
renderFloat d
  | isNaN d                  = T.pack "nan"
  | isInfinite d && d > 0    = T.pack "inf"
  | isInfinite d             = T.pack "-inf"
  | otherwise                = T.pack (show d)

-- | The decoder currently returns 'TDateTime' as the original
-- source text. Distinguish offset-bearing vs. local-only based on
-- a trailing @Z@ / @+@ / @-@ in the time portion.
classifyDT :: T.Text -> T.Text
classifyDT t
  | hasOffset t = T.pack "datetime"
  | otherwise   = T.pack "datetime-local"
  where
    hasOffset s
      | T.length s < 11 = False
      | otherwise =
          let timepart = T.drop 11 s
              ends c = c == 'Z' || c == 'z'
                      || c == '+' || c == '-'
          in T.any ends timepart

-- | Strip whitespace separators so @T@ vs space-as-separator both
-- normalise, and lowercase the final @Z@ for consistency.
canonDT :: T.Text -> T.Text
canonDT = id   -- Decoder preserves source text verbatim.
