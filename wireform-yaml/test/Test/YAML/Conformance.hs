{-# LANGUAGE OverloadedStrings #-}
-- | Optional spec-conformance harness.
--
-- The official YAML test suite (https://github.com/yaml/yaml-test-suite)
-- ships ~400 test cases of the form
--
-- @
--   <case-id>/in.yaml         -- raw YAML input
--   <case-id>/error           -- present iff the input must fail
--   <case-id>/in.json         -- equivalent JSON (where applicable)
--   <case-id>/===             -- one-line label
-- @
--
-- We run every test we can find by reading the @YAML_TEST_SUITE@
-- environment variable and walking the directory. When the variable
-- is unset, the suite is skipped and a stub success test is emitted
-- so CI stays green out-of-the-box.
--
-- A built-in mini-suite (the cases in @wireform-yaml/test-data/yaml@)
-- always runs so that core compliance is exercised even without the
-- external suite.
module Test.YAML.Conformance (tests) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory
  (doesFileExist, doesDirectoryExist, listDirectory, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))

import qualified YAML.Decode as YD
import qualified YAML.Value as YV

tests :: IO TestTree
tests = do
  builtin <- builtinSuite
  ext     <- externalSuite
  pure (testGroup "conformance" [builtin, ext])

-- ---------------------------------------------------------------------------
-- Built-in mini-suite
-- ---------------------------------------------------------------------------

-- | A handful of cases distilled from the YAML 1.2 spec that we
-- never want to regress on.
builtinSuite :: IO TestTree
builtinSuite = pure $ testGroup "builtin"
  [ caseOK "spec ex 2.1 seq of strings"
      (T.unlines [ "- Mark McGwire"
                 , "- Sammy Sosa"
                 , "- Ken Griffey"
                 ])
      (\v -> case v of
         YV.YSeq xs -> V.length xs @?= 3
         _ -> assertFailure "expected sequence")

  , caseOK "spec ex 2.2 mapping of scalars"
      (T.unlines [ "hr:  65"
                 , "avg: 0.278"
                 , "rbi: 147"
                 ])
      (\v -> do
         YV.lookupKey "hr"  v @?= Just (YV.YInt 65)
         case YV.lookupKey "rbi" v of
           Just (YV.YInt 147) -> pure ()
           r -> assertFailure (show r))

  , caseOK "spec ex 7.1 alias nodes"
      (T.unlines [ "First occurrence: &anchor Foo"
                 , "Second occurrence: *anchor"
                 ])
      (\v -> do
         YV.lookupKey "First occurrence" v @?= Just (YV.YString "Foo")
         YV.lookupKey "Second occurrence" v @?= Just (YV.YString "Foo"))

  , caseOK "spec ex 8.1 block scalar header"
      (T.unlines [ "literal: |"
                 , "  text"
                 , "folded: >"
                 , "  text"
                 ])
      (\v -> do
         YV.lookupKey "literal" v @?= Just (YV.YString "text\n")
         YV.lookupKey "folded"  v @?= Just (YV.YString "text\n"))

  , caseOK "spec ex 5.3 block sequence"
      (T.unlines [ "block sequence:"
                 , "  - one"
                 , "  - two : three"
                 ])
      (\v -> case YV.lookupKey "block sequence" v of
         Just (YV.YSeq xs) -> V.length xs @?= 2
         _ -> assertFailure "expected nested sequence")

  , caseOK "flow nested in block"
      (T.unlines [ "flow: { a: 1, b: 2, c: [3, 4] }"
                 ])
      (\v -> case YV.lookupKey "flow" v of
         Just (YV.YMap _) -> pure ()
         r -> assertFailure (show r))

  , caseOK "string preserves int-like value when quoted"
      "version: \"1.0\""
      (\v -> YV.lookupKey "version" v @?= Just (YV.YString "1.0"))
  ]

caseOK :: String -> T.Text -> (YV.Value -> IO ()) -> TestTree
caseOK name src k = testCase name $
  case YD.decode src of
    Left  e -> assertFailure $ "decode failed: " ++ e
    Right v -> k v

-- ---------------------------------------------------------------------------
-- External suite (yaml-test-suite)
-- ---------------------------------------------------------------------------

externalSuite :: IO TestTree
externalSuite = do
  mDir <- lookupEnv "YAML_TEST_SUITE"
  case mDir of
    Nothing  -> pure (testGroup "yaml-test-suite (skipped, set YAML_TEST_SUITE)" [])
    Just dir -> do
      exists <- doesDirectoryExist dir
      if not exists
        then pure (testGroup "yaml-test-suite (path missing)" [])
        else do
          cases <- discoverCases dir
          pure (testGroup ("yaml-test-suite (" ++ show (length cases) ++ " cases)")
                  (map (mkCase dir) cases))

-- | Each case-id is a path relative to the test-suite root.
discoverCases :: FilePath -> IO [FilePath]
discoverCases root = do
  -- A "case directory" is one that contains an @in.yaml@ or has
  -- numbered subdirectories that do.
  walk root
  where
    walk d = do
      entries <- listDirectory d
      let absEntries = map (d </>) entries
      dirs    <- filterM doesDirectoryExist absEntries
      hasIn   <- doesFileExist (d </> "in.yaml")
      let here = if hasIn then [d] else []
      subs <- mapM walk dirs
      pure (here ++ concat subs)

mkCase :: FilePath -> FilePath -> TestTree
mkCase _root caseDir = testCase caseDir $ do
  inPath  <- pure (caseDir </> "in.yaml")
  errPath <- pure (caseDir </> "error")
  isErr   <- doesFileExist errPath
  bytes   <- BS.readFile inPath
  let txt = TE.decodeUtf8Lenient bytes
  res <- try (pure $! YD.decodeStream txt) :: IO (Either SomeException (Either String YV.Stream))
  case (isErr, res) of
    (True,  Right (Left _))     -> pure ()              -- expected failure
    (True,  Right (Right _))    -> assertFailure "expected parse error, got success"
    (True,  Left  _)            -> pure ()              -- exception counts as fail
    (False, Right (Right _))    -> pure ()              -- expected success
    (False, Right (Left e))     -> assertFailure $ "decode failed: " ++ e
    (False, Left  e)            -> assertFailure $ "exception: " ++ show e
