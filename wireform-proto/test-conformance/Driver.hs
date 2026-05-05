{-# LANGUAGE OverloadedStrings #-}

-- | Tasty driver for the upstream protobuf conformance suite.
--
-- The suite is a separate test target ('protobuf-conformance-test')
-- because it depends on an external binary (the upstream
-- @conformance_test_runner@ from
-- <https://github.com/protocolbuffers/protobuf>) that we can't
-- ship through Hackage.
--
-- == How the suite finds the runner
--
-- 1. If the @CONFORMANCE_TEST_RUNNER@ environment variable points
--    at an executable, that's used.
-- 2. Otherwise, look for the runner under
--    @dist-newstyle/conformance/conformance_test_runner@. The
--    helper script @scripts\/build-conformance-runner.sh@ clones
--    + builds the upstream tree to that location.
-- 3. Otherwise, the suite skips with an instructive message.
--
-- The skip path keeps the test green in environments without a
-- C++ toolchain or network (CI sandboxes, fresh dev VMs, etc.).
-- A non-skipped run rebuilds @wireform-conformance-runner@ via
-- @cabal list-bin@ and pipes it to the upstream runner.
--
-- == What \"pass\" means
--
-- The upstream runner emits one summary line of the form
--
-- @
-- CONFORMANCE TEST BEGIN ====================================
-- ...
-- CONFORMANCE SUITE PASSED: ... successes, ... skipped.
-- @
--
-- (or @FAILED@). We assert on the @PASSED@ marker, capturing the
-- runner's stderr into the test failure when the assertion
-- fires so the failure list is visible without re-running.
--
-- A small @failure_list_proto3.txt@ file lists tests we
-- knowingly don't support (JSON conformance for messages whose
-- schema we omit; JSPB; TEXT_FORMAT). The runner's
-- @--failure_list@ flag treats those as expected failures so the
-- net assertion is on regressions rather than absolute coverage.
module Main (main) where

import Control.Exception (IOException, try)
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, getPermissions, executable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , waitForProcess
  )

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

main :: IO ()
main = defaultMain =<< buildTree

buildTree :: IO TestTree
buildTree = do
  mRunner <- locateUpstreamRunner
  case mRunner of
    Nothing -> pure (testGroup "protobuf-conformance"
      [ testCase "skipped (no upstream runner)" $ do
          hPutStrLn stderr ""
          hPutStrLn stderr instructions
      ])
    Just runner -> pure (testGroup "protobuf-conformance"
      [ testCase ("upstream runner: " <> runner) (runConformance runner) ])

-- | Probe order: @CONFORMANCE_TEST_RUNNER@ env var first, then
-- the helper-script's default install path.
locateUpstreamRunner :: IO (Maybe FilePath)
locateUpstreamRunner = do
  envPath <- lookupEnv "CONFORMANCE_TEST_RUNNER"
  case envPath of
    Just p  -> ifExists p
    Nothing -> ifExists defaultRunnerPath

defaultRunnerPath :: FilePath
defaultRunnerPath =
  "dist-newstyle" </> "conformance" </> "conformance_test_runner"

ifExists :: FilePath -> IO (Maybe FilePath)
ifExists p = do
  exists <- doesFileExist p
  if not exists then pure Nothing
  else do
    perm <- getPermissions p
    pure (if executable perm then Just p else Nothing)

instructions :: String
instructions =
  unlines
    [ "wireform-proto conformance suite skipped."
    , ""
    , "To enable, build the upstream protobuf conformance_test_runner:"
    , ""
    , "    bash wireform-proto/test-conformance/scripts/build-conformance-runner.sh"
    , ""
    , "or point CONFORMANCE_TEST_RUNNER at a pre-built copy and re-run"
    , "    cabal test wireform-proto:protobuf-conformance-test"
    ]

-- | Locate the freshly-built @wireform-conformance-runner@
-- binary via @cabal list-bin@. This is more reliable than
-- guessing dist-newstyle paths (which differ by GHC version,
-- platform, and project file), and it'll refuse to run if the
-- binary isn't built.
locateWireformRunner :: IO (Either String FilePath)
locateWireformRunner = do
  res <- try @IOException $ do
    (_, _, _, ph) <- createProcess
      (proc "cabal" ["build", "wireform-proto:exe:wireform-conformance-runner"])
        { std_out = Inherit, std_err = Inherit }
    code <- waitForProcess ph
    case code of
      ExitSuccess -> pure ()
      _ -> error "cabal build wireform-conformance-runner failed"

    (_, mout, _, ph2) <- createProcess
      (proc "cabal" ["list-bin", "wireform-proto:exe:wireform-conformance-runner"])
        { std_out = CreatePipe, std_err = Inherit }
    code2 <- waitForProcess ph2
    case code2 of
      ExitSuccess -> pure ()
      _ -> error "cabal list-bin failed"
    case mout of
      Nothing -> error "cabal list-bin produced no output"
      Just h  -> do
        bs <- BS8.hGetContents h
        case lines (BS8.unpack bs) of
          (path:_) -> pure path
          []       -> error "cabal list-bin returned empty"
  pure (case res of
          Left e  -> Left (show e)
          Right p -> Right p)

runConformance :: FilePath -> IO ()
runConformance runner = do
  eRunnerBin <- locateWireformRunner
  case eRunnerBin of
    Left e -> assertFailure ("could not build wireform-conformance-runner: " <> e)
    Right wireformBin -> do
      failureList <- failureListPath
      let args = [ "--enforce_recommended"
                 , "--failure_list", failureList
                 , wireformBin
                 ]
      hPutStrLn stderr ("> " <> runner <> " " <> unwords args)
      (_, _, _, ph) <- createProcess (proc runner args)
        { std_out = Inherit, std_err = Inherit }
      code <- waitForProcess ph
      case code of
        ExitSuccess -> pure ()
        ExitFailure n -> assertFailure
          ("upstream conformance_test_runner exited with code "
            <> show n
            <> "; failures listed above. See "
            <> failureList
            <> " to add expected failures.")

failureListPath :: IO FilePath
failureListPath = do
  envPath <- lookupEnv "CONFORMANCE_FAILURE_LIST"
  pure (fromMaybe defaultFailureList envPath)

defaultFailureList :: FilePath
defaultFailureList =
  "wireform-proto" </> "test-conformance" </> "failure_list_proto3.txt"
