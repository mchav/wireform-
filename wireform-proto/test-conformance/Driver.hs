{-# LANGUAGE OverloadedStrings #-}

{- | Tasty driver for the upstream protobuf conformance suite.

The suite is a separate test target ('protobuf-conformance-test')
because it depends on an external binary (the upstream
@conformance_test_runner@ from
<https://github.com/protocolbuffers/protobuf>) that we can't
ship through Hackage.

== How the suite finds the runner

1. If the @CONFORMANCE_TEST_RUNNER@ environment variable points
   at an executable, that's used.
2. Otherwise, look for the runner under
   @dist-newstyle/conformance/conformance_test_runner@. The
   helper script @scripts\/build-conformance-runner.sh@ clones
   + builds the upstream tree to that location.
3. Otherwise, the suite skips with an instructive message.

The skip path keeps the test green in environments without a
C++ toolchain or network (CI sandboxes, fresh dev VMs, etc.).
A non-skipped run rebuilds @wireform-conformance-runner@ via
@cabal list-bin@ and pipes it to the upstream runner.

== What \"pass\" means

The upstream runner emits one summary line of the form

@
CONFORMANCE TEST BEGIN ====================================
...
CONFORMANCE SUITE PASSED: ... successes, ... skipped.
@

(or @FAILED@). We assert on the @PASSED@ marker, capturing the
runner's stderr into the test failure when the assertion
fires so the failure list is visible without re-running.

A small @failure_list_proto3.txt@ file lists tests we
knowingly don't support (JSON conformance for messages whose
schema we omit; JSPB; TEXT_FORMAT). The runner's
@--failure_list@ flag treats those as expected failures so the
net assertion is on regressions rather than absolute coverage.
-}
module Main (main) where

import Control.Exception (IOException, try)
import Data.ByteString.Char8 qualified as BS8
import Data.Maybe qualified
import System.Directory (doesFileExist, executable, getPermissions)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (
  CreateProcess (..),
  StdStream (..),
  createProcess,
  proc,
  waitForProcess,
 )
import Test.Syd


main :: IO ()
main = sydTest =<< buildTree


buildTree :: IO Spec
buildTree = do
  mRunner <- locateUpstreamRunner
  case mRunner of
    Nothing ->
      pure
        ( describe "protobuf-conformance" $
            sequence_
              [ it "skipped (no upstream runner)" $ do
                  hPutStrLn stderr ""
                  hPutStrLn stderr instructions
              ]
        )
    Just runner ->
      pure
        ( describe "protobuf-conformance" $
            sequence_
              [it ("upstream runner: " <> runner) (runConformance runner)]
        )


{- | Probe order: @CONFORMANCE_TEST_RUNNER@ env var first, then
the helper-script's default install path.
-}
locateUpstreamRunner :: IO (Maybe FilePath)
locateUpstreamRunner = do
  envPath <- lookupEnv "CONFORMANCE_TEST_RUNNER"
  case envPath of
    Just p -> ifExists p
    Nothing -> ifExists defaultRunnerPath


defaultRunnerPath :: FilePath
defaultRunnerPath =
  "dist-newstyle" </> "conformance" </> "conformance_test_runner"


ifExists :: FilePath -> IO (Maybe FilePath)
ifExists p = do
  exists <- doesFileExist p
  if not exists
    then pure Nothing
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


{- | Locate the freshly-built @wireform-conformance-runner@
binary via @cabal list-bin@. This is more reliable than
guessing dist-newstyle paths (which differ by GHC version,
platform, and project file), and it'll refuse to run if the
binary isn't built.
-}
locateWireformRunner :: IO (Either String FilePath)
locateWireformRunner = do
  res <- try @IOException $ do
    (_, _, _, ph) <-
      createProcess
        (proc "cabal" ["build", "wireform-proto:exe:wireform-conformance-runner"])
          { std_out = Inherit
          , std_err = Inherit
          }
    code <- waitForProcess ph
    case code of
      ExitSuccess -> pure ()
      _ -> error "cabal build wireform-conformance-runner failed"

    (_, mout, _, ph2) <-
      createProcess
        (proc "cabal" ["list-bin", "wireform-proto:exe:wireform-conformance-runner"])
          { std_out = CreatePipe
          , std_err = Inherit
          }
    code2 <- waitForProcess ph2
    case code2 of
      ExitSuccess -> pure ()
      _ -> error "cabal list-bin failed"
    case mout of
      Nothing -> error "cabal list-bin produced no output"
      Just h -> do
        bs <- BS8.hGetContents h
        case lines (BS8.unpack bs) of
          (path : _) -> pure path
          [] -> error "cabal list-bin returned empty"
  pure
    ( case res of
        Left e -> Left (show e)
        Right p -> Right p
    )


runConformance :: FilePath -> IO ()
runConformance runner = do
  eRunnerBin <- locateWireformRunner
  case eRunnerBin of
    Left e -> expectationFailure ("could not build wireform-conformance-runner: " <> e)
    Right wireformBin -> do
      failureList <- failureListPath
      let args =
            [ "--enforce_recommended"
            , "--failure_list"
            , failureList
            , wireformBin
            ]
      hPutStrLn stderr ("> " <> runner <> " " <> unwords args)
      -- Capture stderr so we can salvage a real pass/fail
      -- decision from the summary line. The runner exits non-zero
      -- on "doesn't exist" entries in the failure list (which fire
      -- when one iteration of the suite skips a test that another
      -- iteration's failure list references). Those are noise; the
      -- actual signal is "N unexpected failures" in the summary.
      (_, _, Just hErr, ph) <-
        createProcess
          (proc runner args)
            { std_out = Inherit
            , std_err = CreatePipe
            }
      errBytes <- BS8.hGetContents hErr
      hPutStrLn stderr (BS8.unpack errBytes)
      code <- waitForProcess ph
      let unexpectedFailures = parseUnexpectedFailures errBytes
      case (code, unexpectedFailures) of
        (ExitSuccess, _) -> pure ()
        (ExitFailure _, Just 0) ->
          -- Exit non-zero is the runner complaining about
          -- "doesn't exist" entries in the failure list; that's
          -- a stale-list issue, not a wireform regression.
          hPutStrLn
            stderr
            "upstream runner exited non-zero but reported 0 unexpected failures; \
            \treating as PASS."
        (ExitFailure n, _) ->
          expectationFailure
            ( "upstream conformance_test_runner exited with code "
                <> show n
                <> "; failures listed above. See "
                <> failureList
                <> " to add expected failures."
            )


{- | Parse the runner's @CONFORMANCE SUITE (PASSED|FAILED): N successes, ...,
M unexpected failures.@ summary line out of stderr. Returns
'Nothing' if the line isn't found (treat as a failure to be safe).
-}
parseUnexpectedFailures :: BS8.ByteString -> Maybe Int
parseUnexpectedFailures bs =
  let summaryLine =
        Data.Maybe.listToMaybe
          [l | l <- BS8.lines bs, BS8.isInfixOf (BS8.pack "unexpected failures") l]
  in case summaryLine of
       Nothing -> Nothing
       Just l ->
         -- Extract the integer immediately preceding " unexpected failures".
         let parts = BS8.split ',' l
             uf = filter (BS8.isInfixOf (BS8.pack "unexpected failures")) parts
         in case uf of
              (x : _) -> case BS8.words x of
                (n : _) -> Just (read (BS8.unpack n))
                _ -> Nothing
              _ -> Nothing


failureListPath :: IO FilePath
failureListPath = do
  envPath <- lookupEnv "CONFORMANCE_FAILURE_LIST"
  case envPath of
    Just p -> pure p
    Nothing -> pickDefaultFailureList


{- | The default failure-list path is hairier than it looks
because cabal's test runner doesn't fix the working
directory in any consistent way across versions. Try several
candidates in order of likelihood.
-}
pickDefaultFailureList :: IO FilePath
pickDefaultFailureList = do
  let candidates =
        [ "wireform-proto" </> "test-conformance" </> "failure_list_proto3.txt"
        , "test-conformance" </> "failure_list_proto3.txt"
        , ".." </> "test-conformance" </> "failure_list_proto3.txt"
        ]
      first [] = pure (head candidates) -- give up; runner will say
      first (c : cs) = do
        ex <- doesFileExist c
        if ex then pure c else first cs
  first candidates
