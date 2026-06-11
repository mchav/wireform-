{- | External-tool conformance tests.

This test suite spins up @wireform-http1-echo-server@ once, then drives
two public HTTP\/1.1 conformance probes against it:

  * __h1spec__ — uNetworking\/h1spec, 33 RFC 9112-focused tests
    covering request-line and header parsing, request smuggling,
    chunked transfer-encoding, Expect: 100-continue, and fragmented
    socket reads. Drives the server via Deno + TCP.
  * __Http11Probe__ — MDA2AV\/Http11Probe, 215 RFC 9110+9112+
    security tests (161 scored), covering compliance, smuggling,
    malformed input, normalisation, cookies, and capabilities.
    Drives the server via .NET + TCP.

Each probe is /silently skipped/ when its driver (@deno@ \/ @dotnet@)
isn't on @PATH@, so a vanilla @cabal test@ still passes on machines
that don't have those toolchains installed.

To pin specific copies of the probes set the env vars:

  * @WIREFORM_HTTP1_H1SPEC=\/path\/to\/h1spec\/http_test.ts@
  * @WIREFORM_HTTP1_HTTP11PROBE=\/path\/to\/Http11Probe.Cli.dll@

If a probe's driver is missing /and/ its env variable is unset, the
test prints @SKIPPED@ instead of failing.
-}
module Main (main) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Exception (SomeException, bracket, try)
import Data.ByteString qualified as BS
import Network.HTTP1.Server
import Network.HTTP1.Status
import Network.HTTP1.Types
import Network.Socket qualified as NS
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.IO (Handle, hGetContents)
import System.Process
import Test.Syd


main :: IO ()
main = sydTest tests


tests :: Spec
tests =
  describe "wireform-http1 external conformance" $
    sequence_
      [ h1specTest
      , http11ProbeTest
      ]


------------------------------------------------------------------------
-- Echo server lifecycle
------------------------------------------------------------------------

data EchoCtx = EchoCtx {ecPort :: !Int, ecThread :: !ThreadId, ecSocket :: !NS.Socket}


startEchoServer :: IO EchoCtx
startEchoServer = do
  let hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> expectationFailure "could not resolve 127.0.0.1"
    (addr : _) -> do
      sock <- NS.openSocket addr
      NS.setSocketOption sock NS.ReuseAddr 1
      NS.bind sock (NS.addrAddress addr)
      NS.listen sock 1024
      bound <- NS.getSocketName sock
      let port = case bound of
            NS.SockAddrInet p _ -> fromIntegral p
            _ -> 0
      tid <- forkIO (acceptForever sock)
      pure EchoCtx {ecPort = port, ecThread = tid, ecSocket = sock}


stopEchoServer :: EchoCtx -> IO ()
stopEchoServer EchoCtx {ecThread = tid, ecSocket = sock} = do
  killThread tid
  _ <- try @SomeException (NS.close sock)
  pure ()


acceptForever :: NS.Socket -> IO ()
acceptForever listenSock = loop
  where
    loop = do
      (s, _) <- NS.accept listenSock
      _ <- forkIO $ runServerOnSocket cfg s
      loop
    cfg =
      defaultServerConfig
        { serverHandler = echoHandler
        , serverListenBacklog = 4096
        }


echoHandler :: Handler
echoHandler req = do
  body <- drainAll (requestBody req)
  pure
    Response
      { responseStatus = OK
      , responseVersion = HTTP_1_1
      , responseHeaders =
          [ ("Content-Type", "application/octet-stream")
          , ("Server", "wireform-http1")
          ]
      , responseBody = BodyBytes body
      , responseTrailers = pure []
      }


drainAll :: Body -> IO BS.ByteString
drainAll BodyEmpty = pure BS.empty
drainAll (BodyBytes bs) = pure bs
drainAll (BodyPreEncoded _) = pure BS.empty
drainAll (BodyStream producer) = go []
  where
    go acc = do
      mc <- producer
      case mc of
        Nothing -> pure (BS.concat (reverse acc))
        Just chunk -> go (chunk : acc)


------------------------------------------------------------------------
-- h1spec
------------------------------------------------------------------------

{- | Run h1spec under @deno@. Pass criterion: every test must be ✅,
i.e. the trailer line must say @33 out of 33 tests passed.@
-}
h1specTest :: Spec
h1specTest = it "h1spec (33 tests, RFC 9112)" $ bracket startEchoServer stopEchoServer $ \ctx -> do
  deno <- findExecutable "deno"
  script <- resolveH1specScript
  case (deno, script) of
    (Nothing, _) ->
      assertSkip "deno not on PATH"
    (_, Nothing) ->
      assertSkip "h1spec http_test.ts not found"
    (Just denoExe, Just path) -> do
      output <-
        runProc
          denoExe
          ["run", "--allow-net", path, "127.0.0.1", show (ecPort ctx)]
      case parseH1specSummary output of
        Nothing ->
          expectationFailure $ "could not parse h1spec summary.\nOutput:\n" <> output
        Just (passed, total) ->
          (passed, total) `shouldBe` (total, total)


parseH1specSummary :: String -> Maybe (Int, Int)
parseH1specSummary out =
  -- "33 out of 33 tests passed."
  case [words l | l <- lines out, " out of " `isInfixOf'` l] of
    [] -> Nothing
    (ws : _) -> case ws of
      (pStr : "out" : "of" : tStr : _) -> (,) <$> readMaybe pStr <*> readMaybe tStr
      _ -> Nothing


------------------------------------------------------------------------
-- Http11Probe
------------------------------------------------------------------------

{- | Baseline score we have to meet. We currently pass /every/ scored
test in the corpus (161/161); the unscored 54 are capabilities and
cookie-handling probes that don't have a single right answer.
Bumping the baseline as we improve the parser keeps us from
silently sliding backwards.
-}
http11ProbeBaseline :: (Int, Int)
http11ProbeBaseline = (161, 161)


http11ProbeTest :: Spec
http11ProbeTest = it "Http11Probe (215 tests, baseline >= 161/161)" $ bracket startEchoServer stopEchoServer $ \ctx -> do
  dotnet <- findExecutable "dotnet"
  dll <- resolveHttp11ProbeDll
  case (dotnet, dll) of
    (Nothing, _) ->
      assertSkip "dotnet not on PATH"
    (_, Nothing) ->
      assertSkip "Http11Probe.Cli.dll not found"
    (Just dnExe, Just path) -> do
      output <- runProc dnExe [path, "--host", "127.0.0.1", "--port", show (ecPort ctx)]
      case parseHttp11ProbeSummary output of
        Nothing ->
          expectationFailure $ "could not parse Http11Probe summary.\nOutput:\n" <> output
        Just (passed, total) ->
          ( if (passed >= fst http11ProbeBaseline && total >= snd http11ProbeBaseline)
              then pure ()
              else
                expectationFailure
                  ( concat
                      [ "Http11Probe regression: "
                      , show passed
                      , "/"
                      , show total
                      , " (baseline "
                      , show (fst http11ProbeBaseline)
                      , "/"
                      , show (snd http11ProbeBaseline)
                      , ")\n"
                      , "Output tail:\n"
                      , unlines (drop (max 0 (length (lines output) - 6)) (lines output))
                      ]
                  )
          )


parseHttp11ProbeSummary :: String -> Maybe (Int, Int)
parseHttp11ProbeSummary out =
  -- "  Score: 128/161 (33 failed, 20 warnings) ..."
  let digit c = c >= '0' && c <= '9'
  in case [ dropWhile (\c -> c == ' ' || c == '\t') (drop 6 trimmed)
          | l <- lines out
          , let trimmed = dropWhile (== ' ') l
          , take 6 trimmed == "Score:"
          ] of
       [] -> Nothing
       (rest : _) ->
         let p = takeWhile digit rest
             afterSlash = drop 1 (dropWhile (/= '/') rest)
             t = takeWhile digit afterSlash
         in (,) <$> readMaybe p <*> readMaybe t


------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

isInfixOf' :: String -> String -> Bool
isInfixOf' needle hay =
  let n = length needle
  in any (\i -> take n (drop i hay) == needle) [0 .. length hay - n]


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of [(x, "")] -> Just x; _ -> Nothing


resolveH1specScript :: IO (Maybe FilePath)
resolveH1specScript = do
  envPath <- lookupEnv "WIREFORM_HTTP1_H1SPEC"
  let candidates = case envPath of
        Just p -> [p, p <> "/http_test.ts"]
        Nothing ->
          [ "/tmp/h1spec/http_test.ts"
          , "/usr/local/share/h1spec/http_test.ts"
          ]
  pickFirst doesFileExist candidates


resolveHttp11ProbeDll :: IO (Maybe FilePath)
resolveHttp11ProbeDll = do
  envPath <- lookupEnv "WIREFORM_HTTP1_HTTP11PROBE"
  let candidates = case envPath of
        Just p -> [p]
        Nothing ->
          [ "/tmp/http11probe/src/Http11Probe.Cli/bin/Release/net10.0/Http11Probe.Cli.dll"
          , "/tmp/http11probe/src/Http11Probe.Cli/bin/Debug/net10.0/Http11Probe.Cli.dll"
          ]
  pickFirst doesFileExist candidates


pickFirst :: (FilePath -> IO Bool) -> [FilePath] -> IO (Maybe FilePath)
pickFirst _ [] = pure Nothing
pickFirst p (x : xs) = do
  ok <- p x
  if ok then pure (Just x) else pickFirst p xs


assertSkip :: String -> IO ()
assertSkip reason = putStrLn ("  SKIPPED: " <> reason)


{- | Run a process, capture stdout, wait for exit. Stderr is forwarded
to ours so diagnostic output is visible.
-}
runProc :: FilePath -> [String] -> IO String
runProc exe args = do
  (_, Just hout, _, ph) <- createProcess (proc exe args) {std_out = CreatePipe}
  out <- hGetContents' hout
  _ <- waitForProcess ph
  pure out


hGetContents' :: Handle -> IO String
hGetContents' h = do
  s <- hGetContents h
  length s `seq` pure s
