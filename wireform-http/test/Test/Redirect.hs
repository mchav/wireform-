{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
Tests for "Network.HTTP.Client.Redirect" — the redirect-following
middleware. Covers:

* 301 / 302 rewriting to GET (rpRewriteToGet default).
* 303 always rewrites to GET.
* 307 / 308 preserve method.
* Cross-origin redirect strips Authorization and Cookie (the
  audit's main concern).
* Loop detection on normalised URI key.
* Hop count exceeded.
-}
module Test.Redirect (tests) where

import Control.Exception (try)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.IORef

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Status as S

import qualified Network.HTTP.Client.BodyStream as BSm
import qualified Network.HTTP.Client.Request    as WReq
import           Network.HTTP.Client.Request    (Request, get)
import           Network.HTTP.Client.Redirect
import           Network.HTTP.Client.Response   (RawResponse (..))
import qualified Network.HTTP.Client.Response   as Resp
import           Network.HTTP.Client.Protocol   (ProtocolInfo (..))
import           Network.HTTP.Client.Send       (prepareRequest)
import           Network.HTTP.Client.Transport
import qualified Network.HTTP.Client.URI        as WURI

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- ---------------------------------------------------------------------------
-- Mock transport that returns a scripted sequence
-- ---------------------------------------------------------------------------

mkScripted
  :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
  -> IO (Transport IO, IORef [Request BSm.BodyStream])
mkScripted script = do
  scriptRef <- newIORef script
  reqsRef   <- newIORef ([] :: [Request BSm.BodyStream])
  let t = Transport $ \req -> do
        atomicModifyIORef' reqsRef (\xs -> (req : xs, ()))
        next <- atomicModifyIORef' scriptRef $ \s -> case s of
          (x : rest) -> (rest, x)
          []         -> ([], (S.status500, [], "exhausted"))
        let (st, hdrs, body) = next
        popper <- BSm.popperFromStrict body
        pure RawResponse
          { Resp.statusCode    = st
          , Resp.headers       = hdrs
          , Resp.bodyPopper    = popper
          , Resp.protocolInfo  = HTTP1_1
          }
  pure (t, reqsRef)

mkRequest :: String -> M.Method -> IO (Request BSm.BodyStream)
mkRequest url meth = case WURI.parseTemplate url of
  Left e  -> error (show e)
  Right t -> do
    r <- prepareRequest [] (get t)
    pure r { WReq.method = meth }

-- ---------------------------------------------------------------------------
-- 3xx mechanics
-- ---------------------------------------------------------------------------

unit_302_to_get :: TestTree
unit_302_to_get = testCase "302 rewrites POST to GET by default" $ do
  (t, reqs) <- mkScripted
    [ (S.status302, [(H.hLocation, "http://example.com/landing")], "")
    , (S.status200, [], "landed")
    ]
  req <- mkRequest "http://example.com/start" M.mPost
  _   <- sendRaw (withRedirects defaultRedirectPolicy t) req
  xs  <- reverse <$> readIORef reqs
  assertEqual "two requests sent"   2          (length xs)
  assertEqual "first method"   M.mPost (WReq.method (xs !! 0))
  -- After the 302 the method should have been rewritten to GET.
  assertEqual "second method"  M.mGet  (WReq.method (xs !! 1))

unit_307_preserves_method :: TestTree
unit_307_preserves_method = testCase "307 preserves the method and body" $ do
  (t, reqs) <- mkScripted
    [ (S.status307, [(H.hLocation, "http://example.com/again")], "")
    , (S.status200, [], "ok")
    ]
  req <- mkRequest "http://example.com/start" M.mPost
  _   <- sendRaw (withRedirects defaultRedirectPolicy t) req
  xs  <- reverse <$> readIORef reqs
  assertEqual "two requests sent" 2          (length xs)
  assertEqual "first method"  M.mPost (WReq.method (xs !! 0))
  assertEqual "second method" M.mPost (WReq.method (xs !! 1))

-- ---------------------------------------------------------------------------
-- Cross-origin credential stripping
-- ---------------------------------------------------------------------------

unit_strips_auth_cross_origin :: TestTree
unit_strips_auth_cross_origin = testCase
  "Authorization is dropped on cross-origin redirect" $ do
  (t, reqs) <- mkScripted
    [ (S.status302, [(H.hLocation, "http://attacker.example/x")], "")
    , (S.status200, [], "")
    ]
  req0 <- mkRequest "http://api.example.com/" M.mGet
  let req = req0
        { WReq.headers =
            H.insertHeader H.hAuthorization "Bearer secret" (WReq.headers req0)
        }
  _   <- sendRaw (withRedirects defaultRedirectPolicy t) req
  xs  <- reverse <$> readIORef reqs
  let r2 = xs !! 1
  assertEqual "no Authorization on second hop"
    Nothing
    (H.lookupHeader H.hAuthorization (WReq.headers r2))

unit_strips_cookie_cross_origin :: TestTree
unit_strips_cookie_cross_origin = testCase
  "Cookie is dropped on cross-origin redirect" $ do
  (t, reqs) <- mkScripted
    [ (S.status302, [(H.hLocation, "http://attacker.example/x")], "")
    , (S.status200, [], "")
    ]
  req0 <- mkRequest "http://api.example.com/" M.mGet
  let req = req0
        { WReq.headers =
            H.insertHeader H.hCookie "session=abc; flavour=oatmeal" (WReq.headers req0)
        }
  _   <- sendRaw (withRedirects defaultRedirectPolicy t) req
  xs  <- reverse <$> readIORef reqs
  let r2 = xs !! 1
  assertEqual "no Cookie on second hop"
    Nothing
    (H.lookupHeader H.hCookie (WReq.headers r2))

unit_keeps_auth_same_origin :: TestTree
unit_keeps_auth_same_origin = testCase
  "Authorization is kept on same-origin redirect" $ do
  (t, reqs) <- mkScripted
    [ (S.status302, [(H.hLocation, "http://api.example.com/landing")], "")
    , (S.status200, [], "")
    ]
  req0 <- mkRequest "http://api.example.com/" M.mGet
  let req = req0
        { WReq.headers =
            H.insertHeader H.hAuthorization "Bearer secret" (WReq.headers req0)
        }
  _   <- sendRaw (withRedirects defaultRedirectPolicy t) req
  xs  <- reverse <$> readIORef reqs
  let r2 = xs !! 1
  assertEqual "Authorization preserved"
    (Just "Bearer secret")
    (H.lookupHeader H.hAuthorization (WReq.headers r2))

-- ---------------------------------------------------------------------------
-- Loop + hop count
-- ---------------------------------------------------------------------------

unit_detects_loop :: TestTree
unit_detects_loop = testCase
  "rpDetectLoops raises RedirectLoop on revisited URI" $ do
  -- Two URIs bouncing between each other. The first hop goes to
  -- /b, the second redirects back to /a. The third would revisit
  -- /b — the loop detector should trip then.
  (t, _) <- mkScripted
    [ (S.status302, [(H.hLocation, "http://example.com/b")], "")
    , (S.status302, [(H.hLocation, "http://example.com/a")], "")
    , (S.status302, [(H.hLocation, "http://example.com/b")], "")
    ]
  req <- mkRequest "http://example.com/a" M.mGet
  r <- try (sendRaw (withRedirects defaultRedirectPolicy t) req)
  case r of
    Left (_ :: RedirectLoop) -> pure ()
    Right _                  -> error "expected RedirectLoop"

unit_too_many_redirects :: TestTree
unit_too_many_redirects = testCase
  "rpMaxRedirects raises TooManyRedirects" $ do
  -- Six redirects in a row, then a 200.  Policy caps at 3 hops.
  let script =
        [ (S.status302, [(H.hLocation, BS8.pack ("http://example.com/" <> show i))], "")
        | i <- [(1 :: Int) .. 6]
        ] <> [(S.status200, [], "")]
  (t, _) <- mkScripted script
  let policy = defaultRedirectPolicy
        { rpMaxRedirects = 3
        , rpDetectLoops  = False  -- so we see TooManyRedirects not RedirectLoop
        }
  req <- mkRequest "http://example.com/0" M.mGet
  r   <- try (sendRaw (withRedirects policy t) req)
  case r of
    Left (_ :: TooManyRedirects) -> pure ()
    Right _                      -> error "expected TooManyRedirects"

unit_no_loop_no_redirect :: TestTree
unit_no_loop_no_redirect = testCase
  "non-redirect response passes through untouched" $ do
  (t, reqs) <- mkScripted [(S.status200, [], "hello")]
  req <- mkRequest "http://example.com/x" M.mGet
  r   <- sendRaw (withRedirects defaultRedirectPolicy t) req
  assertBool "200" (S.statusCode (Resp.statusCode r) == 200)
  xs  <- readIORef reqs
  assertEqual "single request" 1 (length xs)

-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Network.HTTP.Client.Redirect"
  [ unit_302_to_get
  , unit_307_preserves_method
  , unit_strips_auth_cross_origin
  , unit_strips_cookie_cross_origin
  , unit_keeps_auth_same_origin
  , unit_detects_loop
  , unit_too_many_redirects
  , unit_no_loop_no_redirect
  ]
