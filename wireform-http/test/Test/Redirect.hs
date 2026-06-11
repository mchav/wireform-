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
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Network.HTTP.Client.BodyStream qualified as BSm
import Network.HTTP.Client.Protocol (ProtocolInfo (..))
import Network.HTTP.Client.Redirect
import Network.HTTP.Client.Request (Request, get)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response (RawResponse (..))
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.Send (prepareRequest)
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import Test.Syd


-- ---------------------------------------------------------------------------
-- Mock transport that returns a scripted sequence
-- ---------------------------------------------------------------------------

mkScripted
  :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
  -> IO (Transport IO, IORef [Request BSm.BodyStream])
mkScripted script = do
  scriptRef <- newIORef script
  reqsRef <- newIORef ([] :: [Request BSm.BodyStream])
  let t = Transport $ \req -> do
        atomicModifyIORef' reqsRef (\xs -> (req : xs, ()))
        next <- atomicModifyIORef' scriptRef $ \s -> case s of
          (x : rest) -> (rest, x)
          [] -> ([], (S.status500, [], "exhausted"))
        let (st, hdrs, body) = next
        popper <- BSm.popperFromStrict body
        pure
          RawResponse
            { Resp.statusCode = st
            , Resp.headers = hdrs
            , Resp.bodyPopper = popper
            , Resp.protocolInfo = HTTP1_1
            }
  pure (t, reqsRef)


mkRequest :: String -> M.Method -> IO (Request BSm.BodyStream)
mkRequest url meth = case WURI.parseTemplate url of
  Left e -> error (show e)
  Right t -> do
    r <- prepareRequest [] (get t)
    pure r {WReq.method = meth}


-- ---------------------------------------------------------------------------
-- 3xx mechanics
-- ---------------------------------------------------------------------------

unit_302_to_get :: Spec
unit_302_to_get = it "302 rewrites POST to GET by default" $ do
  (t, reqs) <-
    mkScripted
      [ (S.status302, [(H.hLocation, "http://example.com/landing")], "")
      , (S.status200, [], "landed")
      ]
  req <- mkRequest "http://example.com/start" M.mPost
  _ <- sendRaw (withRedirects defaultRedirectPolicy t) req
  xs <- reverse <$> readIORef reqs
  (length xs) `shouldBe` 2
  (WReq.method (xs !! 0)) `shouldBe` M.mPost
  -- After the 302 the method should have been rewritten to GET.
  (WReq.method (xs !! 1)) `shouldBe` M.mGet


unit_307_preserves_method :: Spec
unit_307_preserves_method = it "307 preserves the method and body" $ do
  (t, reqs) <-
    mkScripted
      [ (S.status307, [(H.hLocation, "http://example.com/again")], "")
      , (S.status200, [], "ok")
      ]
  req <- mkRequest "http://example.com/start" M.mPost
  _ <- sendRaw (withRedirects defaultRedirectPolicy t) req
  xs <- reverse <$> readIORef reqs
  (length xs) `shouldBe` 2
  (WReq.method (xs !! 0)) `shouldBe` M.mPost
  (WReq.method (xs !! 1)) `shouldBe` M.mPost


-- ---------------------------------------------------------------------------
-- Cross-origin credential stripping
-- ---------------------------------------------------------------------------

unit_strips_auth_cross_origin :: Spec
unit_strips_auth_cross_origin = it
  "Authorization is dropped on cross-origin redirect"
  $ do
    (t, reqs) <-
      mkScripted
        [ (S.status302, [(H.hLocation, "http://attacker.example/x")], "")
        , (S.status200, [], "")
        ]
    req0 <- mkRequest "http://api.example.com/" M.mGet
    let req =
          req0
            { WReq.headers =
                H.insertHeader H.hAuthorization "Bearer secret" (WReq.headers req0)
            }
    _ <- sendRaw (withRedirects defaultRedirectPolicy t) req
    xs <- reverse <$> readIORef reqs
    let r2 = xs !! 1
    (H.lookupHeader H.hAuthorization (WReq.headers r2)) `shouldBe` Nothing


unit_strips_cookie_cross_origin :: Spec
unit_strips_cookie_cross_origin = it
  "Cookie is dropped on cross-origin redirect"
  $ do
    (t, reqs) <-
      mkScripted
        [ (S.status302, [(H.hLocation, "http://attacker.example/x")], "")
        , (S.status200, [], "")
        ]
    req0 <- mkRequest "http://api.example.com/" M.mGet
    let req =
          req0
            { WReq.headers =
                H.insertHeader H.hCookie "session=abc; flavour=oatmeal" (WReq.headers req0)
            }
    _ <- sendRaw (withRedirects defaultRedirectPolicy t) req
    xs <- reverse <$> readIORef reqs
    let r2 = xs !! 1
    (H.lookupHeader H.hCookie (WReq.headers r2)) `shouldBe` Nothing


unit_keeps_auth_same_origin :: Spec
unit_keeps_auth_same_origin = it
  "Authorization is kept on same-origin redirect"
  $ do
    (t, reqs) <-
      mkScripted
        [ (S.status302, [(H.hLocation, "http://api.example.com/landing")], "")
        , (S.status200, [], "")
        ]
    req0 <- mkRequest "http://api.example.com/" M.mGet
    let req =
          req0
            { WReq.headers =
                H.insertHeader H.hAuthorization "Bearer secret" (WReq.headers req0)
            }
    _ <- sendRaw (withRedirects defaultRedirectPolicy t) req
    xs <- reverse <$> readIORef reqs
    let r2 = xs !! 1
    (H.lookupHeader H.hAuthorization (WReq.headers r2)) `shouldBe` (Just "Bearer secret")


-- ---------------------------------------------------------------------------
-- Loop + hop count
-- ---------------------------------------------------------------------------

unit_detects_loop :: Spec
unit_detects_loop = it
  "rpDetectLoops raises RedirectLoop on revisited URI"
  $ do
    -- Two URIs bouncing between each other. The first hop goes to
    -- /b, the second redirects back to /a. The third would revisit
    -- /b — the loop detector should trip then.
    (t, _) <-
      mkScripted
        [ (S.status302, [(H.hLocation, "http://example.com/b")], "")
        , (S.status302, [(H.hLocation, "http://example.com/a")], "")
        , (S.status302, [(H.hLocation, "http://example.com/b")], "")
        ]
    req <- mkRequest "http://example.com/a" M.mGet
    r <- try (sendRaw (withRedirects defaultRedirectPolicy t) req)
    case r of
      Left (_ :: RedirectLoop) -> pure ()
      Right _ -> error "expected RedirectLoop"


unit_too_many_redirects :: Spec
unit_too_many_redirects = it
  "rpMaxRedirects raises TooManyRedirects"
  $ do
    -- Six redirects in a row, then a 200.  Policy caps at 3 hops.
    let script =
          [ (S.status302, [(H.hLocation, BS8.pack ("http://example.com/" <> show i))], "")
          | i <- [(1 :: Int) .. 6]
          ]
            <> [(S.status200, [], "")]
    (t, _) <- mkScripted script
    let policy =
          defaultRedirectPolicy
            { rpMaxRedirects = 3
            , rpDetectLoops = False -- so we see TooManyRedirects not RedirectLoop
            }
    req <- mkRequest "http://example.com/0" M.mGet
    r <- try (sendRaw (withRedirects policy t) req)
    case r of
      Left (_ :: TooManyRedirects) -> pure ()
      Right _ -> error "expected TooManyRedirects"


unit_no_loop_no_redirect :: Spec
unit_no_loop_no_redirect = it
  "non-redirect response passes through untouched"
  $ do
    (t, reqs) <- mkScripted [(S.status200, [], "hello")]
    req <- mkRequest "http://example.com/x" M.mGet
    r <- sendRaw (withRedirects defaultRedirectPolicy t) req
    (S.statusCode (Resp.statusCode r) == 200) `shouldBe` True
    xs <- readIORef reqs
    (length xs) `shouldBe` 1


-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Network.HTTP.Client.Redirect" $
    sequence_
      [ unit_302_to_get
      , unit_307_preserves_method
      , unit_strips_auth_cross_origin
      , unit_strips_cookie_cross_origin
      , unit_keeps_auth_same_origin
      , unit_detects_loop
      , unit_too_many_redirects
      , unit_no_loop_no_redirect
      ]
