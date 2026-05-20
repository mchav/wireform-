{-# LANGUAGE OverloadedStrings #-}
{- | Pure tests over the vendored HTTP type primitives.

These exist primarily to guard the @Network.HTTP.Types.*@ rebrand
against regressions: the same operations the original @hermes@
library shipped should still behave the same after we trimmed the
dep closure.
-}
module Test.VersionTypes (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Status as S
import qualified Network.HTTP.Types.Version as V

tests :: TestTree
tests = testGroup "Types"
  [ versionRoundTrips
  , versionOrder
  , methodConstants
  , statusCategories
  , headerLookups
  ]

versionRoundTrips :: TestTree
versionRoundTrips = testCase "Version round-trips through canonical bytes" $ do
  V.versionFromBytes "HTTP/1.0" @?= Just V.HTTP1_0
  V.versionFromBytes "HTTP/1.1" @?= Just V.HTTP1_1
  V.versionFromBytes "HTTP/2"   @?= Just V.HTTP2
  V.versionFromBytes "HTTP/2.0" @?= Just V.HTTP2
  V.versionFromBytes "HTTP/3"   @?= Just V.HTTP3
  V.versionFromBytes "HTTP/9.9" @?= Nothing
  V.versionToBytes V.HTTP1_0    @?= "HTTP/1.0"
  V.versionToBytes V.HTTP1_1    @?= "HTTP/1.1"
  V.versionToBytes V.HTTP2      @?= "HTTP/2"

versionOrder :: TestTree
versionOrder = testCase "Version Ord is lexicographic on (major, minor)" $ do
  assertBool "1.0 < 1.1" (V.HTTP1_0 < V.HTTP1_1)
  assertBool "1.1 < 2"   (V.HTTP1_1 < V.HTTP2)
  assertBool "2   < 3"   (V.HTTP2   < V.HTTP3)
  V.versionMajor V.HTTP1_1 @?= 1
  V.versionMinor V.HTTP1_1 @?= 1
  V.versionMajor V.HTTP2   @?= 2
  V.versionMinor V.HTTP2   @?= 0

methodConstants :: TestTree
methodConstants = testCase "Method constants serialise correctly" $ do
  M.methodToBytes M.mGet @?= "GET"
  M.methodToBytes M.mPost @?= "POST"
  M.methodToBytes M.mPropFind @?= "PROPFIND"
  assertBool "GET is safe" (M.isSafe M.mGet)
  assertBool "GET is idempotent" (M.isIdempotent M.mGet)
  assertBool "POST is not safe" (not (M.isSafe M.mPost))
  assertBool "POST is not idempotent" (not (M.isIdempotent M.mPost))
  assertBool "POST allows body" (M.bodyAllowedInRequest M.mPost)

statusCategories :: TestTree
statusCategories = testCase "Status categorises by 100s digit" $ do
  S.statusCategory S.status200 @?= S.Successful
  S.statusCategory S.status301 @?= S.Redirection
  S.statusCategory S.status404 @?= S.ClientError
  S.statusCategory S.status500 @?= S.ServerError
  S.statusCategory (S.Status 0) @?= S.UnknownCategory
  S.statusReason S.status200 @?= "OK"
  S.statusReason S.status404 @?= "Not Found"
  S.statusReason (S.Status 999) @?= ""

headerLookups :: TestTree
headerLookups = testCase "Header lookups are case-insensitive and order-preserving" $ do
  let hs = [(H.hContentType, "text/plain"), (H.hContentLength, "10"), (H.hContentType, "application/json")]
  H.lookupHeader H.hContentType hs @?= Just "text/plain"
  H.lookupHeaders H.hContentType hs @?= ["text/plain", "application/json"]
  H.hasHeader H.hContentLength hs @?= True
  H.hasHeader H.hAuthorization hs @?= False
  let hs2 = H.insertHeader H.hContentType "text/html" hs
  H.lookupHeaders H.hContentType hs2 @?= ["text/html"]
  let hs3 = H.deleteHeader H.hContentType hs
  H.lookupHeader H.hContentType hs3 @?= Nothing
