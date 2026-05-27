{-# LANGUAGE OverloadedStrings #-}
{- |
Tests for the RFC 6265bis ingest hardening added to
"Network.HTTP.Client.Cookies": Domain acceptance, @SameSite=None@
requires @Secure@, the @__Host-@ / @__Secure-@ name-prefix rules,
and the per-cookie size limit.
-}
module Test.Cookies (tests) where

import qualified Data.ByteString as BS

import Network.HTTP.Client.Cookies

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

baseCookie :: Cookie
baseCookie = Cookie
  { cookieName           = "session"
  , cookieValue          = "abc"
  , cookieDomain         = "example.com"
  , cookieDomainExplicit = False
  , cookiePath           = "/"
  , cookieExpires        = Nothing
  , cookieSecure         = True
  , cookieHttpOnly       = False
  , cookieSameSite       = SameSiteLax
  }

withJarThat :: (CookieJar -> IO a) -> IO a
withJarThat k = do
  jar <- newCookieJar
  k jar

tests :: TestTree
tests = testGroup "Network.HTTP.Client.Cookies"
  [ testGroup "validateCookieName / validateCookieValue"
      [ testCase "valid token name accepted" $
          validateCookieName "session" @?= Right ()
      , testCase "empty name rejected" $
          validateCookieName "" @?= Left (CookieNameInvalid "")
      , testCase "name with whitespace rejected" $
          validateCookieName "bad name"
            @?= Left (CookieNameInvalid "bad name")
      , testCase "valid cookie-octet value accepted" $
          validateCookieValue "opaque-token.123" @?= Right ()
      , testCase "value with comma rejected" $
          validateCookieValue "has,comma"
            @?= Left (CookieValueInvalid "has,comma")
      ]
  , testGroup "insertCookieChecked"
      [ testCase "SameSite=None without Secure is rejected" $ withJarThat $ \jar -> do
          let c = baseCookie
                { cookieSecure   = False
                , cookieSameSite = SameSiteNone
                }
          r <- insertCookieChecked jar c
          r @?= Left (CookieSameSiteNoneRequiresSecure "session")
      , testCase "SameSite=None with Secure is accepted" $ withJarThat $ \jar -> do
          let c = baseCookie
                { cookieSecure   = True
                , cookieSameSite = SameSiteNone
                }
          r <- insertCookieChecked jar c
          assertBool "should accept" (r == Right ())
      , testCase "__Secure- prefix without Secure flag is rejected" $ withJarThat $ \jar -> do
          let c = baseCookie
                { cookieName   = "__Secure-token"
                , cookieSecure = False
                }
          r <- insertCookieChecked jar c
          r @?= Left (CookieSecurePrefixWithoutSecure "__Secure-token")
      , testCase "__Secure- prefix with Secure flag is accepted" $ withJarThat $ \jar -> do
          let c = baseCookie
                { cookieName   = "__Secure-token"
                , cookieSecure = True
                }
          r <- insertCookieChecked jar c
          assertBool "should accept" (r == Right ())
      , testCase "__Host- prefix requires Secure + Path=/ + no Domain" $ withJarThat $ \jar -> do
          let c = baseCookie
                { cookieName           = "__Host-id"
                , cookieDomainExplicit = True
                , cookieSecure         = True
                , cookiePath           = "/"
                }
          r <- insertCookieChecked jar c
          r @?= Left (CookieHostPrefixViolation "__Host-id")
      , testCase "__Host- with no Domain, Path=/, Secure is accepted" $ withJarThat $ \jar -> do
          let c = baseCookie
                { cookieName           = "__Host-id"
                , cookieDomainExplicit = False
                , cookieSecure         = True
                , cookiePath           = "/"
                }
          r <- insertCookieChecked jar c
          assertBool "should accept" (r == Right ())
      , testCase "size limit rejects oversize cookies" $ withJarThat $ \jar -> do
          let c = baseCookie { cookieValue = BS.replicate 5000 0x78 }
          r <- insertCookieChecked jar c
          case r of
            Left CookieTooLarge{} -> pure ()
            other                 -> error (show other)
      ]
  ]
