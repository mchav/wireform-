{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the parser's defensive limits against the
-- well-known YAML denial-of-service classes:
--
-- * 'billion laughs' — a small input with N levels of K-fold
--   alias references that expands to K^N nodes when traversed
--   naively.
-- * deep nesting — '[[[[…]]]]' / line-after-indented-line that
--   consumes recursive Haskell stack proportional to its depth.
-- * cyclic aliases — an anchor that references itself.
module Test.YAML.Security (tests) where

import Data.Either (isLeft)
import Data.List (isInfixOf)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( testCase, assertBool, assertFailure )

import qualified YAML.Decode as Y
import qualified YAML.Value  as YV

tests :: TestTree
tests = testGroup "Security"
  [ billionLaughsRefused
  , deepBlockRefused
  , selfCycleRejected
  , flowCycleResolution
  , normalAliasesAllowed
  , normalNestingAllowed
  ]

-- | A 9-level / 9-fold billion-laughs document expands to 9^9
-- ~ 4×10^8 logical nodes and must be rejected before the parser
-- hands a 'Value' to the caller.
billionLaughsRefused :: TestTree
billionLaughsRefused =
    testCase "9-level billion laughs is refused" $ do
  let src = T.unlines
        [ "a: &a [1,1,1,1,1,1,1,1,1]"
        , "b: &b [*a,*a,*a,*a,*a,*a,*a,*a,*a]"
        , "c: &c [*b,*b,*b,*b,*b,*b,*b,*b,*b]"
        , "d: &d [*c,*c,*c,*c,*c,*c,*c,*c,*c]"
        , "e: &e [*d,*d,*d,*d,*d,*d,*d,*d,*d]"
        , "f: &f [*e,*e,*e,*e,*e,*e,*e,*e,*e]"
        , "g: &g [*f,*f,*f,*f,*f,*f,*f,*f,*f]"
        , "h: &h [*g,*g,*g,*g,*g,*g,*g,*g,*g]"
        , "i: &i [*h,*h,*h,*h,*h,*h,*h,*h,*h]"
        , "value: *i"
        ]
  case Y.decode src of
    Left e ->
      assertBool ("expected billion-laughs error, got: " <> e)
        ("billion-laughs" `isInfixOf` e
         || "alias DAG"  `isInfixOf` e
         || "expand"     `isInfixOf` e)
    Right _ -> assertFailure "expected the 9-level billion-laughs to be refused"

-- | A document nested past 'maxParserDepth' must error rather
-- than blowing the runtime stack.
deepBlockRefused :: TestTree
deepBlockRefused = testCase "deep block nesting is refused" $ do
  -- Build a 2000-deep nested mapping: 'a:\\n  a:\\n    a:\\n …'
  let depth = 2000 :: Int
      src   = T.intercalate "\n"
        [ T.replicate (2*i) " " <> "a:"
        | i <- [0 .. depth - 1] ]
        <> "\n" <> T.replicate (2*depth) " " <> "leaf"
  case Y.decode src of
    Left e ->
      assertBool ("expected nesting error, got: " <> e)
        ("nesting" `isInfixOf` e || "depth" `isInfixOf` e)
    Right _ -> assertFailure "expected deep nesting to be refused"

-- | A self-referencing alias must error. Forward references are
-- illegal per YAML 1.2, so this is a pre-existing guarantee;
-- the test pins it down.
selfCycleRejected :: TestTree
selfCycleRejected = testCase "self-cycle in alias is rejected" $ do
  let cases = [ "a: &a [*a]", "&a\n- *a", "key: &a *a" ]
  mapM_ check cases
  where
    check src =
      assertBool ("expected error on " <> show src)
        (isLeft (Y.decode src))

-- | A flow document whose registered anchor's body contains an
-- alias to the SAME anchor must error rather than producing a
-- value tree with unresolved alias sentinels.
flowCycleResolution :: TestTree
flowCycleResolution = testCase "flow alias cycle is rejected" $ do
  -- The flow alias '*a' inside '&a [ ... ]' is recorded after
  -- the surrounding flow node is fully parsed; the resolution
  -- pass then encounters the cycle.
  let src = T.pack "{a: &a {self: *a}}"
  case Y.decode src of
    Left _ -> pure ()         -- any rejection is acceptable; the
                              -- combined record-and-resolve pass
                              -- naturally surfaces the cycle as a
                              -- 'no anchor' error because we walk
                              -- bottom-up.
    Right _ -> assertFailure "expected flow cycle to be refused"

-- | A modest, normal use of aliases (small fan-out, single
-- level) must continue to parse.
normalAliasesAllowed :: TestTree
normalAliasesAllowed = testCase "small aliases are allowed" $ do
  let src = T.unlines
        [ "defaults: &d"
        , "  retries: 3"
        , "  timeout: 30"
        , "alpha: *d"
        , "beta: *d"
        ]
  case Y.decode src of
    Right v ->
      case YV.lookupKey "alpha" v of
        Just _ -> pure ()
        Nothing -> assertFailure "expected alpha to resolve"
    Left e -> assertFailure ("unexpected error: " <> e)

-- | A document nested below 'maxParserDepth' must continue to
-- parse. Pin the limit at 'a few hundred'.
normalNestingAllowed :: TestTree
normalNestingAllowed = testCase "moderate nesting is allowed" $ do
  let depth = 200 :: Int
      src   = T.intercalate "\n"
        [ T.replicate (2*i) " " <> "a:"
        | i <- [0 .. depth - 1] ]
        <> "\n" <> T.replicate (2*depth) " " <> "leaf"
  case Y.decode src of
    Right _ -> pure ()
    Left e  -> assertFailure ("unexpected error at depth " <> show depth
                              <> ": " <> e)
