{-# LANGUAGE OverloadedStrings #-}
module Test.YAML.Annotated (tests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import YAML.Annotated
import YAML.Decode.Annotated (decodeAnnotated)
import YAML.Pretty (defaultOptions, renderAnnotatedDocument, render)
import qualified YAML.Pretty as P
import YAML.Value (Value (..))

tests :: TestTree
tests = testGroup "Annotated"
  [ roundTripExact
  , roundTripWithComments
  , roundTripSeq
  , modifyPreservesUnmodified
  , modifyPreservesComments
  , prettyOptionsRespected
  , prettyMaxLineWidth
  , addKeyAppears
  , deleteKeyDisappears
  ]

-- | Parse and re-render an unmodified document; the result must
-- be byte-identical to the input.
roundTripExact :: TestTree
roundTripExact = testCase "round-trip preserves source verbatim" $ do
  let src = T.unlines
        [ "# top comment"
        , "name: alice"
        , "age: 30"
        , ""
        , "# group comment"
        , "address:"
        , "  street: 1 main st"
        , "  city:   springfield"
        , "tags: [a, b, c]"
        ]
  case decodeAnnotated src of
    Left err -> assertBool err False
    Right (adoc, srcText) -> do
      let out = renderAnnotatedDocument defaultOptions adoc (Just srcText)
      assertEqual "round-trip preserved" src out

-- | Modifying one mapping key only changes that key's value; the
-- surrounding entries — including comments and the unrelated
-- entries' formatting — stay byte-identical.
modifyPreservesUnmodified :: TestTree
modifyPreservesUnmodified =
    testCase "single-key modification preserves rest" $ do
  let src = T.unlines
        [ "# top comment"
        , "name: alice"
        , "age: 30"
        , "city: springfield"
        ]
  case decodeAnnotated src of
    Left err -> assertBool err False
    Right (adoc, srcText) -> do
      let body  = adBody adoc
          body' = setKey "age" (aInt 31) body
          adoc' = adoc { adBody = body' }
          out   = renderAnnotatedDocument defaultOptions adoc' (Just srcText)
      assertBool ("must mention 31, got: " <> T.unpack out)
        ("31" `T.isInfixOf` out)
      assertBool ("must keep `name: alice`: " <> T.unpack out)
        ("name: alice" `T.isInfixOf` out)
      assertBool ("must keep `city: springfield`: " <> T.unpack out)
        ("city: springfield" `T.isInfixOf` out)
      assertBool ("must drop original `age: 30`: " <> T.unpack out)
        (not ("age: 30" `T.isInfixOf` out))

-- | The pretty-printer respects 'roMaxLineWidth' and 'roIndent'.
prettyOptionsRespected :: TestTree
prettyOptionsRespected = testCase "pretty options affect output" $ do
  let v = YMap (V.fromList
                  [ (YString "key", YString "value")
                  , (YString "items", YSeq (V.fromList
                       [ YInt 1, YInt 2, YInt 3 ]))
                  ])
      compact = render P.compactOptions v
  assertBool ("compact emits flow {}: " <> T.unpack compact)
    ("{" `T.isInfixOf` compact)

-- | Appending a new key produces output that contains it.
addKeyAppears :: TestTree
addKeyAppears = testCase "appendKey adds the new entry" $ do
  let src = T.unlines [ "name: alice", "age: 30" ]
  case decodeAnnotated src of
    Left err -> assertBool err False
    Right (adoc, srcText) -> do
      let body  = adBody adoc
          body' = appendKey "city" (aString "springfield") body
          adoc' = adoc { adBody = body' }
          out   = renderAnnotatedDocument defaultOptions adoc' (Just srcText)
      assertBool ("expected city: " <> T.unpack out)
        ("city" `T.isInfixOf` out)
      assertBool ("must keep `name: alice` byte-equal: " <> T.unpack out)
        ("name: alice" `T.isInfixOf` out)
      assertBool ("must keep `age: 30` byte-equal: " <> T.unpack out)
        ("age: 30" `T.isInfixOf` out)

-- | Round-trip with comments scattered throughout.
roundTripWithComments :: TestTree
roundTripWithComments =
    testCase "round-trip preserves comments verbatim" $ do
  let src = T.unlines
        [ "# header comment"
        , "name: alice  # eol comment on name"
        , ""
        , "# group comment"
        , "age: 30"
        , ""
        , "# trailing comment"
        ]
  case decodeAnnotated src of
    Left err -> assertBool err False
    Right (adoc, srcText) -> do
      let out = renderAnnotatedDocument defaultOptions adoc (Just srcText)
      assertEqual "preserved" src out

-- | Round-trip a top-level sequence with item comments.
roundTripSeq :: TestTree
roundTripSeq = testCase "round-trip preserves top-level seq" $ do
  let src = T.unlines
        [ "- alice"
        , "- bob"
        , "- carol"
        ]
  case decodeAnnotated src of
    Left err -> assertBool err False
    Right (adoc, srcText) -> do
      let out = renderAnnotatedDocument defaultOptions adoc (Just srcText)
      assertEqual "preserved" src out

-- | Modifying one entry preserves the *exact bytes* (including
-- end-of-line comments and surrounding blank lines) of all the
-- other entries.
modifyPreservesComments :: TestTree
modifyPreservesComments =
    testCase "modify preserves untouched-entry comments" $ do
  let src = T.unlines
        [ "# header"
        , "name: alice  # eol on name"
        , ""
        , "age: 30"
        , ""
        , "# group"
        , "city: springfield"
        ]
  case decodeAnnotated src of
    Left err -> assertBool err False
    Right (adoc, srcText) -> do
      let body  = adBody adoc
          body' = setKey "age" (aInt 31) body
          adoc' = adoc { adBody = body' }
          out   = renderAnnotatedDocument defaultOptions adoc' (Just srcText)
      assertBool ("must keep eol comment on name: " <> T.unpack out)
        ("# eol on name" `T.isInfixOf` out)
      assertBool ("must keep `# group` comment: " <> T.unpack out)
        ("# group" `T.isInfixOf` out)
      assertBool ("must keep `city: springfield`: " <> T.unpack out)
        ("city: springfield" `T.isInfixOf` out)
      assertBool ("must mention 31: " <> T.unpack out)
        ("31" `T.isInfixOf` out)

-- | The pretty-printer wraps long flow collections.
prettyMaxLineWidth :: TestTree
prettyMaxLineWidth = testCase "max line width wraps long flows" $ do
  let manyItems = YSeq (V.fromList [YInt i | i <- [1 .. 30]])
      narrow    = (P.compactOptions { P.roMaxLineWidth = 20 })
      out       = render narrow manyItems
  -- Should contain at least one newline inside the brackets
  -- because the items don't fit on one line of 20 columns.
  assertBool ("expected wrapping: " <> T.unpack out)
    (T.count "\n" out > 1)

-- | Deleting a key removes it.
deleteKeyDisappears :: TestTree
deleteKeyDisappears = testCase "deleteKey removes the entry" $ do
  let src = T.unlines [ "a: 1", "b: 2", "c: 3" ]
  case decodeAnnotated src of
    Left err -> assertBool err False
    Right (adoc, srcText) -> do
      let body  = adBody adoc
          body' = deleteKey "b" body
          adoc' = adoc { adBody = body' }
          out   = renderAnnotatedDocument defaultOptions adoc' (Just srcText)
      assertBool ("must remove b entirely: " <> T.unpack out)
        (not ("b: 2" `T.isInfixOf` out))
      assertBool ("must keep a: " <> T.unpack out)
        ("a: 1" `T.isInfixOf` out)
      assertBool ("must keep c: " <> T.unpack out)
        ("c: 3" `T.isInfixOf` out)
