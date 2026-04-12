{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty
import Test.Tasty.HUnit

import HTML.Selector
import HTML.Rewriter

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

builderToBS :: BB.Builder -> BS.ByteString
builderToBS = BL.toStrict . BB.toLazyByteString

mustBuild :: RewriterBuilder () -> Rewriter
mustBuild b = case buildRewriter b of
  Right rw -> rw
  Left e   -> error ("buildRewriter failed: " ++ show e)

mustParseSelector :: Text -> Selector
mustParseSelector t = case parseSelector t of
  Right s  -> s
  Left e   -> error ("parseSelector failed: " ++ show e)

rewriteStr :: Rewriter -> BS.ByteString -> IO BS.ByteString
rewriteStr = rewrite

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "HTML Rewriter"
  [ testGroup "Selector parser" selectorParserTests
  , testGroup "Passthrough" passthroughTests
  , testGroup "Element mutation" elementMutationTests
  , testGroup "Text mutation" textMutationTests
  , testGroup "Comment mutation" commentMutationTests
  , testGroup "Content insertion" insertionTests
  , testGroup "Element removal" removalTests
  , testGroup "Streaming" streamingTests
  ]

selectorParserTests :: [TestTree]
selectorParserTests =
  [ testCase "simple tag" $ do
      let Right (Selector [ComplexSelector (CompoundSelector (Just (TypeTag "div")) []) []]) = parseSelector "div"
      pure ()

  , testCase "class selector" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelClass "foo"]) []]) = parseSelector ".foo"
      pure ()

  , testCase "id selector" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelId "main"]) []]) = parseSelector "#main"
      pure ()

  , testCase "tag.class" $ do
      let Right (Selector [ComplexSelector (CompoundSelector (Just (TypeTag "div")) [SelClass "foo"]) []]) = parseSelector "div.foo"
      pure ()

  , testCase "attribute exact" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelAttrExact "type" "text"]) []]) = parseSelector "[type=\"text\"]"
      pure ()

  , testCase "attribute prefix" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelAttrPrefix "href" "https"]) []]) = parseSelector "[href^=\"https\"]"
      pure ()

  , testCase "descendant combinator" $ do
      let Right (Selector [ComplexSelector _ ((Descendant, _):_)]) = parseSelector "div span"
      pure ()

  , testCase "child combinator" $ do
      let Right (Selector [ComplexSelector _ ((Child, _):_)]) = parseSelector "div > span"
      pure ()

  , testCase "comma group" $ do
      let Right (Selector sels) = parseSelector "h1, h2, h3"
      length sels @?= 3

  , testCase "rewriter compatible" $ do
      isRewriterCompatible (mustParseSelector "div.foo > span[href]") @? "should be compatible"

  , testCase "rewriter incompatible (pseudo)" $ do
      not (isRewriterCompatible (mustParseSelector "div:first-child")) @? "should be incompatible"

  , testCase "rewriter incompatible (sibling)" $ do
      not (isRewriterCompatible (mustParseSelector "h1 + p")) @? "should be incompatible"

  , testCase "syntax error" $ do
      case parseSelector "" of
        Left (SelectorSyntaxError _ _) -> pure ()
        Right _ -> assertFailure "should fail on empty"
  ]

passthroughTests :: [TestTree]
passthroughTests =
  [ testCase "no handlers = passthrough" $ do
      let rw = mustBuild (pure ())
      result <- rewriteStr rw "<div><p>Hello</p></div>"
      BS.isInfixOf "Hello" result @? "should contain Hello"
      BS.isInfixOf "<div>" result @? "should contain <div>"

  , testCase "doctype preserved" $ do
      let rw = mustBuild (pure ())
      result <- rewriteStr rw "<!DOCTYPE html><html><body>Hi</body></html>"
      BS.isInfixOf "<!DOCTYPE html>" result @? "should contain doctype"

  , testCase "comments preserved" $ do
      let rw = mustBuild (pure ())
      result <- rewriteStr rw "<div><!-- note --><p>text</p></div>"
      BS.isInfixOf "<!-- note -->" result @? "should contain comment"

  , testCase "attributes preserved" $ do
      let rw = mustBuild (pure ())
      result <- rewriteStr rw "<div id=\"main\" class=\"foo\">text</div>"
      BS.isInfixOf "id=" result @? "should contain id attribute"
      BS.isInfixOf "class=" result @? "should contain class attribute"
  ]

elementMutationTests :: [TestTree]
elementMutationTests =
  [ testCase "setTagName renames element" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            setTagName er "section"
      result <- rewriteStr rw "<div>content</div>"
      BS.isInfixOf "<section>" result @? "should have renamed start tag"
      BS.isInfixOf "content" result @? "should have content"

  , testCase "setAttr adds attribute" $ do
      let rw = mustBuild $ onElement (mustParseSelector "p") $ \er -> do
            setElemAttr er "class" "highlight"
      result <- rewriteStr rw "<p>text</p>"
      BS.isInfixOf "class=\"highlight\"" result @? "should have class attribute"

  , testCase "removeAttr removes attribute" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            removeElemAttr er "id"
      result <- rewriteStr rw "<div id=\"main\">text</div>"
      not (BS.isInfixOf "id=" result) @? "should not have id attribute"

  , testCase "getAttr reads attribute" $ do
      ref <- newIORef T.empty
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            mval <- getElemAttr er "id"
            case mval of
              Just v -> writeIORef ref v
              Nothing -> pure ()
      _ <- rewriteStr rw "<div id=\"main\">text</div>"
      val <- readIORef ref
      val @?= "main"

  , testCase "getElemAttrs returns all" $ do
      ref <- newIORef []
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            as <- getElemAttrs er
            writeIORef ref as
      _ <- rewriteStr rw "<div id=\"a\" class=\"b\">x</div>"
      as <- readIORef ref
      length as @?= 2
  ]

textMutationTests :: [TestTree]
textMutationTests =
  [ testCase "replaceTextChunk" $ do
      let rw = mustBuild $ onText (mustParseSelector "p") $ \tr -> do
            replaceTextChunk tr "REPLACED" AsText
      result <- rewriteStr rw "<p>original</p>"
      BS.isInfixOf "REPLACED" result @? "should have replacement"
      not (BS.isInfixOf "original" result) @? "should not have original"

  , testCase "removeTextChunk" $ do
      let rw = mustBuild $ onText (mustParseSelector "p") $ \tr -> do
            removeTextChunk tr
      result <- rewriteStr rw "<p>gone</p>"
      not (BS.isInfixOf "gone" result) @? "text should be removed"
  ]

commentMutationTests :: [TestTree]
commentMutationTests =
  [ testCase "removeComment" $ do
      let rw = mustBuild $ onComment $ \cr -> do
            removeComment cr
      result <- rewriteStr rw "<div><!-- delete me --><p>keep</p></div>"
      not (BS.isInfixOf "delete me" result) @? "comment should be removed"
      BS.isInfixOf "keep" result @? "content should remain"

  , testCase "setCommentText" $ do
      let rw = mustBuild $ onComment $ \cr -> do
            setCommentText cr "modified"
      result <- rewriteStr rw "<div><!-- original --></div>"
      BS.isInfixOf "modified" result @? "should have new comment text"
  ]

insertionTests :: [TestTree]
insertionTests =
  [ testCase "beforeElement" $ do
      let rw = mustBuild $ onElement (mustParseSelector "p") $ \er -> do
            beforeElement er "<hr>" AsHTML
      result <- rewriteStr rw "<div><p>text</p></div>"
      let s = TE.decodeUtf8 result
      T.isInfixOf "<hr><p>" s @? "hr should be before p: " ++ show s

  , testCase "afterElement" $ do
      let rw = mustBuild $ onElement (mustParseSelector "p") $ \er -> do
            afterElement er "<hr>" AsHTML
      result <- rewriteStr rw "<div><p>text</p></div>"
      let s = TE.decodeUtf8 result
      T.isInfixOf "</p><hr>" s @? "hr should be after p: " ++ show s

  , testCase "prependToElement" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            prependToElement er "<b>first</b>" AsHTML
      result <- rewriteStr rw "<div>existing</div>"
      let s = TE.decodeUtf8 result
      T.isInfixOf "<div><b>first</b>" s @? "should prepend: " ++ show s

  , testCase "appendToElement" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            appendToElement er "<b>last</b>" AsHTML
      result <- rewriteStr rw "<div>existing</div>"
      let s = TE.decodeUtf8 result
      T.isInfixOf "<b>last</b></div>" s @? "should append: " ++ show s

  , testCase "setInnerContent" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            setInnerContent er "new content" AsText
      result <- rewriteStr rw "<div>old content</div>"
      let s = TE.decodeUtf8 result
      T.isInfixOf "new content" s @? "should have new content: " ++ show s
      not (T.isInfixOf "old content" s) @? "should not have old: " ++ show s
  ]

removalTests :: [TestTree]
removalTests =
  [ testCase "removeElement removes element and children" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div.remove") $ \er -> do
            removeElement er
      result <- rewriteStr rw "<main><div class=\"remove\"><p>gone</p></div><p>keep</p></main>"
      not (BS.isInfixOf "gone" result) @? "removed content should be gone"
      BS.isInfixOf "keep" result @? "non-removed content should remain"

  , testCase "replaceElement" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            replaceElement er "<span>replaced</span>" AsHTML
      result <- rewriteStr rw "<main><div><p>old</p></div></main>"
      BS.isInfixOf "replaced" result @? "should have replacement"
      not (BS.isInfixOf "old" result) @? "should not have old content"
  ]

streamingTests :: [TestTree]
streamingTests =
  [ testCase "newRewriterState + feedRewriter + finishRewriter" $ do
      let rw = mustBuild $ onElement (mustParseSelector "p") $ \er -> do
            setElemAttr er "class" "modified"
      st <- newRewriterState rw
      feedRewriter st "<div><p>he"
      feedRewriter st "llo</p></div>"
      result <- finishRewriter st
      let bs = BL.toStrict (BB.toLazyByteString result)
      BS.isInfixOf "class=\"modified\"" bs @? "should have modified attribute"

  , testCase "empty rewriter state" $ do
      let rw = mustBuild (pure ())
      st <- newRewriterState rw
      result <- finishRewriter st
      let bs = BL.toStrict (BB.toLazyByteString result)
      BS.null bs @? "empty input should give empty output"
  ]
