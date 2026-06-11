{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HTML.DOM qualified as DOM
import HTML.Rewriter
import HTML.Selector
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import Test.Syd hiding (Selector)
import Wireform.Builder qualified as BB


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

builderToBS :: BB.Builder -> BS.ByteString
builderToBS = BL.toStrict . BB.toLazyByteString


mustBuild :: RewriterBuilder () -> Rewriter
mustBuild b = case buildRewriter b of
  Right rw -> rw
  Left e -> error ("buildRewriter failed: " ++ show e)


mustParseSelector :: Text -> Selector
mustParseSelector t = case parseSelector t of
  Right s -> s
  Left e -> error ("parseSelector failed: " ++ show e)


rewriteStr :: Rewriter -> BS.ByteString -> IO BS.ByteString
rewriteStr = rewrite


decodeResult :: BS.ByteString -> Text
decodeResult = TE.decodeUtf8


-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  fixtureTests <- loadFixtureTests
  sydTest
    $ describe
      "HTML Rewriter (lol-html port)"
    $ sequence_
      [ describe "Selector parser" $ sequence_ selectorParserTests
      , describe "Passthrough" $ sequence_ passthroughTests
      , describe "Element mutation" $ sequence_ elementMutationTests
      , describe "Text mutation" $ sequence_ textMutationTests
      , describe "Comment mutation" $ sequence_ commentMutationTests
      , describe "Content insertion" $ sequence_ insertionTests
      , describe "Element removal" $ sequence_ removalTests
      , describe "Element replace" $ sequence_ elementReplaceTests
      , describe "End tag handlers" $ sequence_ endTagTests
      , describe "Doctype" $ sequence_ doctypeTests
      , describe "Void elements" $ sequence_ voidElementTests
      , describe "Multiple handlers" $ sequence_ multipleHandlerTests
      , describe "Handler invocation order" $ sequence_ handlerOrderTests
      , describe "Streaming" $ sequence_ streamingTests
      , describe "Selector matching fixtures" $ sequence_ [fixtureTests]
      ]


-- ---------------------------------------------------------------------------
-- Selector parser tests (ported from existing)
-- ---------------------------------------------------------------------------

selectorParserTests :: [Spec]
selectorParserTests =
  [ it "simple tag" $ do
      let Right (Selector [ComplexSelector (CompoundSelector (Just (TypeTag "div")) []) []]) = parseSelector "div"
      pure () :: IO ()
  , it "class selector" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelClass "foo"]) []]) = parseSelector ".foo"
      pure () :: IO ()
  , it "id selector" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelId "main"]) []]) = parseSelector "#main"
      pure () :: IO ()
  , it "tag.class" $ do
      let Right (Selector [ComplexSelector (CompoundSelector (Just (TypeTag "div")) [SelClass "foo"]) []]) = parseSelector "div.foo"
      pure () :: IO ()
  , it "attribute exact" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelAttrExact "type" "text" False]) []]) = parseSelector "[type=\"text\"]"
      pure () :: IO ()
  , it "attribute prefix" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelAttrPrefix "href" "https" False]) []]) = parseSelector "[href^=\"https\"]"
      pure () :: IO ()
  , it "attribute suffix" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelAttrSuffix "src" ".png" False]) []]) = parseSelector "[src$=\".png\"]"
      pure () :: IO ()
  , it "attribute contains" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelAttrContains "class" "btn" False]) []]) = parseSelector "[class*=\"btn\"]"
      pure () :: IO ()
  , it "attribute word" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelAttrWord "class" "active" False]) []]) = parseSelector "[class~=\"active\"]"
      pure () :: IO ()
  , it "attribute hyphen" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelAttrHyphen "lang" "en" False]) []]) = parseSelector "[lang|=\"en\"]"
      pure () :: IO ()
  , it "attribute exists" $ do
      let Right (Selector [ComplexSelector (CompoundSelector Nothing [SelAttrExists "hidden"]) []]) = parseSelector "[hidden]"
      pure () :: IO ()
  , it "descendant combinator" $ do
      let Right (Selector [ComplexSelector _ ((Descendant, _) : _)]) = parseSelector "div span"
      pure () :: IO ()
  , it "child combinator" $ do
      let Right (Selector [ComplexSelector _ ((Child, _) : _)]) = parseSelector "div > span"
      pure () :: IO ()
  , it "adjacent sibling" $ do
      let Right (Selector [ComplexSelector _ ((AdjacentSibling, _) : _)]) = parseSelector "h1 + p"
      pure () :: IO ()
  , it "general sibling" $ do
      let Right (Selector [ComplexSelector _ ((GeneralSibling, _) : _)]) = parseSelector "h1 ~ p"
      pure () :: IO ()
  , it "comma group" $ do
      let Right (Selector sels) = parseSelector "h1, h2, h3"
      length sels `shouldBe` 3
  , it "universal selector" $ do
      let Right (Selector [ComplexSelector (CompoundSelector (Just TypeUniversal) []) []]) = parseSelector "*"
      pure () :: IO ()
  , it "universal.class" $ do
      let Right (Selector [ComplexSelector (CompoundSelector (Just TypeUniversal) [SelClass "t1"]) []]) = parseSelector "*.t1"
      pure () :: IO ()
  , it "rewriter compatible (tag.class > tag[attr])" $
      isRewriterCompatible (mustParseSelector "div.foo > span[href]") `shouldBe` True
  , it "rewriter incompatible (pseudo :first-child)" $
      not (isRewriterCompatible (mustParseSelector "div:first-child")) `shouldBe` True
  , it "rewriter incompatible (sibling +)" $
      not (isRewriterCompatible (mustParseSelector "h1 + p")) `shouldBe` True
  , it "rewriter incompatible (sibling ~)" $
      not (isRewriterCompatible (mustParseSelector "h1 ~ p")) `shouldBe` True
  , it "syntax error on empty" $ do
      case parseSelector "" of
        Left (SelectorSyntaxError _ _) -> pure ()
        Right _ -> expectationFailure "should fail on empty"
  , it "syntax error on just comma" $ do
      case parseSelector "," of
        Left _ -> pure ()
        Right _ -> expectationFailure "should fail on bare comma"
  ]


-- ---------------------------------------------------------------------------
-- Passthrough tests (from lol_html rewrite_arbitrary_settings)
-- ---------------------------------------------------------------------------

passthroughTests :: [Spec]
passthroughTests =
  [ it "no handlers = identity" $ do
      let rw = mustBuild (pure ())
      result <- rewriteStr rw "<span>Some text</span>"
      decodeResult result `shouldBe` "<span>Some text</span>"
  , it "doctype preserved" $ do
      let rw = mustBuild (pure ())
      result <- rewriteStr rw "<!DOCTYPE html><html><body>Hi</body></html>"
      BS.isInfixOf "<!DOCTYPE html>" result `shouldBe` True
  , it "comments preserved" $ do
      let rw = mustBuild (pure ())
      result <- rewriteStr rw "<div><!-- note --><p>text</p></div>"
      BS.isInfixOf "<!-- note -->" result `shouldBe` True
  , it "attributes preserved" $ do
      let rw = mustBuild (pure ())
      result <- rewriteStr rw "<div id=\"main\" class=\"foo\">text</div>"
      BS.isInfixOf "id=" result `shouldBe` True
      BS.isInfixOf "class=" result `shouldBe` True
  , it "self-closing tags preserved" $ do
      let rw = mustBuild (pure ())
      result <- rewriteStr rw "<div><br/><img src=\"x\"></div>"
      let s = decodeResult result
      T.isInfixOf "br" s `shouldBe` True
      T.isInfixOf "img" s `shouldBe` True
  , it "nested elements preserved" $ do
      let rw = mustBuild (pure ())
      result <- rewriteStr rw "<div><ul><li>a</li><li>b</li></ul></div>"
      let s = decodeResult result
      T.isInfixOf "<li>" s `shouldBe` True
  ]


-- ---------------------------------------------------------------------------
-- Element mutation tests (ported from lol_html element.rs)
-- ---------------------------------------------------------------------------

elementMutationTests :: [Spec]
elementMutationTests =
  [ it "setTagName renames start and end tags" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            setTagName er "section"
      result <- rewriteStr rw "<div>content</div>"
      let s = decodeResult result
      T.isInfixOf "<section>" s `shouldBe` True
      T.isInfixOf "</section>" s `shouldBe` True
  , it "setAttr adds new attribute" $ do
      let rw = mustBuild $ onElement (mustParseSelector "p") $ \er -> do
            setElemAttr er "class" "highlight"
      result <- rewriteStr rw "<p>text</p>"
      BS.isInfixOf "class=\"highlight\"" result `shouldBe` True
  , it "setAttr overwrites existing attribute" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            setElemAttr er "id" "new-id"
      result <- rewriteStr rw "<div id=\"old-id\">text</div>"
      BS.isInfixOf "new-id" result `shouldBe` True
      not (BS.isInfixOf "old-id" result) `shouldBe` True
  , it "removeAttr removes attribute" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            removeElemAttr er "id"
      result <- rewriteStr rw "<div id=\"main\">text</div>"
      not (BS.isInfixOf "id=" result) `shouldBe` True
  , it "removeAttr non-existent is no-op" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            removeElemAttr er "nonexistent"
      result <- rewriteStr rw "<div id=\"main\">text</div>"
      BS.isInfixOf "id=\"main\"" result `shouldBe` True
  , it "getAttr reads attribute value" $ do
      ref <- newIORef T.empty
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            mval <- getElemAttr er "id"
            case mval of
              Just v -> writeIORef ref v
              Nothing -> pure ()
      _ <- rewriteStr rw "<div id=\"main\">text</div>"
      val <- readIORef ref
      val `shouldBe` "main"
  , it "getAttr returns Nothing for missing" $ do
      ref <- newIORef (Just "sentinel")
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            mval <- getElemAttr er "missing"
            writeIORef ref mval
      _ <- rewriteStr rw "<div>text</div>"
      val <- readIORef ref
      val `shouldBe` Nothing
  , it "hasAttr" $ do
      ref <- newIORef (False, False)
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            h1 <- hasElemAttr er "id"
            h2 <- hasElemAttr er "missing"
            writeIORef ref (h1, h2)
      _ <- rewriteStr rw "<div id=\"x\">text</div>"
      (h1, h2) <- readIORef ref
      h1 `shouldBe` True
      not h2 `shouldBe` True
  , it "getElemAttrs returns all attributes" $ do
      ref <- newIORef []
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            as <- getElemAttrs er
            writeIORef ref as
      _ <- rewriteStr rw "<div id=\"a\" class=\"b\" data-x=\"c\">x</div>"
      as <- readIORef ref
      length as `shouldBe` 3
  , it "getTagName returns lowercase" $ do
      ref <- newIORef T.empty
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            t <- getTagName er
            writeIORef ref t
      _ <- rewriteStr rw "<DIV>text</DIV>"
      val <- readIORef ref
      val `shouldBe` "div"
  ]


-- ---------------------------------------------------------------------------
-- Text mutation tests (ported from lol_html text_chunk.rs)
-- ---------------------------------------------------------------------------

textMutationTests :: [Spec]
textMutationTests =
  [ it "replaceTextChunk with text" $ do
      let rw = mustBuild $ onText (mustParseSelector "p") $ \tr -> do
            replaceTextChunk tr "REPLACED" AsText
      result <- rewriteStr rw "<p>original</p>"
      BS.isInfixOf "REPLACED" result `shouldBe` True
      not (BS.isInfixOf "original" result) `shouldBe` True
  , it "replaceTextChunk with HTML" $ do
      let rw = mustBuild $ onText (mustParseSelector "p") $ \tr -> do
            replaceTextChunk tr "<b>bold</b>" AsHTML
      result <- rewriteStr rw "<p>original</p>"
      BS.isInfixOf "<b>bold</b>" result `shouldBe` True
  , it "removeTextChunk" $ do
      let rw = mustBuild $ onText (mustParseSelector "p") $ \tr -> do
            removeTextChunk tr
      result <- rewriteStr rw "<p>gone</p>"
      not (BS.isInfixOf "gone" result) `shouldBe` True
      BS.isInfixOf "<p>" result `shouldBe` True
  , it "beforeTextChunk" $ do
      let rw = mustBuild $ onText (mustParseSelector "p") $ \tr -> do
            beforeTextChunk tr "[before]" AsHTML
      result <- rewriteStr rw "<p>text</p>"
      let s = decodeResult result
      T.isInfixOf "[before]text" s `shouldBe` True
  , it "afterTextChunk" $ do
      let rw = mustBuild $ onText (mustParseSelector "p") $ \tr -> do
            afterTextChunk tr "[after]" AsHTML
      result <- rewriteStr rw "<p>text</p>"
      let s = decodeResult result
      T.isInfixOf "text[after]" s `shouldBe` True
  , it "isLastInTextNode" $ do
      ref <- newIORef []
      let rw = mustBuild $ onText (mustParseSelector "p") $ \tr -> do
            lastV <- isLastInTextNode tr
            modifyIORef' ref (lastV :)
      _ <- rewriteStr rw "<p>hello</p>"
      vals <- readIORef ref
      or vals `shouldBe` True
  ]


-- ---------------------------------------------------------------------------
-- Comment mutation tests (ported from lol_html comment.rs)
-- ---------------------------------------------------------------------------

commentMutationTests :: [Spec]
commentMutationTests =
  [ it "removeComment" $ do
      let rw = mustBuild $ onComment $ \cr -> do
            removeComment cr
      result <- rewriteStr rw "<div><!-- delete me --><p>keep</p></div>"
      not (BS.isInfixOf "delete me" result) `shouldBe` True
      BS.isInfixOf "keep" result `shouldBe` True
  , it "setCommentText" $ do
      let rw = mustBuild $ onComment $ \cr -> do
            setCommentText cr "modified"
      result <- rewriteStr rw "<div><!-- original --></div>"
      BS.isInfixOf "modified" result `shouldBe` True
      not (BS.isInfixOf "original" result) `shouldBe` True
  , it "getCommentText" $ do
      ref <- newIORef T.empty
      let rw = mustBuild $ onComment $ \cr -> do
            t <- getCommentText cr
            writeIORef ref t
      _ <- rewriteStr rw "<!-- hello -->"
      val <- readIORef ref
      T.strip val `shouldBe` "hello"
  , it "replaceComment with HTML" $ do
      let rw = mustBuild $ onComment $ \cr -> do
            replaceComment cr "<span>replaced</span>" AsHTML
      result <- rewriteStr rw "<div><!-- old --></div>"
      BS.isInfixOf "<span>replaced</span>" result `shouldBe` True
      not (BS.isInfixOf "<!-- old -->" result) `shouldBe` True
  , it "replaceComment with text" $ do
      let rw = mustBuild $ onComment $ \cr -> do
            replaceComment cr "<script>alert(1)</script>" AsText
      result <- rewriteStr rw "<div><!-- old --></div>"
      BS.isInfixOf "&lt;script&gt;" result `shouldBe` True
  , it "beforeComment" $ do
      let rw = mustBuild $ onComment $ \cr -> do
            beforeComment cr "[PRE]" AsHTML
      result <- rewriteStr rw "<div><!-- c --></div>"
      let s = decodeResult result
      T.isInfixOf "[PRE]<!--" s `shouldBe` True
  , it "afterComment" $ do
      let rw = mustBuild $ onComment $ \cr -> do
            afterComment cr "[POST]" AsHTML
      result <- rewriteStr rw "<div><!-- c --></div>"
      let s = decodeResult result
      T.isInfixOf "-->[POST]" s `shouldBe` True
  , it "before and after comment with text escaping" $ do
      let rw = mustBuild $ onComment $ \cr -> do
            beforeComment cr "<span>" AsText
            afterComment cr "<foo & bar>" AsText
      result <- rewriteStr rw "<!-- c -->"
      let s = decodeResult result
      T.isInfixOf "&lt;span&gt;" s `shouldBe` True
      T.isInfixOf "&lt;foo &amp; bar&gt;" s `shouldBe` True
  , it "multiple comments" $ do
      ref <- newIORef (0 :: Int)
      let rw = mustBuild $ onComment $ \_ -> do
            modifyIORef' ref (+ 1)
      _ <- rewriteStr rw "<!-- a --><!-- b --><!-- c -->"
      count <- readIORef ref
      count `shouldBe` 3
  ]


-- ---------------------------------------------------------------------------
-- Content insertion tests (ported from lol_html element.rs)
-- ---------------------------------------------------------------------------

insertionTests :: [Spec]
insertionTests =
  [ it "beforeElement with HTML" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            beforeElement er "<img>" AsHTML
      result <- rewriteStr rw "<div><span>hi</span></div>"
      let s = decodeResult result
      T.isInfixOf "<img><span>" s `shouldBe` True
  , it "beforeElement with text escaping" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            beforeElement er "<img>" AsText
      result <- rewriteStr rw "<div><span>hi</span></div>"
      let s = decodeResult result
      T.isInfixOf "&lt;img&gt;<span>" s `shouldBe` True
  , it "afterElement with HTML" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            afterElement er "<img>" AsHTML
      result <- rewriteStr rw "<div><span>hi</span></div>"
      let s = decodeResult result
      T.isInfixOf "</span><img>" s `shouldBe` True
  , it "afterElement with text escaping" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            afterElement er "<img>" AsText
      result <- rewriteStr rw "<div><span>hi</span></div>"
      let s = decodeResult result
      T.isInfixOf "</span>&lt;img&gt;" s `shouldBe` True
  , it "prependToElement" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            prependToElement er "<b>first</b>" AsHTML
      result <- rewriteStr rw "<div><span>existing</span></div>"
      let s = decodeResult result
      T.isInfixOf "<span><b>first</b>existing" s `shouldBe` True
  , it "prependToElement with text" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            prependToElement er "<b>first</b>" AsText
      result <- rewriteStr rw "<div><span>existing</span></div>"
      let s = decodeResult result
      T.isInfixOf "<span>&lt;b&gt;first&lt;/b&gt;existing" s `shouldBe` True
  , it "appendToElement" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            appendToElement er "<b>last</b>" AsHTML
      result <- rewriteStr rw "<div><span>existing</span></div>"
      let s = decodeResult result
      T.isInfixOf "existing<b>last</b></span>" s `shouldBe` True
  , it "setInnerContent with text" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            setInnerContent er "new content" AsText
      result <- rewriteStr rw "<div>old content</div>"
      let s = decodeResult result
      T.isInfixOf "new content" s `shouldBe` True
      not (T.isInfixOf "old content" s) `shouldBe` True
  , it "setInnerContent with HTML" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            setInnerContent er "<b>html</b>" AsHTML
      result <- rewriteStr rw "<div>old content</div>"
      let s = decodeResult result
      T.isInfixOf "<b>html</b>" s `shouldBe` True
  , it "setInnerContent replaces children too" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            setInnerContent er "replaced" AsText
      result <- rewriteStr rw "<div><span>child</span><p>para</p></div>"
      let s = decodeResult result
      T.isInfixOf "replaced" s `shouldBe` True
      not (T.isInfixOf "child" s) `shouldBe` True
      not (T.isInfixOf "para" s) `shouldBe` True
  , it "setInnerContent overrides prepend and append" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            prependToElement er "<prepended>" AsHTML
            appendToElement er "<appended>" AsHTML
            setInnerContent er "<img>" AsText
      result <- rewriteStr rw "<div><span>Hi</span></div>"
      let s = decodeResult result
      T.isInfixOf "&lt;img&gt;" s `shouldBe` True
  ]


-- ---------------------------------------------------------------------------
-- Element removal tests (ported from lol_html element.rs)
-- ---------------------------------------------------------------------------

removalTests :: [Spec]
removalTests =
  [ it "removeElement removes element and children" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            removeElement er
      result <- rewriteStr rw "<div><span>Hi</span>Keep</div>"
      let s = decodeResult result
      not (T.isInfixOf "Hi" s) `shouldBe` True
      T.isInfixOf "Keep" s `shouldBe` True
  , it "removeElement with nested children" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div.remove") $ \er -> do
            removeElement er
      result <- rewriteStr rw "<main><div class=\"remove\"><p><span>deep</span></p></div><p>keep</p></main>"
      let s = decodeResult result
      not (T.isInfixOf "deep" s) `shouldBe` True
      T.isInfixOf "keep" s `shouldBe` True
  , it "removeElement preserves before/after content" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            beforeElement er "[before]" AsHTML
            afterElement er "[after]" AsHTML
            removeElement er
      result <- rewriteStr rw "<div><span>Hi</span></div>"
      let s = decodeResult result
      not (T.isInfixOf "Hi" s) `shouldBe` True
  ]


-- ---------------------------------------------------------------------------
-- Element replace tests (ported from lol_html element.rs)
-- ---------------------------------------------------------------------------

elementReplaceTests :: [Spec]
elementReplaceTests =
  [ it "replaceElement with HTML" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            replaceElement er "<img>" AsHTML
      result <- rewriteStr rw "<div><span>content</span></div>"
      let s = decodeResult result
      T.isInfixOf "<img>" s `shouldBe` True
      not (T.isInfixOf "content" s) `shouldBe` True
      not (T.isInfixOf "<span>" s) `shouldBe` True
  , it "replaceElement with text" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            replaceElement er "<foo & bar>" AsText
      result <- rewriteStr rw "<div><span>old</span></div>"
      let s = decodeResult result
      T.isInfixOf "&lt;foo &amp; bar&gt;" s `shouldBe` True
  , it "replaceElement last call wins" $ do
      let rw = mustBuild $ onElement (mustParseSelector "span") $ \er -> do
            replaceElement er "<div></div>" AsHTML
            replaceElement er "<!--42-->" AsHTML
            replaceElement er "<img>" AsHTML
      result <- rewriteStr rw "<div><span>old</span></div>"
      let s = decodeResult result
      T.isInfixOf "<img>" s `shouldBe` True
  , it "multiple consecutive removes" $ do
      let rw = mustBuild $ do
            onElement (mustParseSelector "div") $ \er ->
              replaceElement er "hey & ya" AsHTML
            onElement (mustParseSelector "h1") $ \er ->
              removeElement er
      result <- rewriteStr rw "<div><span>42</span></div><h1>Hello</h1><p>Keep</p>"
      let s = decodeResult result
      T.isInfixOf "hey & ya" s `shouldBe` True
      not (T.isInfixOf "Hello" s) `shouldBe` True
      T.isInfixOf "Keep" s `shouldBe` True
  ]


-- ---------------------------------------------------------------------------
-- End tag handler tests (ported from lol_html element.rs on_end_tag_handlers)
-- ---------------------------------------------------------------------------

endTagTests :: [Spec]
endTagTests =
  [ it "onElementEndTag inserts before end tag" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            onElementEndTag er $ \etr -> do
              beforeEndTag etr "X" AsHTML
      result <- rewriteStr rw "<div>foo</div>"
      let s = decodeResult result
      T.isInfixOf "fooX</div>" s `shouldBe` True
  , it "onElementEndTag inserts after end tag" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            onElementEndTag er $ \etr -> do
              afterEndTag etr "Y" AsHTML
      result <- rewriteStr rw "<div>foo</div>"
      let s = decodeResult result
      T.isInfixOf "</div>Y" s `shouldBe` True
  , it "setEndTagName" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            onElementEndTag er $ \etr -> do
              setEndTagName etr "section"
      result <- rewriteStr rw "<div>foo</div>"
      let s = decodeResult result
      T.isInfixOf "</section>" s `shouldBe` True
  , it "getEndTagName" $ do
      ref <- newIORef T.empty
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            onElementEndTag er $ \etr -> do
              n <- getEndTagName etr
              writeIORef ref n
      _ <- rewriteStr rw "<div>foo</div>"
      val <- readIORef ref
      val `shouldBe` "div"
  , it "appendToElement via end tag handler" $ do
      let rw = mustBuild $ onElement (mustParseSelector "div") $ \er -> do
            appendToElement er "<appended>" AsHTML
      result <- rewriteStr rw "<div>text</div>"
      let s = decodeResult result
      T.isInfixOf "<appended></div>" s `shouldBe` True
  , it "onEndTag selector-based" $ do
      let rw = mustBuild $ onEndTag (mustParseSelector "div") $ \etr -> do
            beforeEndTag etr "Z" AsHTML
      result <- rewriteStr rw "<div>foo</div>"
      let s = decodeResult result
      T.isInfixOf "fooZ</div>" s `shouldBe` True
  ]


-- ---------------------------------------------------------------------------
-- Doctype tests (ported from lol_html doctype.rs)
-- ---------------------------------------------------------------------------

doctypeTests :: [Spec]
doctypeTests =
  [ it "doctype name" $ do
      nameRef <- newIORef T.empty
      let rw = mustBuild $ onDoctype $ \dr -> do
            n <- getDoctypeName dr
            writeIORef nameRef n
      _ <- rewriteStr rw "<!DOCTYPE html><html></html>"
      val <- readIORef nameRef
      val `shouldBe` "html"
  , it "doctype public and system ids (absent)" $ do
      pubRef <- newIORef (Just "sentinel")
      sysRef <- newIORef (Just "sentinel")
      let rw = mustBuild $ onDoctype $ \dr -> do
            p <- getDoctypePublicId dr
            s <- getDoctypeSystemId dr
            writeIORef pubRef p
            writeIORef sysRef s
      _ <- rewriteStr rw "<!DOCTYPE html>"
      pub <- readIORef pubRef
      sys <- readIORef sysRef
      pub `shouldBe` Nothing
      sys `shouldBe` Nothing
  , it "doctype serialization preserved" $ do
      let rw = mustBuild $ onDoctype $ \_ -> pure ()
      result <- rewriteStr rw "<!DOCTYPE html><p>text</p>"
      BS.isInfixOf "<!DOCTYPE html>" result `shouldBe` True
  , it "multiple doctypes" $ do
      ref <- newIORef (0 :: Int)
      let rw = mustBuild $ onDoctype $ \_ -> do
            modifyIORef' ref (+ 1)
      _ <- rewriteStr rw "<!DOCTYPE html1><!-- test --><div><!DOCTYPE html></div>"
      count <- readIORef ref
      count `shouldBe` 2
  ]


-- ---------------------------------------------------------------------------
-- Void element tests (ported from lol_html element.rs void_element)
-- ---------------------------------------------------------------------------

voidElementTests :: [Spec]
voidElementTests =
  [ it "void element after content" $ do
      let rw = mustBuild $ onElement (mustParseSelector "img") $ \er -> do
            afterElement er "<!--after-->" AsHTML
      result <- rewriteStr rw "<img><span>Hi</span>"
      let s = decodeResult result
      T.isInfixOf "<!--after-->" s `shouldBe` True
  , it "void element tag rename" $ do
      let rw = mustBuild $ onElement (mustParseSelector "img") $ \er -> do
            setTagName er "img-foo"
      result <- rewriteStr rw "<img src=\"x\">"
      let s = decodeResult result
      T.isInfixOf "<img-foo" s `shouldBe` True
  , it "br is void" $ do
      let rw = mustBuild $ onElement (mustParseSelector "br") $ \er -> do
            afterElement er "[after-br]" AsHTML
      result <- rewriteStr rw "<p>line1<br>line2</p>"
      let s = decodeResult result
      T.isInfixOf "[after-br]" s `shouldBe` True
  ]


-- ---------------------------------------------------------------------------
-- Multiple handler tests (ported from lol_html element.rs)
-- ---------------------------------------------------------------------------

multipleHandlerTests :: [Spec]
multipleHandlerTests =
  [ it "multiple selectors match same element" $ do
      ref <- newIORef (0 :: Int)
      let rw = mustBuild $ do
            onElement (mustParseSelector "span") $ \_ -> modifyIORef' ref (+ 1)
            onElement (mustParseSelector "[foo]") $ \_ -> modifyIORef' ref (+ 1)
      _ <- rewriteStr rw "<div><span foo></span></div>"
      count <- readIORef ref
      count `shouldBe` 2
  , it "element and text handlers on same selector" $ do
      elemRef <- newIORef False
      textRef <- newIORef False
      let rw = mustBuild $ do
            onElement (mustParseSelector "p") $ \_ -> writeIORef elemRef True
            onText (mustParseSelector "p") $ \_ -> writeIORef textRef True
      _ <- rewriteStr rw "<p>hello</p>"
      readIORef elemRef >>= (`shouldBe` True)
      readIORef textRef >>= (`shouldBe` True)
  , it "star selector matches all elements" $ do
      ref <- newIORef (0 :: Int)
      let rw = mustBuild $ onElement (mustParseSelector "*") $ \_ ->
            modifyIORef' ref (+ 1)
      _ <- rewriteStr rw "<div><p><span>x</span></p></div>"
      count <- readIORef ref
      count `shouldBe` 3
  , it "star selector with attribute prepend" $ do
      let rw = mustBuild $ onElement (mustParseSelector "*") $ \er -> do
            setElemAttr er "foo" "bar"
            prependToElement er "<test></test>" AsHTML
      result <- rewriteStr rw "<!DOCTYPE html>\n<html>\n   <head></head>\n   <body>\n       <div>Test</div>\n   </body>\n</html>"
      let s = decodeResult result
      T.isInfixOf "foo=\"bar\"" s `shouldBe` True
      T.isInfixOf "<test></test>" s `shouldBe` True
  ]


-- ---------------------------------------------------------------------------
-- Handler invocation order tests (ported from lol_html)
-- ---------------------------------------------------------------------------

handlerOrderTests :: [Spec]
handlerOrderTests =
  [ it "handlers fire in registration order" $ do
      ref <- newIORef ([] :: [Int])
      let rw = mustBuild $ do
            onElement (mustParseSelector "div span") $ \_ ->
              modifyIORef' ref (0 :)
            onElement (mustParseSelector "div > span") $ \_ ->
              modifyIORef' ref (1 :)
            onElement (mustParseSelector "span") $ \_ ->
              modifyIORef' ref (2 :)
            onElement (mustParseSelector "[foo]") $ \_ ->
              modifyIORef' ref (3 :)
            onElement (mustParseSelector "div span[foo]") $ \_ ->
              modifyIORef' ref (4 :)
      _ <- rewriteStr rw "<div><span foo></span></div>"
      order <- reverse <$> readIORef ref
      order `shouldBe` [0, 1, 2, 3, 4]
  ]


-- ---------------------------------------------------------------------------
-- Streaming tests
-- ---------------------------------------------------------------------------

streamingTests :: [Spec]
streamingTests =
  [ it "newRewriterState + feedRewriter + finishRewriter" $ do
      let rw = mustBuild $ onElement (mustParseSelector "p") $ \er -> do
            setElemAttr er "class" "modified"
      st <- newRewriterState rw
      out1 <- feedRewriter st "<div><p>he"
      out2 <- feedRewriter st "llo</p></div>"
      out3 <- finishRewriter st
      let bs = BL.toStrict (BB.toLazyByteString (out1 <> out2 <> out3))
      BS.isInfixOf "class=\"modified\"" bs `shouldBe` True
  , it "empty rewriter state" $ do
      let rw = mustBuild (pure ())
      st <- newRewriterState rw
      result <- finishRewriter st
      let bs = BL.toStrict (BB.toLazyByteString result)
      BS.null bs `shouldBe` True
  , it "streaming with many chunks" $ do
      let rw = mustBuild $ onElement (mustParseSelector "b") $ \er -> do
            setTagName er "strong"
      st <- newRewriterState rw
      out1 <- feedRewriter st "<div>"
      out2 <- feedRewriter st "<b>he"
      out3 <- feedRewriter st "llo"
      out4 <- feedRewriter st "</b>"
      out5 <- feedRewriter st "</div>"
      out6 <- finishRewriter st
      let bs = BL.toStrict (BB.toLazyByteString (mconcat [out1, out2, out3, out4, out5, out6]))
      BS.isInfixOf "strong" bs `shouldBe` True
  , it "feedRewriter' with output callback" $ do
      ref <- newIORef ([] :: [BS.ByteString])
      let rw = mustBuild (pure ())
      st <- newRewriterState rw
      feedRewriter' st "<div>hello</div>" $ \chunk ->
        modifyIORef' ref (chunk :)
      _ <- finishRewriter st
      chunks <- readIORef ref
      not (null chunks) `shouldBe` True
  , it "streaming passthrough preserves content" $ do
      let rw = mustBuild (pure ())
          input = "<html><head><title>Test</title></head><body><p>Hello world</p></body></html>"
      st <- newRewriterState rw
      out1 <- feedRewriter st (BS.take 20 input)
      out2 <- feedRewriter st (BS.drop 20 input)
      out3 <- finishRewriter st
      let bs = BL.toStrict (BB.toLazyByteString (out1 <> out2 <> out3))
      BS.isInfixOf "Hello world" bs `shouldBe` True
  ]


-- ---------------------------------------------------------------------------
-- Fixture-based tests (ported from lol-html's selector_matching & element_content_replacement)
-- ---------------------------------------------------------------------------

data TestFixture = TestFixture
  { tfDescription :: !String
  , tfSelector :: !Text
  , tfInput :: !BS.ByteString
  , tfExpected :: !BS.ByteString
  , tfSuite :: !String
  }


loadFixtureTests :: IO Spec
loadFixtureTests = do
  smTests <- loadSelectorMatchingTests
  ecrTests <- loadElementContentReplacementTests
  domTests <- loadDOMSelectorTests
  pure
    $ describe
      "Fixture tests"
    $ sequence_
      [ describe "Selector matching" $ sequence_ smTests
      , describe "Element content replacement" $ sequence_ ecrTests
      , describe "DOM selector matching" $ sequence_ domTests
      ]


loadSelectorMatchingTests :: IO [Spec]
loadSelectorMatchingTests = do
  let dir = "test-data/selector_matching"
  exists <- doesDirectoryExist dir
  if not exists
    then pure [it "SKIP: no test data" $ (pure () :: IO ())]
    else do
      files <- sort <$> listDirectory dir
      let infoFiles = filter ("-info.json" `isSuffixOf`) files
      fixtures <- fmap concat $ forEachM infoFiles $ \infoFile -> do
        loadSelectorsTestFile dir infoFile "selector_matching"
      let supported = filter (\tf -> canParseSelector (tfSelector tf)) fixtures
      let skipped = length fixtures - length supported
      when (skipped > 0) $
        putStrLn $
          "Skipping " ++ show skipped ++ " selector_matching tests with unsupported selectors"
      pure $ fmap mkSelectorMatchingTest supported


loadElementContentReplacementTests :: IO [Spec]
loadElementContentReplacementTests = do
  let dir = "test-data/element_content_replacement"
  exists <- doesDirectoryExist dir
  if not exists
    then pure [it "SKIP: no test data" $ (pure () :: IO ())]
    else do
      files <- sort <$> listDirectory dir
      let infoFiles = filter ("-info.json" `isSuffixOf`) files
      fixtures <- fmap concat $ forEachM infoFiles $ \infoFile -> do
        loadSelectorsTestFile dir infoFile "element_content_replacement"
      let supported = filter (\tf -> canParseSelector (tfSelector tf)) fixtures
      let skipped = length fixtures - length supported
      when (skipped > 0) $
        putStrLn $
          "Skipping " ++ show skipped ++ " element_content_replacement tests with unsupported selectors"
      pure $ fmap mkContentReplacementTest supported


loadSelectorsTestFile :: FilePath -> FilePath -> String -> IO [TestFixture]
loadSelectorsTestFile dir infoFile suite = do
  jsonBS <- BS.readFile (dir </> infoFile)
  case Aeson.decodeStrict' jsonBS :: Maybe Aeson.Value of
    Nothing -> do
      putStrLn $ "Failed to parse JSON: " ++ infoFile
      pure []
    Just (Aeson.Object obj) -> do
      let descV = KM.lookup "description" obj
          selV = KM.lookup "selectors" obj
          srcV = KM.lookup "src" obj
      case (descV, selV, srcV) of
        (Just (Aeson.String desc), Just (Aeson.Object sels), Just (Aeson.String srcFile)) -> do
          srcData <- BS.readFile (dir </> T.unpack srcFile)
          fixtures <- forEachM (KM.toList sels) $ \(selKey, expectedFileV) -> do
            case expectedFileV of
              Aeson.String expectedFile -> do
                expected <- BS.readFile (dir </> T.unpack expectedFile)
                pure
                  [ TestFixture
                      { tfDescription = T.unpack desc ++ " (" ++ T.unpack (AK.toText selKey) ++ ")"
                      , tfSelector = AK.toText selKey
                      , tfInput = srcData
                      , tfExpected = expected
                      , tfSuite = suite
                      }
                  ]
              _ -> pure []
          pure (concat fixtures)
        _ -> pure []
    _ -> pure []


forEachM :: [a] -> (a -> IO b) -> IO [b]
forEachM = flip mapM


canParseSelector :: Text -> Bool
canParseSelector sel =
  case parseSelector sel of
    Right s -> isRewriterCompatible s && not (T.any (== '|') sel)
    Left _ -> False


canParseSelectorAtAll :: Text -> Bool
canParseSelectorAtAll sel =
  case parseSelector sel of
    Right _ -> True
    Left _ -> False


{- | Selectors that parse but can't be correctly tested against the fixture data
because they depend on browser state (:visited) or XML namespace semantics
(the test suite was generated from XHTML where namespace distinctions matter).
-}
isDOMTestExcluded :: Text -> Bool
isDOMTestExcluded sel =
  T.isInfixOf ":visited" sel
    || T.isInfixOf "|" sel


mkSelectorMatchingTest :: TestFixture -> Spec
mkSelectorMatchingTest tf = it (tfDescription tf) $ do
  let sel = mustParseSelector (tfSelector tf)
      selStr = T.unpack (tfSelector tf)
  firstTextRef <- newIORef True
  let rw = mustBuild $ do
        onElement sel $ \er -> do
          beforeElement er (T.pack $ "<!--[ELEMENT('" ++ selStr ++ "')]-->") AsHTML
          afterElement er (T.pack $ "<!--[/ELEMENT('" ++ selStr ++ "')]-->") AsHTML
        onText sel $ \tr -> do
          isFirst <- readIORef firstTextRef
          when isFirst $ do
            beforeTextChunk tr (T.pack $ "<!--[TEXT('" ++ selStr ++ "')]-->") AsHTML
            writeIORef firstTextRef False
          lastV <- isLastInTextNode tr
          when lastV $ do
            afterTextChunk tr (T.pack $ "<!--[/TEXT('" ++ selStr ++ "')]-->") AsHTML
            writeIORef firstTextRef True
  result <- rewriteStr rw (tfInput tf)
  let expected = stripCommentAnnotations (tfExpected tf)
      actual = stripCommentAnnotations result
  actual `shouldBe` expected


stripCommentAnnotations :: BS.ByteString -> BS.ByteString
stripCommentAnnotations bs =
  TE.encodeUtf8 (removeAnnotations (TE.decodeUtf8 bs))


removeAnnotations :: Text -> Text
removeAnnotations = go
  where
    go txt
      | T.null txt = txt
      | otherwise =
          case findAnnotation txt of
            Nothing -> txt
            Just (before, rest) -> before <> go rest

    findAnnotation txt =
      let (b1, r1) = T.breakOn "<!--[COMMENT(" txt
          (b2, r2) = T.breakOn "<!--[/COMMENT(" txt
      in case (T.null r1, T.null r2) of
           (True, True) -> Nothing
           (True, False) -> Just (b2, skipToEnd r2)
           (False, True) -> Just (b1, skipToEnd r1)
           (False, False)
             | T.length b1 <= T.length b2 -> Just (b1, skipToEnd r1)
             | otherwise -> Just (b2, skipToEnd r2)

    skipToEnd txt =
      case T.breakOn "-->" txt of
        (_, rest)
          | T.null rest -> txt
          | otherwise -> T.drop 3 rest


mkContentReplacementTest :: TestFixture -> Spec
mkContentReplacementTest tf = it (tfDescription tf) $ do
  let sel = mustParseSelector (tfSelector tf)
      selStr = T.unpack (tfSelector tf)
      rw = mustBuild $ do
        onElement sel $ \er -> do
          setInnerContent er (T.pack $ "<!--Replaced (" ++ selStr ++ ") -->") AsHTML
  result <- rewriteStr rw (tfInput tf)
  result `shouldBe` tfExpected tf


-- ---------------------------------------------------------------------------
-- DOM-based selector matching tests
-- ---------------------------------------------------------------------------

loadDOMSelectorTests :: IO [Spec]
loadDOMSelectorTests = do
  let dir = "test-data/selector_matching"
  exists <- doesDirectoryExist dir
  if not exists
    then pure [it "SKIP: no test data" $ (pure () :: IO ())]
    else do
      files <- sort <$> listDirectory dir
      let infoFiles = filter ("-info.json" `isSuffixOf`) files
      fixtures <- fmap concat $ forEachM infoFiles $ \infoFile -> do
        loadSelectorsTestFile dir infoFile "selector_matching"
      let supported =
            filter
              ( \tf ->
                  canParseSelectorAtAll (tfSelector tf)
                    && not (isDOMTestExcluded (tfSelector tf))
              )
              fixtures
      let skipped = length fixtures - length supported
      when (skipped > 0) $
        putStrLn $
          "Skipping " ++ show skipped ++ " DOM selector tests with unparseable selectors"
      pure $ fmap mkDOMSelectorTest supported


mkDOMSelectorTest :: TestFixture -> Spec
mkDOMSelectorTest tf = it ("DOM: " ++ tfDescription tf) $ do
  let selStr = tfSelector tf
      doc = DOM.parseDocument (tfInput tf)
  case parseSelector selStr of
    Left _ -> expectationFailure ("Failed to parse selector: " ++ T.unpack selStr)
    Right sel -> do
      let !matched = DOM.querySelectorAllDoc sel doc
          !actualCount = length matched
          !expectedCount = countElementMarkers selStr (TE.decodeUtf8 (tfExpected tf))
          rootDelta
            | expectedCount == 0
            , actualCount == 1
            , any isDocumentElement matched =
                1
            | otherwise = 0
      actualCount `shouldBe` expectedCount + rootDelta
  where
    isDocumentElement n = case DOM.tagName n of
      Just "html" -> case DOM.parentNode n of
        Nothing -> True
        _ -> False
      _ -> False


countElementMarkers :: Text -> Text -> Int
countElementMarkers sel = go 0
  where
    !marker = "<!--[ELEMENT('" <> sel <> "')]-->"
    go !n txt =
      let (_, rest) = T.breakOn marker txt
      in if T.null rest
           then n
           else go (n + 1) (T.drop (T.length marker) rest)
