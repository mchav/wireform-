{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import HTML.DOM
import HTML.Selector qualified as Sel
import HTML.Value (Doctype (..), HTMLAttribute (..), HTMLNode (..), TreeEvent (..))
import Test.Syd hiding (Selector)
import Wireform.Builder qualified as BB


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

simpleDoc :: Document
simpleDoc =
  parseDocument
    "<!DOCTYPE html><html><head><title>Test</title></head>\
    \<body><div id=\"main\" class=\"foo bar\"><p>Hello</p><p>World</p></div></body></html>"


bodyNode :: Node
bodyNode =
  let root = documentElement simpleDoc
  in case childNodes root of
      [_head, body] -> body
      _ -> error "expected <head> and <body>"


mainDiv :: Node
mainDiv = case childNodes bodyNode of
  [d] -> d
  _ -> error "expected single <div> in body"


builderToBS :: BB.Builder -> BS.ByteString
builderToBS = BL.toStrict . BB.toLazyByteString


-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

main :: IO ()
main =
  sydTest $
    describe
      "HTML.DOM" $ sequence_
      [ describe "Parsing" $ sequence_ parsingTests
      , describe "Document access" $ sequence_ docAccessTests
      , describe "Navigation" $ sequence_ navigationTests
      , describe "Node inspection" $ sequence_ inspectionTests
      , describe "Serialization" $ sequence_ serializationTests
      , describe "CSS selectors" $ sequence_ selectorTests
      , describe "CSS selectors (extended)" $ sequence_ extendedSelectorTests
      , describe "Incremental parser" $ sequence_ incrementalTests
      , describe "Streaming tree events" $ sequence_ streamingTests
      ]


parsingTests :: [Spec]
parsingTests =
  [ it "parseDocument produces a document" $ do
      let doc = parseDocument "<p>hi</p>"
          root = documentElement doc
      tagName root `shouldBe` Just "html"
  , it "parseDocument handles doctype" $ do
      let doc = parseDocument "<!DOCTYPE html><html></html>"
      isJust (documentDoctype doc) `shouldBe` True
  ]


docAccessTests :: [Spec]
docAccessTests =
  [ it "documentElement returns root" $ do
      tagName (documentElement simpleDoc) `shouldBe` Just "html"
  , it "documentDoctype returns doctype" $ do
      let mdt = documentDoctype simpleDoc
      isJust mdt `shouldBe` True
  ]


navigationTests :: [Spec]
navigationTests =
  [ it "childNodes of element" $ do
      let root = documentElement simpleDoc
          kids = childNodes root
      length kids `shouldBe` 2
      tagName (head kids) `shouldBe` Just "head"
      tagName (kids !! 1) `shouldBe` Just "body"
  , it "childNodes of text node is empty" $ do
      let p1 = head (childNodes mainDiv)
          textKids = childNodes p1
      length textKids `shouldBe` 1
      childNodes (head textKids) `shouldBe` []
  , it "firstChild / lastChild" $ do
      let fc = firstChild mainDiv
          lc = lastChild mainDiv
      isJust fc `shouldBe` True
      isJust lc `shouldBe` True
      tagName (fromJust fc) `shouldBe` Just "p"
      tagName (fromJust lc) `shouldBe` Just "p"
      textContent (fromJust fc) `shouldBe` "Hello"
      textContent (fromJust lc) `shouldBe` "World"
  , it "firstChild of text node is Nothing" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      firstChild textNode `shouldBe` Nothing
  , it "nextSibling / prevSibling" $ do
      let p1 = fromJust (firstChild mainDiv)
          p2 = fromJust (nextSibling p1)
      textContent p2 `shouldBe` "World"
      let p1' = fromJust (prevSibling p2)
      textContent p1' `shouldBe` "Hello"
  , it "nextSibling of last is Nothing" $ do
      let p2 = fromJust (lastChild mainDiv)
      nextSibling p2 `shouldBe` Nothing
  , it "prevSibling of first is Nothing" $ do
      let p1 = fromJust (firstChild mainDiv)
      prevSibling p1 `shouldBe` Nothing
  , it "parentNode" $ do
      let p1 = fromJust (firstChild mainDiv)
          parent = fromJust (parentNode p1)
      tagName parent `shouldBe` Just "div"
      getAttribute parent "id" `shouldBe` Just "main"
  , it "parentNode of root is Nothing" $ do
      let root = documentElement simpleDoc
      parentNode root `shouldBe` Nothing
  , it "round-trip: parent then child" $ do
      let p1 = fromJust (firstChild mainDiv)
          backToDiv = fromJust (parentNode p1)
          backToP1 = fromJust (firstChild backToDiv)
      textContent backToP1 `shouldBe` "Hello"
  ]


inspectionTests :: [Spec]
inspectionTests =
  [ it "nodeName for elements" $ do
      nodeName mainDiv `shouldBe` "div"
  , it "nodeName for text" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      nodeName textNode `shouldBe` "#text"
  , it "nodeType for elements" $ do
      nodeType mainDiv `shouldBe` ElementNode
  , it "nodeType for text" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      nodeType textNode `shouldBe` TextNode
  , it "textContent" $ do
      textContent mainDiv `shouldBe` "HelloWorld"
  , it "tagName for element" $ do
      tagName mainDiv `shouldBe` Just "div"
  , it "tagName for non-element" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      tagName textNode `shouldBe` Nothing
  , it "getAttribute" $ do
      getAttribute mainDiv "id" `shouldBe` Just "main"
      getAttribute mainDiv "class" `shouldBe` Just "foo bar"
      getAttribute mainDiv "nonexistent" `shouldBe` Nothing
  , it "getAttributes" $ do
      let attrs = getAttributes mainDiv
      length attrs `shouldBe` 2
  , it "hasAttribute" $ do
      hasAttribute mainDiv "id" `shouldBe` True
      not (hasAttribute mainDiv "nope") `shouldBe` True
  , it "classList" $ do
      classList mainDiv `shouldBe` ["foo", "bar"]
  , it "classList empty" $ do
      let p1 = fromJust (firstChild mainDiv)
      classList p1 `shouldBe` []
  , it "rawNode returns HTMLNode" $ do
      case rawNode mainDiv of
        HTMLElement tag _ _ -> tag `shouldBe` "div"
        _ -> expectationFailure "expected HTMLElement"
  ]


serializationTests :: [Spec]
serializationTests =
  [ it "serialize element" $ do
      let bs = builderToBS (serialize mainDiv)
      BS.isInfixOf "<div" bs `shouldBe` True
      BS.isInfixOf "</div>" bs `shouldBe` True
      BS.isInfixOf "Hello" bs `shouldBe` True
  , it "serializeDocument includes doctype" $ do
      let bs = builderToBS (serializeDocument simpleDoc)
      BS.isInfixOf "<!DOCTYPE" bs `shouldBe` True
      BS.isInfixOf "<html>" bs `shouldBe` True
  , it "innerHTML" $ do
      let h = innerHTML mainDiv
      T.isInfixOf "<p>" h `shouldBe` True
      T.isInfixOf "Hello" h `shouldBe` True
      not (T.isInfixOf "<div" h) `shouldBe` True
  , it "outerHTML" $ do
      let h = outerHTML mainDiv
      T.isInfixOf "<div" h `shouldBe` True
      T.isInfixOf "Hello" h `shouldBe` True
  , it "innerHTML of text node" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      innerHTML textNode `shouldBe` ""
  , it "round-trip: serialize -> parse" $ do
      let bs = builderToBS (serializeDocument simpleDoc)
          doc2 = parseDocument bs
          root2 = documentElement doc2
      tagName root2 `shouldBe` Just "html"
      let kids2 = childNodes root2
      length kids2 `shouldBe` 2
  ]


selectorTests :: [Spec]
selectorTests =
  [ it "querySelector by tag" $ do
      let root = documentElement simpleDoc
          result = querySelector root "p"
      isJust result `shouldBe` True
      textContent (fromJust result) `shouldBe` "Hello"
  , it "querySelectorAll by tag" $ do
      let root = documentElement simpleDoc
          results = querySelectorAll root "p"
      length results `shouldBe` 2
  , it "querySelector by class" $ do
      let root = documentElement simpleDoc
          result = querySelector root ".foo"
      isJust result `shouldBe` True
      tagName (fromJust result) `shouldBe` Just "div"
  , it "querySelector by id" $ do
      let root = documentElement simpleDoc
          result = querySelector root "#main"
      isJust result `shouldBe` True
      tagName (fromJust result) `shouldBe` Just "div"
  , it "querySelector tag.class" $ do
      let root = documentElement simpleDoc
          result = querySelector root "div.foo"
      isJust result `shouldBe` True
  , it "querySelector no match" $ do
      let root = documentElement simpleDoc
      querySelector root ".nonexistent" `shouldBe` Nothing
  , it "querySelectorAll wildcard" $ do
      let results = querySelectorAll mainDiv "*"
      length results >= 2 `shouldBe` True
  , it "querySelector descendant combinator" $ do
      let root = documentElement simpleDoc
          result = querySelector root "div p"
      isJust result `shouldBe` True
      textContent (fromJust result) `shouldBe` "Hello"
  , it "selector result preserves navigation context" $ do
      let root = documentElement simpleDoc
          result = fromJust (querySelector root "p")
      isJust (parentNode result) `shouldBe` True
      tagName (fromJust (parentNode result)) `shouldBe` Just "div"
  ]


-- A richer document for testing the full breadth of CSS selectors.
selectorDoc :: Document
selectorDoc =
  parseDocument
    "<!DOCTYPE html>\
    \<html lang=\"en\" dir=\"ltr\">\
    \<head><title>Selector Tests</title></head>\
    \<body>\
    \<div id=\"container\" class=\"wrapper main\">\
    \  <h1>Title</h1>\
    \  <p class=\"intro\">Intro</p>\
    \  <ul>\
    \    <li class=\"item active\">One</li>\
    \    <li class=\"item\">Two</li>\
    \    <li class=\"item\">Three</li>\
    \    <li class=\"item last\">Four</li>\
    \  </ul>\
    \  <form>\
    \    <input type=\"text\" name=\"user\" required=\"\" placeholder=\"Name\">\
    \    <input type=\"email\" name=\"email\" placeholder=\"Email\" value=\"test@example.com\">\
    \    <input type=\"checkbox\" name=\"agree\" checked=\"\">\
    \    <input type=\"radio\" name=\"choice\" value=\"a\">\
    \    <input type=\"hidden\" name=\"token\" value=\"abc\">\
    \    <textarea readonly=\"\">Notes</textarea>\
    \    <select name=\"color\">\
    \      <option>Red</option>\
    \      <option selected=\"\">Blue</option>\
    \    </select>\
    \    <button type=\"submit\">Go</button>\
    \    <input type=\"submit\" value=\"Submit\" disabled=\"\">\
    \  </form>\
    \  <fieldset disabled=\"\">\
    \    <legend><input type=\"text\" name=\"legend-input\" placeholder=\"Legend\"></legend>\
    \    <input type=\"text\" name=\"fs-inner\" placeholder=\"Inside\">\
    \  </fieldset>\
    \  <div class=\"empty\"></div>\
    \  <div class=\"blank\">  \n  </div>\
    \  <div class=\"has-comment\"><!-- comment --></div>\
    \  <span lang=\"fr\">Bonjour</span>\
    \  <span lang=\"en-US\">Hello</span>\
    \  <div dir=\"rtl\"><span>RTL content</span></div>\
    \</div>\
    \</body></html>"


selRoot :: Node
selRoot = documentElement selectorDoc


qsa :: Text -> [Node]
qsa sel = querySelectorAll selRoot sel


qsaLen :: Text -> Int
qsaLen sel = length (qsa sel)


qs :: Text -> Maybe Node
qs sel = querySelector selRoot sel


extendedSelectorTests :: [Spec]
extendedSelectorTests =
  -- === Structural pseudo-classes ===
  [ it ":first-child" $ do
      qsaLen "li:first-child" `shouldBe` 1
      textContent (head (qsa "li:first-child")) `shouldBe` "One"
  , it ":last-child" $ do
      qsaLen "li:last-child" `shouldBe` 1
      textContent (head (qsa "li:last-child")) `shouldBe` "Four"
  , it ":only-child" $ do
      qsaLen "ul:only-child" `shouldBe` 0
      qsaLen "h1:only-child" `shouldBe` 0
  , it ":nth-child(odd)" $ do
      let items = qsa "li:nth-child(odd)"
      length items `shouldBe` 2
      textContent (head items) `shouldBe` "One"
  , it ":nth-child(even)" $ do
      let items = qsa "li:nth-child(even)"
      length items `shouldBe` 2
      textContent (head items) `shouldBe` "Two"
  , it ":nth-child(2n+1)" $ do
      qsaLen "li:nth-child(2n+1)" `shouldBe` 2
  , it ":nth-last-child(1)" $ do
      qsaLen "li:nth-last-child(1)" `shouldBe` 1
      textContent (head (qsa "li:nth-last-child(1)")) `shouldBe` "Four"
  , it ":first-of-type" $ do
      qsaLen "li:first-of-type" `shouldBe` 1
  , it ":last-of-type" $ do
      qsaLen "li:last-of-type" `shouldBe` 1
  , it ":nth-of-type(2)" $ do
      qsaLen "li:nth-of-type(2)" `shouldBe` 1
      textContent (head (qsa "li:nth-of-type(2)")) `shouldBe` "Two"
  , it ":only-of-type" $ do
      qsaLen "h1:only-of-type" `shouldBe` 1
      qsaLen "li:only-of-type" `shouldBe` 0
  , it ":root" $ do
      let Right sel = Sel.parseSelector ":root"
          roots = querySelectorAllDoc sel selectorDoc
      length roots `shouldBe` 1
      tagName (head roots) `shouldBe` Just "html"
  , it ":empty" $ do
      let empties = qsa "div:empty"
      length empties >= 1 `shouldBe` True
  , it ":blank" $ do
      qsaLen "div.blank:blank" `shouldBe` 1
      qsaLen "div.empty:blank" `shouldBe` 1
      qsaLen "div.has-comment:blank" `shouldBe` 1
  , -- === :nth-child(An+B of S) ===
    it ":nth-child(1 of .item)" $ do
      qsaLen "li:nth-child(1 of .item)" `shouldBe` 1
      textContent (head (qsa "li:nth-child(1 of .item)")) `shouldBe` "One"
  , it ":nth-child(2 of .item)" $ do
      qsaLen ":nth-child(2 of .item)" `shouldBe` 1
      textContent (head (qsa ":nth-child(2 of .item)")) `shouldBe` "Two"
  , it ":nth-last-child(1 of .item)" $ do
      qsaLen ":nth-last-child(1 of .item)" `shouldBe` 1
      textContent (head (qsa ":nth-last-child(1 of .item)")) `shouldBe` "Four"
  , -- === Logical pseudo-classes ===
    it ":not(.item)" $ do
      let items = qsa "li:not(.active)"
      length items `shouldBe` 3
  , it ":is(.intro, .item)" $ do
      qsaLen ":is(.intro, .item)" `shouldBe` 5
  , it ":where(.intro)" $ do
      qsaLen ":where(.intro)" `shouldBe` 1
  , it ":is() with invalid branch (forgiving)" $ do
      qsaLen ":is(.intro, ::fake, .item)" `shouldBe` 5
  , it ":has(> li)" $ do
      qsaLen "ul:has(> li)" `shouldBe` 1
  , it ":has(.item)" $ do
      qsaLen "ul:has(.item)" `shouldBe` 1
  , it ":has(+ .last)" $ do
      qsaLen "li:has(+ .last)" `shouldBe` 1
      textContent (head (qsa "li:has(+ .last)")) `shouldBe` "Three"
  , it ":has(~ .last)" $ do
      qsaLen "li.active:has(~ .last)" `shouldBe` 1
  , -- === :scope ===
    it ":scope" $ do
      let Right sel = Sel.parseSelector ":scope"
          scopes = querySelectorAllDoc sel selectorDoc
      length scopes `shouldBe` 1
      tagName (head scopes) `shouldBe` Just "html"
  , -- === :defined ===
    it ":defined matches all elements" $ do
      qsaLen "li:defined" `shouldBe` 4
  , -- === :dir() ===
    it ":dir(ltr)" $ do
      let ltrSpans = qsa "span:dir(ltr)"
      length ltrSpans >= 1 `shouldBe` True
  , it ":dir(rtl)" $ do
      qsaLen "span:dir(rtl)" `shouldBe` 1
  , -- === :lang() ===
    it ":lang(en)" $ do
      let enNodes = qsa ":lang(en)"
      length enNodes >= 1 `shouldBe` True
  , it ":lang(fr)" $ do
      qsaLen "span:lang(fr)" `shouldBe` 1
      textContent (head (qsa "span:lang(fr)")) `shouldBe` "Bonjour"
  , it ":lang(en, fr) multi-argument" $ do
      qsaLen "span:lang(en, fr)" `shouldBe` 3
  , -- === Form pseudo-classes ===
    it ":enabled / :disabled" $ do
      qsaLen "input:enabled" `shouldBe` 6
      qsaLen "input:disabled" `shouldBe` 2
  , it ":checked" $ do
      qsaLen "input:checked" `shouldBe` 1
      qsaLen "option:checked" `shouldBe` 1
  , it ":required / :optional" $ do
      qsaLen "input:required" `shouldBe` 1
      qsaLen "input:optional" `shouldBe` 7
  , it ":read-only / :read-write" $ do
      qsaLen "textarea:read-only" `shouldBe` 1
      qsaLen "input[type=text]:read-write" `shouldBe` 2
  , it ":placeholder-shown" $
      qsaLen "input:placeholder-shown" `shouldBe` 3
  , it ":indeterminate" $ do
      qsaLen "input:indeterminate" `shouldBe` 1
  , it ":default" $ do
      let defaults = qsa ":default"
      length defaults >= 1 `shouldBe` True
  , -- === Fieldset disabled inheritance ===
    it "fieldset disabled inherits to descendants" $ do
      qsaLen "input[name=fs-inner]:disabled" `shouldBe` 1
      qsaLen "input[name=fs-inner]:enabled" `shouldBe` 0
  , it "first legend child exemption" $ do
      qsaLen "input[name=legend-input]:disabled" `shouldBe` 0
      qsaLen "input[name=legend-input]:enabled" `shouldBe` 1
  , it "fieldset itself matches :disabled" $ do
      qsaLen "fieldset:disabled" `shouldBe` 1
  , -- === HTML attribute case-insensitivity ===
    it "[type=text] matches case-insensitively" $ do
      let doc' =
            parseDocument
              "<html><body><input type=\"TEXT\"><input type=\"text\"></body></html>"
          root' = documentElement doc'
      length (querySelectorAll root' "input[type=text]") `shouldBe` 2
  , it "[type=text s] forces case-sensitive" $ do
      let doc' =
            parseDocument
              "<html><body><input type=\"TEXT\"><input type=\"text\"></body></html>"
          root' = documentElement doc'
      length (querySelectorAll root' "input[type=text s]") `shouldBe` 1
  , -- === Dynamic pseudo-classes (always false in static DOM) ===
    it ":hover never matches" $
      qsaLen ":hover" `shouldBe` 0
  , it ":focus never matches" $
      qsaLen ":focus" `shouldBe` 0
  , it ":visited never matches" $
      qsaLen ":visited" `shouldBe` 0
  , -- === querySelector document-order correctness ===
    it "querySelector returns earliest match across comma branches" $ do
      let result = qs "li.last, li.active"
      isJust result `shouldBe` True
      textContent (fromJust result) `shouldBe` "One"
  , -- === Combinators ===
    it "descendant combinator" $
      qsaLen "ul li" `shouldBe` 4
  , it "child combinator" $
      qsaLen "ul > li" `shouldBe` 4
  , it "adjacent sibling" $ do
      qsaLen "h1 + p" `shouldBe` 1
      textContent (head (qsa "h1 + p")) `shouldBe` "Intro"
  , it "general sibling" $
      qsaLen "h1 ~ ul" `shouldBe` 1
  ]


incrementalTests :: [Spec]
incrementalTests =
  [ it "single chunk matches one-shot" $ do
      let input = "<!DOCTYPE html><html><body><p>Hi</p></body></html>"
          oneShot = parseDocument input
      p <- newParser
      feedParser p input
      doc <- finishParser p
      let osRoot = documentElement oneShot
          incRoot = documentElement doc
      tagName osRoot `shouldBe` tagName incRoot
      textContent osRoot `shouldBe` textContent incRoot
  , it "multiple chunks produce same result" $ do
      let full = "<!DOCTYPE html><html><head><title>T</title></head><body><div>Content</div></body></html>"
          oneShot = parseDocument full
      p <- newParser
      feedParser p "<!DOCTYPE html><html><he"
      feedParser p "ad><title>T</title></head>"
      feedParser p "<body><div>Content</div>"
      feedParser p "</body></html>"
      doc <- finishParser p
      let osRoot = documentElement oneShot
          incRoot = documentElement doc
      textContent osRoot `shouldBe` textContent incRoot
      let osKids = childNodes osRoot
          incKids = childNodes incRoot
      length osKids `shouldBe` length incKids
  , it "empty parser produces valid document" $ do
      p <- newParser
      doc <- finishParser p
      let root = documentElement doc
      tagName root `shouldBe` Just "html"
  , it "byte-at-a-time feeding" $ do
      let full = "<p>Hello</p>"
          oneShot = parseDocument full
      p <- newParser
      mapM_ (\b -> feedParser p (BS.singleton b)) (BS.unpack full)
      doc <- finishParser p
      textContent (documentElement doc) `shouldBe` textContent (documentElement oneShot)
  ]


-- ---------------------------------------------------------------------------
-- Streaming tree event tests
-- ---------------------------------------------------------------------------

collectEvents :: IO (Step TreeEvent) -> IO [TreeEvent]
collectEvents act = do
  s <- act
  pure (stepToList s)
  where
    stepToList Done = []
    stepToList (Yield evt rest) = evt : stepToList rest


streamingTests :: [Spec]
streamingTests =
  [ it "streamHTML emits open/close for simple doc" $ do
      evts <- collectEvents (streamHTML "<p>hi</p>")
      let opens = [t | TreeOpen t _ <- evts]
          closes = [t | TreeClose t <- evts]
          texts = [t | TreeText t <- evts]
      ("html" `elem` opens) `shouldBe` True
      ("head" `elem` opens) `shouldBe` True
      ("body" `elem` opens) `shouldBe` True
      ("p" `elem` opens) `shouldBe` True
      ("html" `elem` closes) `shouldBe` True
      ("p" `elem` closes) `shouldBe` True
      ("hi" `elem` texts) `shouldBe` True
  , it "streamHTML emits doctype" $ do
      evts <- collectEvents (streamHTML "<!DOCTYPE html><html><body></body></html>")
      let doctypes = [n | TreeDoctype n _ _ <- evts]
      (not (null doctypes)) `shouldBe` True
  , it "streamHTML events have correct nesting order" $ do
      evts <- collectEvents (streamHTML "<div><span>x</span></div>")
      let relevant = filter isStructural evts
      case dropWhile (not . isOpenTag "div") relevant of
        (TreeOpen "div" _ : rest) ->
          case dropWhile (not . isOpenTag "span") rest of
            (TreeOpen "span" _ : rest2) -> do
              (any isText (takeWhile (not . isCloseTag "span") rest2)) `shouldBe` True
              let afterSpan = dropWhile (not . isCloseTag "span") rest2
              (not (null afterSpan)) `shouldBe` True
              let afterSpanClose = drop 1 afterSpan
              (any (isCloseTag "div") afterSpanClose) `shouldBe` True
            _ -> expectationFailure "no span open after div"
        _ -> expectationFailure "no div open"
  , it "streamHTML with void element emits open+close" $ do
      evts <- collectEvents (streamHTML "<div><br></div>")
      let opens = [t | TreeOpen t _ <- evts]
          closes = [t | TreeClose t <- evts]
      ("br" `elem` opens) `shouldBe` True
      ("br" `elem` closes) `shouldBe` True
  , it "streamHTML preserves attributes" $ do
      evts <- collectEvents (streamHTML "<div class=\"foo\" id=\"bar\">x</div>")
      let divOpens = [() | TreeOpen "div" _ <- evts]
      (not (null divOpens)) `shouldBe` True
  , it "incremental streaming matches one-shot" $ do
      let html = "<html><body><div><p>hello</p><p>world</p></div></body></html>"
      oneShotEvts <- collectEvents (streamHTML html)
      sp <- newStreamParser
      evts1 <- collectEvents (feedChunk sp "<html><body><div>")
      evts2 <- collectEvents (feedChunk sp "<p>hello</p><p>world</p>")
      evts3 <- collectEvents (feedChunk sp "</div></body></html>")
      evtsFinal <- collectEvents (finishStream sp)
      let incrEvts = evts1 ++ evts2 ++ evts3 ++ evtsFinal
      length (filter isOpen incrEvts) `shouldBe` length (filter isOpen oneShotEvts)
      length (filter isClose incrEvts) `shouldBe` length (filter isClose oneShotEvts)
  ]
  where
    isStructural (TreeOpen _ _) = True
    isStructural (TreeClose _) = True
    isStructural (TreeText _) = True
    isStructural _ = False
    isOpenTag n (TreeOpen t _) = t == n
    isOpenTag _ _ = False
    isCloseTag n (TreeClose t) = t == n
    isCloseTag _ _ = False
    isText (TreeText _) = True
    isText _ = False
    isOpen (TreeOpen _ _) = True
    isOpen _ = False
    isClose (TreeClose _) = True
    isClose _ = False
