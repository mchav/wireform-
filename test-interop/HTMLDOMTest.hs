{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import HTML.DOM
import HTML.Value (HTMLNode(..), HTMLAttribute(..), Doctype(..))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

simpleDoc :: Document
simpleDoc = parseDocument
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
main = defaultMain $ testGroup "HTML.DOM"
  [ testGroup "Parsing" parsingTests
  , testGroup "Document access" docAccessTests
  , testGroup "Navigation" navigationTests
  , testGroup "Node inspection" inspectionTests
  , testGroup "Serialization" serializationTests
  , testGroup "CSS selectors" selectorTests
  , testGroup "Incremental parser" incrementalTests
  ]

parsingTests :: [TestTree]
parsingTests =
  [ testCase "parseDocument produces a document" $ do
      let doc = parseDocument "<p>hi</p>"
          root = documentElement doc
      tagName root @?= Just "html"

  , testCase "parseDocument handles doctype" $ do
      let doc = parseDocument "<!DOCTYPE html><html></html>"
      isJust (documentDoctype doc) @? "expected doctype"
  ]

docAccessTests :: [TestTree]
docAccessTests =
  [ testCase "documentElement returns root" $ do
      tagName (documentElement simpleDoc) @?= Just "html"

  , testCase "documentDoctype returns doctype" $ do
      let mdt = documentDoctype simpleDoc
      isJust mdt @? "expected doctype"
  ]

navigationTests :: [TestTree]
navigationTests =
  [ testCase "childNodes of element" $ do
      let root = documentElement simpleDoc
          kids = childNodes root
      length kids @?= 2
      tagName (head kids) @?= Just "head"
      tagName (kids !! 1) @?= Just "body"

  , testCase "childNodes of text node is empty" $ do
      let p1 = head (childNodes mainDiv)
          textKids = childNodes p1
      length textKids @?= 1
      childNodes (head textKids) @?= []

  , testCase "firstChild / lastChild" $ do
      let fc = firstChild mainDiv
          lc = lastChild mainDiv
      isJust fc @? "expected firstChild"
      isJust lc @? "expected lastChild"
      tagName (fromJust fc) @?= Just "p"
      tagName (fromJust lc) @?= Just "p"
      textContent (fromJust fc) @?= "Hello"
      textContent (fromJust lc) @?= "World"

  , testCase "firstChild of text node is Nothing" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      firstChild textNode @?= Nothing

  , testCase "nextSibling / prevSibling" $ do
      let p1 = fromJust (firstChild mainDiv)
          p2 = fromJust (nextSibling p1)
      textContent p2 @?= "World"
      let p1' = fromJust (prevSibling p2)
      textContent p1' @?= "Hello"

  , testCase "nextSibling of last is Nothing" $ do
      let p2 = fromJust (lastChild mainDiv)
      nextSibling p2 @?= Nothing

  , testCase "prevSibling of first is Nothing" $ do
      let p1 = fromJust (firstChild mainDiv)
      prevSibling p1 @?= Nothing

  , testCase "parentNode" $ do
      let p1 = fromJust (firstChild mainDiv)
          parent = fromJust (parentNode p1)
      tagName parent @?= Just "div"
      getAttribute parent "id" @?= Just "main"

  , testCase "parentNode of root is Nothing" $ do
      let root = documentElement simpleDoc
      parentNode root @?= Nothing

  , testCase "round-trip: parent then child" $ do
      let p1 = fromJust (firstChild mainDiv)
          backToDiv = fromJust (parentNode p1)
          backToP1 = fromJust (firstChild backToDiv)
      textContent backToP1 @?= "Hello"
  ]

inspectionTests :: [TestTree]
inspectionTests =
  [ testCase "nodeName for elements" $ do
      nodeName mainDiv @?= "div"

  , testCase "nodeName for text" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      nodeName textNode @?= "#text"

  , testCase "nodeType for elements" $ do
      nodeType mainDiv @?= ElementNode

  , testCase "nodeType for text" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      nodeType textNode @?= TextNode

  , testCase "textContent" $ do
      textContent mainDiv @?= "HelloWorld"

  , testCase "tagName for element" $ do
      tagName mainDiv @?= Just "div"

  , testCase "tagName for non-element" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      tagName textNode @?= Nothing

  , testCase "getAttribute" $ do
      getAttribute mainDiv "id" @?= Just "main"
      getAttribute mainDiv "class" @?= Just "foo bar"
      getAttribute mainDiv "nonexistent" @?= Nothing

  , testCase "getAttributes" $ do
      let attrs = getAttributes mainDiv
      length attrs @?= 2

  , testCase "hasAttribute" $ do
      hasAttribute mainDiv "id" @? "should have id"
      not (hasAttribute mainDiv "nope") @? "should not have nope"

  , testCase "classList" $ do
      classList mainDiv @?= ["foo", "bar"]

  , testCase "classList empty" $ do
      let p1 = fromJust (firstChild mainDiv)
      classList p1 @?= []

  , testCase "rawNode returns HTMLNode" $ do
      case rawNode mainDiv of
        HTMLElement tag _ _ -> tag @?= "div"
        _ -> assertFailure "expected HTMLElement"
  ]

serializationTests :: [TestTree]
serializationTests =
  [ testCase "serialize element" $ do
      let bs = builderToBS (serialize mainDiv)
      BS.isInfixOf "<div" bs @? "should contain <div"
      BS.isInfixOf "</div>" bs @? "should contain </div>"
      BS.isInfixOf "Hello" bs @? "should contain Hello"

  , testCase "serializeDocument includes doctype" $ do
      let bs = builderToBS (serializeDocument simpleDoc)
      BS.isInfixOf "<!DOCTYPE" bs @? "should contain doctype"
      BS.isInfixOf "<html>" bs @? "should contain <html>"

  , testCase "innerHTML" $ do
      let h = innerHTML mainDiv
      T.isInfixOf "<p>" h @? "should contain <p>"
      T.isInfixOf "Hello" h @? "should contain Hello"
      not (T.isInfixOf "<div" h) @? "should not contain <div (inner only)"

  , testCase "outerHTML" $ do
      let h = outerHTML mainDiv
      T.isInfixOf "<div" h @? "should contain <div"
      T.isInfixOf "Hello" h @? "should contain Hello"

  , testCase "innerHTML of text node" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      innerHTML textNode @?= ""

  , testCase "round-trip: serialize -> parse" $ do
      let bs = builderToBS (serializeDocument simpleDoc)
          doc2 = parseDocument bs
          root2 = documentElement doc2
      tagName root2 @?= Just "html"
      let kids2 = childNodes root2
      length kids2 @?= 2
  ]

selectorTests :: [TestTree]
selectorTests =
  [ testCase "querySelector by tag" $ do
      let root = documentElement simpleDoc
          result = querySelector root "p"
      isJust result @? "should find <p>"
      textContent (fromJust result) @?= "Hello"

  , testCase "querySelectorAll by tag" $ do
      let root = documentElement simpleDoc
          results = querySelectorAll root "p"
      length results @?= 2

  , testCase "querySelector by class" $ do
      let root = documentElement simpleDoc
          result = querySelector root ".foo"
      isJust result @? "should find .foo"
      tagName (fromJust result) @?= Just "div"

  , testCase "querySelector by id" $ do
      let root = documentElement simpleDoc
          result = querySelector root "#main"
      isJust result @? "should find #main"
      tagName (fromJust result) @?= Just "div"

  , testCase "querySelector tag.class" $ do
      let root = documentElement simpleDoc
          result = querySelector root "div.foo"
      isJust result @? "should find div.foo"

  , testCase "querySelector no match" $ do
      let root = documentElement simpleDoc
      querySelector root ".nonexistent" @?= Nothing

  , testCase "querySelectorAll wildcard" $ do
      let results = querySelectorAll mainDiv "*"
      length results >= 2 @? "should match at least div + children"

  , testCase "querySelector descendant combinator" $ do
      let root = documentElement simpleDoc
          result = querySelector root "div p"
      isJust result @? "should find div p"
      textContent (fromJust result) @?= "Hello"

  , testCase "selector result preserves navigation context" $ do
      let root = documentElement simpleDoc
          result = fromJust (querySelector root "p")
      isJust (parentNode result) @? "selector result should have parent"
      tagName (fromJust (parentNode result)) @?= Just "div"
  ]

incrementalTests :: [TestTree]
incrementalTests =
  [ testCase "single chunk matches one-shot" $ do
      let input = "<!DOCTYPE html><html><body><p>Hi</p></body></html>"
          oneShot = parseDocument input
      p <- newParser
      feedParser p input
      doc <- finishParser p
      let osRoot = documentElement oneShot
          incRoot = documentElement doc
      tagName osRoot @?= tagName incRoot
      textContent osRoot @?= textContent incRoot

  , testCase "multiple chunks produce same result" $ do
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
      textContent osRoot @?= textContent incRoot
      let osKids = childNodes osRoot
          incKids = childNodes incRoot
      length osKids @?= length incKids

  , testCase "empty parser produces valid document" $ do
      p <- newParser
      doc <- finishParser p
      let root = documentElement doc
      tagName root @?= Just "html"

  , testCase "byte-at-a-time feeding" $ do
      let full = "<p>Hello</p>"
          oneShot = parseDocument full
      p <- newParser
      mapM_ (\b -> feedParser p (BS.singleton b)) (BS.unpack full)
      doc <- finishParser p
      textContent (documentElement doc) @?= textContent (documentElement oneShot)
  ]
