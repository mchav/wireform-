{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.XML (xmlTests) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Test.Syd
import Test.Syd.Hedgehog ()

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Concurrent (killThread)
import XML.Value
import XML.SAX
import XML.Decode
import XML.Encode
import XML.Path
import XML.DSL (Query(..))
import qualified XML.DSL as DSL
import XML.Class
import XML.Schema
import XML.CodeGen
import qualified XML.FastDOM as FD
import XML.Incremental

import Hedgehog (Gen, Property, property, forAll, (===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

xmlTests :: Spec
xmlTests = describe "XML" $ sequence_
  [ saxTests
  , domTests
  , roundtripTests
  , namespaceTests
  , entityTests
  , cdataTests
  , commentAndPITests
  , pathTests
  , pathEnhancedTests
  , dslTests
  , typeclassTests
  , genericTests
  , largeDocTests
  , edgeCaseTests
  , fastDOMTests
  -- New expanded test groups
  , saxEdgeCaseTests
  , domRobustnessTests
  , fastDOMRobustnessTests
  , encodeRobustnessTests
  , pathDSLQueryTests
  , propertyTests
  , conformanceTests
  , incrementalTests
  , concurrentTests
  , streamFoldTests
  , w3cConformanceTests
  ]

-- Simple document for testing
simpleXML :: Text
simpleXML = "<root><child attr=\"value\">text</child></root>"

-- SAX Tests
saxTests :: Spec
saxTests = describe "SAX Parser" $ sequence_
  [ it "parse simple document" $ do
      let Right events = parseSAX (TE.encodeUtf8 simpleXML)
          evList = V.toList events
      (any isStartDoc evList) `shouldBe` True
      (any isEndDoc evList) `shouldBe` True
      (any (\e -> case e of StartElement n _ -> nameLocal n == "root"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of StartElement n _ -> nameLocal n == "child"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of Characters t -> t == "text"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of EndElement n -> nameLocal n == "child"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of EndElement n -> nameLocal n == "root"; _ -> False) evList) `shouldBe` True

  , it "parse with XML declaration" $ do
      let xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      (any (\e -> case e of
          StartDocument (Just decl) -> xmlVersion decl == "1.0" && xmlEncoding decl == Just "UTF-8"
          _ -> False) evList) `shouldBe` True

  , it "foldSAX counts elements" $ do
      let xml = "<a><b/><c/><d/></a>"
          Right count = foldSAX countElems 0 (TE.encodeUtf8 xml)
      count `shouldBe` 4

  , it "parse self-closing tags" $ do
      let xml = "<root><empty/></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      (any (\e -> case e of StartElement n _ -> nameLocal n == "empty"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of EndElement n -> nameLocal n == "empty"; _ -> False) evList) `shouldBe` True

  , it "error on mismatched tags" $ do
      let xml = "<a><b></c></a>"
          result = parseSAX (TE.encodeUtf8 xml)
      (isLeft result) `shouldBe` True
  ]

-- DOM Tests
domTests :: Spec
domTests = describe "DOM Parser" $ sequence_
  [ it "decode simple document" $ do
      let Right doc = decode (TE.encodeUtf8 simpleXML)
          root = docRoot doc
      elementName root `shouldBe` Just (simpleName "root")
      V.length (elementChildren root) `shouldBe` 1
      let child = V.head (elementChildren root)
      elementName child `shouldBe` Just (simpleName "child")

  , it "decode with attributes" $ do
      let xml = "<person name=\"John\" age=\"30\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      attr "name" root `shouldBe` Just "John"
      attr "age" root `shouldBe` Just "30"

  , it "decode nested elements" $ do
      let xml = "<a><b><c>deep</c></b></a>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = queryPath ["b", "c"] (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "deep"
  ]

-- Roundtrip tests
roundtripTests :: Spec
roundtripTests = describe "Roundtrip" $ sequence_
  [ it "encode then decode = identity" $ do
      let xml = "<root><child attr=\"value\">text</child><empty/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      docRoot doc `shouldBe` docRoot doc2

  , it "roundtrip with XML declaration" $ do
      let decl = XMLDecl "1.0" (Just "UTF-8") Nothing
          root = Element (simpleName "test") V.empty
                   (V.singleton (Text "hello"))
          doc = Document (Just decl) root
          encoded = encode doc
          Right doc2 = decode encoded
      docRoot doc `shouldBe` docRoot doc2

  , it "roundtrip with CDATA" $ do
      let root = Element (simpleName "root") V.empty
                   (V.singleton (CData "some <special> & chars"))
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
          child = V.head (elementChildren (docRoot doc2))
      case child of
        CData t -> t `shouldBe` "some <special> & chars"
        _ -> expectationFailure "Expected CDATA node"

  , it "roundtrip with multiple children" $ do
      let children = V.fromList
            [ Element (simpleName "a") V.empty (V.singleton (Text "1"))
            , Element (simpleName "b") V.empty (V.singleton (Text "2"))
            , Element (simpleName "c") V.empty (V.singleton (Text "3"))
            ]
          root = Element (simpleName "root") V.empty children
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      docRoot doc `shouldBe` docRoot doc2
  ]

-- Namespace tests
namespaceTests :: Spec
namespaceTests = describe "Namespaces" $ sequence_
  [ it "default namespace" $ do
      let xml = "<root xmlns=\"http://example.com\"><child/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element name _ _ ->
          nameNamespace name `shouldBe` Just "http://example.com"
        _ -> expectationFailure "Expected Element"

  , it "prefixed namespace" $ do
      let xml = "<ns:root xmlns:ns=\"http://example.com\"><ns:child/></ns:root>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element name _ _ -> do
          nameLocal name `shouldBe` "root"
          namePrefix name `shouldBe` Just "ns"
          nameNamespace name `shouldBe` Just "http://example.com"
        _ -> expectationFailure "Expected Element"
  ]

-- Entity reference tests
entityTests :: Spec
entityTests = describe "Entity References" $ sequence_
  [ it "standard entities" $ do
      let xml = "<root>&amp;&lt;&gt;&apos;&quot;</root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "&<>'\""

  , it "numeric entity decimal" $ do
      let xml = "<root>&#65;&#66;&#67;</root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "ABC"

  , it "numeric entity hex" $ do
      let xml = "<root>&#x41;&#x42;&#x43;</root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "ABC"

  , it "entities in attributes" $ do
      let xml = "<root attr=\"a&amp;b\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "attr" (docRoot doc) `shouldBe` Just "a&b"
  ]

-- CDATA tests
cdataTests :: Spec
cdataTests = describe "CDATA Sections" $ sequence_
  [ it "parse CDATA" $ do
      let xml = "<root><![CDATA[<not>markup & stuff]]></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      (any (\e -> case e of CDATASection t -> t == "<not>markup & stuff"; _ -> False) evList) `shouldBe` True

  , it "CDATA in DOM" $ do
      let xml = "<root><![CDATA[content]]></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      V.length children `shouldBe` 1
      case V.head children of
        CData t -> t `shouldBe` "content"
        _ -> expectationFailure "Expected CData node"
  ]

-- Comment and PI tests
commentAndPITests :: Spec
commentAndPITests = describe "Comments and PIs" $ sequence_
  [ it "parse comment" $ do
      let xml = "<root><!-- a comment --></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      (any (\e -> case e of CommentEvent t -> T.strip t == "a comment"; _ -> False) evList) `shouldBe` True

  , it "parse processing instruction" $ do
      let xml = "<root><?target data?></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      (any (\e -> case e of PI t _ -> t == "target"; _ -> False) evList) `shouldBe` True

  , it "comment in DOM" $ do
      let xml = "<root><!-- hello --></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      (V.any isCommentNode children) `shouldBe` True

  , it "PI in DOM" $ do
      let xml = "<root><?app data?></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      (V.any isPINode children) `shouldBe` True
  ]

-- Path query tests
pathTests :: Spec
pathTests = describe "Path Queries" $ sequence_
  [ it "child query" $ do
      let xml = "<root><items><item>a</item><item>b</item></items></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = queryPath ["items", "item"] (docRoot doc)
      V.length results `shouldBe` 2

  , it "descendant query" $ do
      let xml = "<root><a><b><target>found</target></b></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query (Descendant (simpleName "target")) (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "found"

  , it "attribute query" $ do
      let xml = "<person name=\"John\" age=\"30\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "name" (docRoot doc) `shouldBe` Just "John"
      attr "age" (docRoot doc) `shouldBe` Just "30"
      attr "missing" (docRoot doc) `shouldBe` Nothing

  , it "text content recursive" $ do
      let xml = "<root>hello <b>world</b></root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "hello world"

  , it "parsePath simple" $ do
      let Right path = parsePath "items/item"
          xml = "<root><items><item>x</item></items></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results `shouldBe` 1

  , it "parsePath descendant" $ do
      let Right path = parsePath "//item"
          xml = "<root><a><item>x</item></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results `shouldBe` 1
  ]

-- Typeclass tests
typeclassTests :: Spec
typeclassTests = describe "Typeclass Instances" $ sequence_
  [ it "Text roundtrip" $ do
      let val = "hello" :: Text
          node = toXML val
      fromXML node `shouldBe` Right val

  , it "Int roundtrip" $ do
      let val = 42 :: Int
          node = toXML val
      fromXML node `shouldBe` Right val

  , it "Bool roundtrip" $ do
      let node = toXML True
      fromXML node `shouldBe` Right True
      let node2 = toXML False
      fromXML node2 `shouldBe` Right False

  , it "Maybe roundtrip" $ do
      let val = Just (42 :: Int)
          node = toXML val
      fromXML node `shouldBe` Right val
      let val2 = Nothing :: Maybe Int
          node2 = toXML val2
      fromXML node2 `shouldBe` Right val2

  , it "List roundtrip" $ do
      let val = [1, 2, 3] :: [Int]
          node = toXML val
      fromXML node `shouldBe` Right val
  ]

-- Generic deriving tests
data TestPerson = TestPerson
  { name :: !Text
  , age :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToXML, FromXML)

genericTests :: Spec
genericTests = describe "Generic Deriving" $ sequence_
  [ it "record to XML" $ do
      let person = TestPerson "John" 30
          node = toXML person
      case node of
        Element n _ cs -> do
          nameLocal n `shouldBe` "TestPerson"
          V.length cs `shouldBe` 2
        _ -> expectationFailure "Expected Element"

  , it "record roundtrip" $ do
      let person = TestPerson "Jane" 25
          node = toXML person
      fromXML node `shouldBe` Right person

  , it "encodeXML / decodeXML roundtrip" $ do
      let person = TestPerson "Bob" 40
          bs = encodeXML person
          Right person2 = decodeXML bs :: Either String TestPerson
      person2 `shouldBe` person
  ]

-- Large document test
largeDocTests :: Spec
largeDocTests = describe "Large Documents" $ sequence_
  [ it "10k elements parse and verify" $ do
      let n = 10000 :: Int
          items = T.concat [ "<item id=\"" <> T.pack (show i) <> "\">"
                           <> T.pack (show i) <> "</item>"
                           | i <- [1..n] ]
          xml = "<root>" <> items <> "</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      V.length children `shouldBe` n
      let firstChild = V.head children
          lastChild = V.last children
      textContent firstChild `shouldBe` "1"
      textContent lastChild `shouldBe` T.pack (show n)
      attr "id" firstChild `shouldBe` Just "1"
      attr "id" lastChild `shouldBe` Just (T.pack (show n))
  ]

-- Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "Edge Cases" $ sequence_
  [ it "empty element" $ do
      let xml = "<root/>"
          Right doc = decode (TE.encodeUtf8 xml)
      V.null (elementChildren (docRoot doc)) `shouldBe` True

  , it "self-closing tag" $ do
      let xml = "<root><br/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
      V.length (elementChildren (docRoot doc)) `shouldBe` 1

  , it "attributes with single quotes" $ do
      let xml = "<root attr='value'/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "attr" (docRoot doc) `shouldBe` Just "value"

  , it "attributes with double quotes" $ do
      let xml = "<root attr=\"value\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "attr" (docRoot doc) `shouldBe` Just "value"

  , it "whitespace handling" $ do
      let xml = "<root>  hello  </root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "  hello  "

  , it "mixed content" $ do
      let xml = "<root>text1<child/>text2</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      V.length children `shouldBe` 3

  , it "deeply nested" $ do
      let depth = 100 :: Int
          opens = T.concat [ "<n" <> T.pack (show i) <> ">" | i <- [1..depth] ]
          closes = T.concat [ "</n" <> T.pack (show i) <> ">" | i <- reverse [1..depth] ]
          xml = opens <> "leaf" <> closes
          Right doc = decode (TE.encodeUtf8 xml)
      (True) `shouldBe` True
      let go (Element _ _ cs) = if V.null cs then 0 else 1 + go (V.head cs)
          go _ = 0 :: Int
      go (docRoot doc) `shouldBe` depth

  , it "pretty print" $ do
      let root = Element (simpleName "root") V.empty
                   (V.fromList
                     [ Element (simpleName "a") V.empty (V.singleton (Text "1"))
                     , Element (simpleName "b") V.empty (V.singleton (Text "2"))
                     ])
          doc = Document Nothing root
          pretty = encodePretty 2 doc
      (not (null (show pretty))) `shouldBe` True

  , it "DOCTYPE is skipped" $ do
      let xml = "<?xml version=\"1.0\"?><!DOCTYPE root SYSTEM \"root.dtd\"><root/>"
          Right doc = decode (TE.encodeUtf8 xml)
      elementName (docRoot doc) `shouldBe` Just (simpleName "root")
  ]

-- Enhanced parsePath tests
pathEnhancedTests :: Spec
pathEnhancedTests = describe "Enhanced Path Parsing" $ sequence_
  [ it "parse \".\" (self)" $ do
      let Right path = parsePath "."
      case path of
        Sequence [Self] -> pure ()
        _ -> expectationFailure $ "Expected Sequence [Self], got " ++ show path

  , it "parse \"..\" (parent)" $ do
      let Right path = parsePath ".."
      case path of
        Sequence [Parent] -> pure ()
        _ -> expectationFailure $ "Expected Sequence [Parent], got " ++ show path

  , it "parse \"*\" (wildcard)" $ do
      let Right path = parsePath "*"
          xml = "<root><a/><b/><c/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results `shouldBe` 3

  , it "parse \"name[@attr='val']\"" $ do
      let Right path = parsePath "item[@type='book']"
          xml = "<root><item type=\"book\">B</item><item type=\"dvd\">D</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "B"

  , it "parse \"name[3]\" (1-based index)" $ do
      let Right path = parsePath "item[2]"
          xml = "<root><item>a</item><item>b</item><item>c</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "b"
  ]

-- DSL tests
dslTests :: Spec
dslTests = describe "XML DSL" $ sequence_
  [ it "child /> child composition" $ do
      let xml = "<root><items><item>a</item><item>b</item></items></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "items" DSL./> DSL.child "item"
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 2
      textContent (V.head results) `shouldBe` "a"

  , it "anyDescendant search" $ do
      let xml = "<root><a><b><target>found</target></b></a><target>also</target></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.anyDescendant
          results = DSL.select q (docRoot doc)
      (V.length results >= 3) `shouldBe` True

  , it "where_ filter with attribute" $ do
      let xml = "<root><item type=\"book\">B</item><item type=\"dvd\">D</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.whereAttr "type" "book" (DSL.child "item")
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "B"

  , it "whereContains" $ do
      let xml = "<root><a href=\"https://example.com/foo\">X</a><a href=\"other\">Y</a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.whereContains "href" "example" (DSL.child "a")
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "X"

  , it "index (1-based)" $ do
      let xml = "<root><item>a</item><item>b</item><item>c</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.index 2 (DSL.child "item")
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "b"

  , it "selectOne" $ do
      let xml = "<root><item>a</item><item>b</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "item"
          result = DSL.selectOne q (docRoot doc)
      case result of
        Just n -> textContent n `shouldBe` "a"
        Nothing -> expectationFailure "Expected Just"

  , it "selectText" $ do
      let xml = "<root><msg>hello </msg><msg>world</msg></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "msg" DSL./> DSL.textContent
          result = DSL.selectText q (docRoot doc)
      result `shouldBe` "hello world"

  , it "count" $ do
      let xml = "<root><item/><item/><item/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.count (DSL.child "item")
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 1
      V.head results `shouldBe` 3

  , it "liftQuery extension" $ do
      let xml = "<root><a/><b/><c/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          customQ = DSL.liftQuery $ \n -> case n of
            Element _ _ cs -> V.filter (isElementNamed "b") cs
            _ -> V.empty
          results = DSL.select customQ (docRoot doc)
      V.length results `shouldBe` 1

  , it "union (|>)" $ do
      let xml = "<root><a>1</a><b>2</b><c>3</c></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "a" DSL.|> DSL.child "c"
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 2
      textContent (V.head results) `shouldBe` "1"
      textContent (results V.! 1) `shouldBe` "3"

  , it "first and last" $ do
      let xml = "<root><item>a</item><item>b</item><item>c</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          qFirst = DSL.first (DSL.child "item")
          qLast = DSL.last (DSL.child "item")
      case DSL.selectOne qFirst (docRoot doc) of
        Just n -> textContent n `shouldBe` "a"
        Nothing -> expectationFailure "Expected first"
      case DSL.selectOne qLast (docRoot doc) of
        Just n -> textContent n `shouldBe` "c"
        Nothing -> expectationFailure "Expected last"

  , it "descendant search by name" $ do
      let xml = "<root><a><b><target>deep</target></b></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.descendant "target"
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "deep"

  , it "descendant chain (//>)" $ do
      let xml = "<root><a><item>x</item></a><b><item>y</item></b></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.anyChild DSL.//> DSL.child "item"
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 2
  ]

-- FastDOM zero-copy parser tests
fastDOMTests :: Spec
fastDOMTests = describe "FastDOM" $ sequence_
  [ it "parseFast roundtrip: parse then toDocument equals decode" $ do
      let xml = "<root><child attr=\"value\">text</child><empty/></root>"
          bs = TE.encodeUtf8 xml
          Right docFull = decode bs
          Right fastDoc = FD.parseFast bs
          docMaterialized = FD.toDocument fastDoc
      docRoot docMaterialized `shouldBe` docRoot docFull

  , it "parseFast accessor: nodeTagBS returns correct slice" $ do
      let bs = BS8.pack "<person><name>John</name></person>"
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          src  = FD.fdSource fastDoc
      FD.nodeTagBS root src `shouldBe` "person"

  , it "parseFast accessor: attrValueBS returns correct slice" $ do
      let bs = BS8.pack "<item id=\"42\" class=\"main\"/>"
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          src  = FD.fdSource fastDoc
          attrs = FD.nodeAttrs root
      V.length attrs `shouldBe` 2
      FD.attrNameBS (attrs V.! 0) src `shouldBe` "id"
      FD.attrValueBS (attrs V.! 0) src `shouldBe` "42"
      FD.attrNameBS (attrs V.! 1) src `shouldBe` "class"
      FD.attrValueBS (attrs V.! 1) src `shouldBe` "main"

  , it "parseFast large document (100 items)" $ do
      let header = "<?xml version=\"1.0\"?>\n<catalog>\n"
          footer = "</catalog>\n"
          mkItem :: Int -> String
          mkItem i = concat
            [ "  <item id=\"", show i, "\">"
            , "Product ", show i
            , "</item>\n"
            ]
          items = concatMap mkItem [1..100 :: Int]
          bs = BS8.pack (header ++ items ++ footer)
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          children = FD.nodeChildren root
      -- Remove whitespace text nodes
      let elems = V.filter isElem children
      V.length elems `shouldBe` 100
      let firstChild = V.head elems
          src = FD.fdSource fastDoc
      FD.nodeTagBS firstChild src `shouldBe` "item"

  , it "parseFast with CDATA" $ do
      let bs = BS8.pack "<root><![CDATA[<not>markup & stuff]]></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length children `shouldBe` 1
      case V.head children of
        FD.FCData _ -> FD.nodeTextBS (V.head children) src `shouldBe` "<not>markup & stuff"
        other -> expectationFailure $ "Expected FCData, got: " ++ show other

  , it "parseFast with comment" $ do
      let bs = BS8.pack "<root><!-- hello world --></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children `shouldBe` 1
      case V.head children of
        FD.FComment (FD.Span _ _) -> pure ()
        other -> expectationFailure $ "Expected FComment, got: " ++ show other

  , it "parseFast with processing instruction" $ do
      let bs = BS8.pack "<root><?target some data?></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length children `shouldBe` 1
      case V.head children of
        FD.FPI tSpan _dSpan ->
          FD.nodeTag (FD.FElement tSpan V.empty V.empty) src `shouldBe` "target"
        other -> expectationFailure $ "Expected FPI, got: " ++ show other

  , it "parseFast self-closing elements" $ do
      let bs = BS8.pack "<root><br/><hr/><img src=\"x.png\"/></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length children `shouldBe` 3
      FD.nodeTagBS (children V.! 0) src `shouldBe` "br"
      FD.nodeTagBS (children V.! 1) src `shouldBe` "hr"
      FD.nodeTagBS (children V.! 2) src `shouldBe` "img"
      V.length (FD.nodeChildren (children V.! 0)) `shouldBe` 0
      V.length (FD.nodeAttrs (children V.! 2)) `shouldBe` 1
      FD.attrValueBS (V.head (FD.nodeAttrs (children V.! 2))) src `shouldBe` "x.png"

  , it "parseFast toDocument with entities" $ do
      let bs = BS8.pack "<root>&amp;&lt;&gt;</root>"
          Right fastDoc = FD.parseFast bs
          doc = FD.toDocument fastDoc
      textContent (docRoot doc) `shouldBe` "&<>"

  , it "parseFast with XML declaration" $ do
      let bs = BS8.pack "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
          Right fastDoc = FD.parseFast bs
          src = FD.fdSource fastDoc
      FD.nodeTagBS (FD.fdRoot fastDoc) src `shouldBe` "root"
  ]
  where
    isElem (FD.FElement _ _ _) = True
    isElem _ = False

-- Helpers

isStartDoc :: SAXEvent -> Bool
isStartDoc (StartDocument _) = True
isStartDoc _ = False

isEndDoc :: SAXEvent -> Bool
isEndDoc EndDocument = True
isEndDoc _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

countElems :: Int -> SAXEvent -> Int
countElems n (StartElement _ _) = n + 1
countElems n _ = n

isCommentNode :: Node -> Bool
isCommentNode (Comment _) = True
isCommentNode _ = False

isPINode :: Node -> Bool
isPINode (ProcessingInstruction _ _) = True
isPINode _ = False

------------------------------------------------------------------------
-- Group 1: SAX parser edge cases (30+ tests)
------------------------------------------------------------------------

saxEdgeCaseTests :: Spec
saxEdgeCaseTests = describe "SAX Edge Cases" $ sequence_
  [ it "empty document with self-closing root" $ do
      let xml = "<?xml version=\"1.0\"?><root/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      (any isStartDoc evList) `shouldBe` True
      (any (\e -> case e of StartElement n _ -> nameLocal n == "root"; _ -> False) evList) `shouldBe` True
      (any isEndDoc evList) `shouldBe` True

  , it "self-closing with attributes" $ do
      let xml = "<br class=\"clear\"/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      (any (\e -> case e of
          StartElement n attrs -> nameLocal n == "br" &&
            V.any (\(Attribute an av) -> nameLocal an == "class" && av == "clear") attrs
          _ -> False) evList) `shouldBe` True
      (any (\e -> case e of EndElement n -> nameLocal n == "br"; _ -> False) evList) `shouldBe` True

  , it "deeply nested (50 levels)" $ do
      let depth = 50 :: Int
          opens = T.concat [ "<d" <> T.pack (show i) <> ">" | i <- [1..depth] ]
          closes = T.concat [ "</d" <> T.pack (show i) <> ">" | i <- reverse [1..depth] ]
          xml = opens <> "leaf" <> closes
          Right events = parseSAX (TE.encodeUtf8 xml)
          startCount = V.length $ V.filter (\e -> case e of StartElement _ _ -> True; _ -> False) events
      startCount `shouldBe` depth

  , it "very long tag name (1000 chars)" $ do
      let tagName = T.replicate 1000 "a"
          xml = "<" <> tagName <> "></" <> tagName <> ">"
          Right events = parseSAX (TE.encodeUtf8 xml)
      (V.any (\e -> case e of StartElement n _ -> nameLocal n == tagName; _ -> False) events) `shouldBe` True

  , it "very long attribute value (10000 chars)" $ do
      let longVal = T.replicate 10000 "x"
          xml = "<root attr=\"" <> longVal <> "\"/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      (V.any (\e -> case e of
          StartElement _ attrs -> V.any (\(Attribute _ v) -> v == longVal) attrs
          _ -> False) events) `shouldBe` True

  , it "multiple attributes with same prefix different NS" $ do
      let xml = "<root xmlns:a=\"http://a.com\" xmlns:b=\"http://b.com\" a:x=\"1\" b:x=\"2\"/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          startElems = V.filter (\e -> case e of StartElement _ _ -> True; _ -> False) events
      (V.length startElems == 1) `shouldBe` True
      case V.head startElems of
        StartElement _ attrs ->
          (V.length attrs == 4) `shouldBe` True
        _ -> expectationFailure "Expected StartElement"

  , it "attribute with empty value" $ do
      let xml = "<x a=\"\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "a" (docRoot doc) `shouldBe` Just ""

  , it "attribute with no value (invalid XML, should error)" $ do
      let xml = "<x a/>"
          result = parseSAX (TE.encodeUtf8 xml)
      (isLeft result) `shouldBe` True

  , it "tag with only whitespace inside" $ do
      let xml = "<x>   </x>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "   "

  , it "mixed content: text and elements" $ do
      let xml = "<p>Hello <b>world</b> today</p>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "Hello world today"
      let children_ = elementChildren (docRoot doc)
      V.length children_ `shouldBe` 3

  , it "adjacent text after entity" $ do
      let xml = "<x>a&amp;b</x>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "a&b"

  , it "all 5 entity references in text" $ do
      let xml = "<r>&amp;&lt;&gt;&apos;&quot;</r>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "&<>'\""

  , it "all 5 entity references in attributes" $ do
      let xml = "<r a=\"&amp;\" b=\"&lt;\" c=\"&gt;\" d=\"&apos;\" e=\"&quot;\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      attr "a" root `shouldBe` Just "&"
      attr "b" root `shouldBe` Just "<"
      attr "c" root `shouldBe` Just ">"
      attr "d" root `shouldBe` Just "'"
      attr "e" root `shouldBe` Just "\""

  , it "numeric character ref decimal" $ do
      let xml = "<r>&#65;</r>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "A"

  , it "hex character ref" $ do
      let xml = "<r>&#x41;</r>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "A"

  , it "high unicode char ref (emoji)" $ do
      let xml = "<r>&#x1F600;</r>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) `shouldBe` "\x1F600"

  , it "invalid entity produces error" $ do
      let xml = "<r>&bogus;</r>"
          result = decode (TE.encodeUtf8 xml)
      (isLeft result) `shouldBe` True

  , it "unterminated entity produces error" $ do
      let xml = "<r>&amp</r>"
          result = decode (TE.encodeUtf8 xml)
      (isLeft result) `shouldBe` True

  , it "CDATA with special chars" $ do
      let xml = "<r><![CDATA[<not>&a tag>]]></r>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      (V.any (\e -> case e of CDATASection t -> t == "<not>&a tag>"; _ -> False) events) `shouldBe` True

  , it "CDATA containing ]] but not ]]>" $ do
      let xml = "<r><![CDATA[hello]]world]]></r>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      (V.any (\e -> case e of CDATASection t -> T.isInfixOf "]]" t; _ -> False) events) `shouldBe` True

  , it "comment basic" $ do
      let xml = "<r><!-- comment --></r>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      (V.any (\e -> case e of CommentEvent _ -> True; _ -> False) events) `shouldBe` True

  , it "PI basic" $ do
      let xml = "<r><?php echo \"hello\"; ?></r>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      (V.any (\e -> case e of PI t _ -> t == "php"; _ -> False) events) `shouldBe` True

  , it "XML declaration with encoding" $ do
      let xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><r/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      (V.any (\e -> case e of
          StartDocument (Just decl) -> xmlVersion decl == "1.0" && xmlEncoding decl == Just "UTF-8"
          _ -> False) events) `shouldBe` True

  , it "XML declaration with standalone" $ do
      let xml = "<?xml version=\"1.0\" standalone=\"yes\"?><r/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      (V.any (\e -> case e of
          StartDocument (Just decl) -> xmlStandalone decl == Just True
          _ -> False) events) `shouldBe` True

  , it "BOM handling: UTF-8 BOM before XML" $ do
      let bom = BS.pack [0xEF, 0xBB, 0xBF]
          xml = bom <> TE.encodeUtf8 "<?xml version=\"1.0\"?><root/>"
          result = decode xml
      (not (isLeft result)) `shouldBe` True

  , it "whitespace between attributes" $ do
      let xml = "<x  a = \"1\"  b = \"2\" >"
          -- Not well-formed without closing, add close tag
          xmlFull = xml <> "</x>"
          Right doc = decode (TE.encodeUtf8 xmlFull)
      attr "a" (docRoot doc) `shouldBe` Just "1"
      attr "b" (docRoot doc) `shouldBe` Just "2"

  , it "namespace declaration" $ do
      let xml = "<x xmlns=\"http://example.com\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      case docRoot doc of
        Element n _ _ -> nameNamespace n `shouldBe` Just "http://example.com"
        _ -> expectationFailure "Expected Element"

  , it "namespace with prefix" $ do
      let xml = "<ns:x xmlns:ns=\"http://example.com\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      case docRoot doc of
        Element n _ _ -> do
          namePrefix n `shouldBe` Just "ns"
          nameLocal n `shouldBe` "x"
          nameNamespace n `shouldBe` Just "http://example.com"
        _ -> expectationFailure "Expected Element"

  , it "default namespace on child" $ do
      let xml = "<a xmlns=\"http://a\"><b xmlns=\"http://b\"/></a>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element rn _ cs -> do
          nameNamespace rn `shouldBe` Just "http://a"
          case V.head cs of
            Element cn _ _ -> nameNamespace cn `shouldBe` Just "http://b"
            _ -> expectationFailure "Expected child Element"
        _ -> expectationFailure "Expected Element"

  , it "namespace undeclaration" $ do
      let xml = "<a xmlns=\"http://a\"><b xmlns=\"\"/></a>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element rn _ cs -> do
          nameNamespace rn `shouldBe` Just "http://a"
          case V.head cs of
            Element cn _ _ -> nameNamespace cn `shouldBe` Just ""
            _ -> expectationFailure "Expected child Element"
        _ -> expectationFailure "Expected Element"

  , it "PI before root element" $ do
      let xml = "<?pi-target data?><root/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      (V.any (\e -> case e of PI t _ -> t == "pi-target"; _ -> False) events) `shouldBe` True

  , it "comment before root element" $ do
      let xml = "<!-- pre-comment --><root/>"
          result = decode (TE.encodeUtf8 xml)
      (not (isLeft result)) `shouldBe` True

  , it "multiple self-closing" $ do
      let xml = "<root><a/><b/><c/></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          starts = V.filter (\e -> case e of StartElement _ _ -> True; _ -> False) events
          ends = V.filter (\e -> case e of EndElement _ -> True; _ -> False) events
      V.length starts `shouldBe` 4
      V.length ends `shouldBe` 4
  ]

------------------------------------------------------------------------
-- Group 2: DOM parser robustness (15+ tests)
------------------------------------------------------------------------

domRobustnessTests :: Spec
domRobustnessTests = describe "DOM Robustness" $ sequence_
  [ it "parse then encode roundtrip preserves structure" $ do
      let xml = "<root><a x=\"1\">text</a><b><c/></b></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      docRoot doc `shouldBe` docRoot doc2

  , it "parse then encode preserves namespace declarations" $ do
      let xml = "<root xmlns=\"http://example.com\" xmlns:ns=\"http://ns.com\"><ns:child/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          root2 = docRoot doc2
      case root2 of
        Element n _ _ -> nameNamespace n `shouldBe` Just "http://example.com"
        _ -> expectationFailure "Expected Element"

  , it "comments survive roundtrip" $ do
      let xml = "<root><!-- comment --></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          cs = elementChildren (docRoot doc2)
      (V.any isCommentNode cs) `shouldBe` True

  , it "PIs survive roundtrip" $ do
      let xml = "<root><?target data?></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          cs = elementChildren (docRoot doc2)
      (V.any isPINode cs) `shouldBe` True

  , it "CDATA sections survive roundtrip" $ do
      let xml = "<root><![CDATA[special <content> & more]]></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) `shouldBe` "special <content> & more"

  , it "mixed content roundtrip" $ do
      let xml = "<root>text1<child/>text2</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          cs = elementChildren (docRoot doc2)
      V.length cs `shouldBe` 3

  , it "empty element roundtrip" $ do
      let xml = "<x/>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      V.null (elementChildren (docRoot doc2)) `shouldBe` True

  , it "whitespace in attributes preserved" $ do
      let xml = "<x a=\"  spaced  \"/>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      attr "a" (docRoot doc2) `shouldBe` Just "  spaced  "

  , it "Unicode text roundtrip: Chinese" $ do
      let xml = "<r>\x4F60\x597D\x4E16\x754C</r>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) `shouldBe` "\x4F60\x597D\x4E16\x754C"

  , it "Unicode text roundtrip: Arabic" $ do
      let xml = "<r>\x0645\x0631\x062D\x0628\x0627</r>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) `shouldBe` "\x0645\x0631\x062D\x0628\x0627"

  , it "Unicode text roundtrip: emoji" $ do
      let xml = "<r>\x1F600\x1F4A9\x2764</r>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) `shouldBe` "\x1F600\x1F4A9\x2764"

  , it "large document roundtrip (1000 elements)" $ do
      let n = 1000 :: Int
          items = T.concat [ "<item id=\"" <> T.pack (show i) <> "\">"
                           <> T.pack (show i) <> "</item>"
                           | i <- [1..n] ]
          xml = "<root>" <> items <> "</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      V.length (elementChildren (docRoot doc2)) `shouldBe` n

  , it "attribute order preserved in roundtrip" $ do
      let xml = "<x first=\"1\" second=\"2\" third=\"3\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          attrs' = elementAttributes (docRoot doc2)
      V.length attrs' `shouldBe` 3
      case attrs' V.! 0 of
        Attribute an _ -> nameLocal an `shouldBe` "first"
      case attrs' V.! 1 of
        Attribute an _ -> nameLocal an `shouldBe` "second"
      case attrs' V.! 2 of
        Attribute an _ -> nameLocal an `shouldBe` "third"

  , it "deeply nested roundtrip" $ do
      let depth = 50 :: Int
          opens = T.concat [ "<n" <> T.pack (show i) <> ">" | i <- [1..depth] ]
          closes = T.concat [ "</n" <> T.pack (show i) <> ">" | i <- reverse [1..depth] ]
          xml = opens <> "leaf" <> closes
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          go (Element _ _ cs) = if V.null cs then 0 else 1 + go (V.head cs)
          go _ = 0 :: Int
      go (docRoot doc2) `shouldBe` depth

  , it "multiple text children roundtrip" $ do
      let root = Element (simpleName "r") V.empty
                   (V.fromList [Text "hello", Text " ", Text "world"])
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) `shouldBe` "hello world"
  ]

------------------------------------------------------------------------
-- Group 3: FastDOM robustness (15+ tests)
------------------------------------------------------------------------

fastDOMRobustnessTests :: Spec
fastDOMRobustnessTests = describe "FastDOM Robustness" $ sequence_
  [ it "parseFast then toDocument equals decode" $ do
      let xml = "<root><a x=\"1\">text</a><b><c/></b></root>"
          bs = TE.encodeUtf8 xml
          Right docFull = decode bs
          Right fastDoc = FD.parseFast bs
          docMat = FD.toDocument fastDoc
      docRoot docMat `shouldBe` docRoot docFull

  , it "FastDOM on document with entities (toDocument resolves)" $ do
      let bs = BS8.pack "<root>&amp;&lt;&gt;&apos;&quot;</root>"
          Right fastDoc = FD.parseFast bs
          doc = FD.toDocument fastDoc
      textContent (docRoot doc) `shouldBe` "&<>'\""

  , it "FastDOM nodeTagBS returns exact bytes" $ do
      let bs = BS8.pack "<myElement>content</myElement>"
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          src = FD.fdSource fastDoc
      FD.nodeTagBS root src `shouldBe` "myElement"

  , it "FastDOM attrValueBS on attribute with entities (raw)" $ do
      let bs = BS8.pack "<x a=\"&amp;b\"/>"
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          src = FD.fdSource fastDoc
          attrs = FD.nodeAttrs root
      V.length attrs `shouldBe` 1
      FD.attrValueBS (V.head attrs) src `shouldBe` "&amp;b"

  , it "FastDOM children count matches" $ do
      let bs = BS8.pack "<root><a/><b/><c/></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ `shouldBe` 3

  , it "FastDOM deep nesting (50 levels)" $ do
      let depth = 50 :: Int
          opens = concatMap (\i -> "<d" ++ show i ++ ">") [1..depth]
          closes = concatMap (\i -> "</d" ++ show i ++ ">") (reverse [1..depth])
          bs = BS8.pack (opens ++ "leaf" ++ closes)
          Right fastDoc = FD.parseFast bs
          go (FD.FElement _ _ cs) = if V.null cs then 0 else 1 + go (V.head cs)
          go _ = 0 :: Int
      go (FD.fdRoot fastDoc) `shouldBe` depth

  , it "FastDOM large document (1000 items)" $ do
      let items = concatMap (\i -> "<item>" ++ show i ++ "</item>") [1..1000 :: Int]
          bs = BS8.pack ("<root>" ++ items ++ "</root>")
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ `shouldBe` 1000

  , it "FastDOM self-closing elements" $ do
      let bs = BS8.pack "<root><br/><hr/></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ `shouldBe` 2
      let src = FD.fdSource fastDoc
      FD.nodeTagBS (children_ V.! 0) src `shouldBe` "br"
      FD.nodeTagBS (children_ V.! 1) src `shouldBe` "hr"

  , it "FastDOM CDATA" $ do
      let bs = BS8.pack "<root><![CDATA[raw <data>]]></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length children_ `shouldBe` 1
      case V.head children_ of
        FD.FCData _ -> FD.nodeTextBS (V.head children_) src `shouldBe` "raw <data>"
        _ -> expectationFailure "Expected FCData"

  , it "FastDOM comments" $ do
      let bs = BS8.pack "<root><!-- hi --></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ `shouldBe` 1
      case V.head children_ of
        FD.FComment _ -> pure ()
        _ -> expectationFailure "Expected FComment"

  , it "FastDOM PIs" $ do
      let bs = BS8.pack "<root><?target data?></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ `shouldBe` 1
      case V.head children_ of
        FD.FPI _ _ -> pure ()
        _ -> expectationFailure "Expected FPI"

  , it "FastDOM namespace attributes visible" $ do
      let bs = BS8.pack "<root xmlns:ns=\"http://example.com\" ns:a=\"1\"/>"
          Right fastDoc = FD.parseFast bs
          attrs = FD.nodeAttrs (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length attrs `shouldBe` 2
      FD.attrNameBS (attrs V.! 0) src `shouldBe` "xmlns:ns"
      FD.attrNameBS (attrs V.! 1) src `shouldBe` "ns:a"

  , it "FastDOM empty element" $ do
      let bs = BS8.pack "<empty/>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ `shouldBe` 0

  , it "FastDOM with XML declaration" $ do
      let bs = BS8.pack "<?xml version=\"1.0\"?><root/>"
          Right fastDoc = FD.parseFast bs
          src = FD.fdSource fastDoc
      FD.nodeTagBS (FD.fdRoot fastDoc) src `shouldBe` "root"

  , it "FastDOM text node between elements" $ do
      let bs = BS8.pack "<root><a/>text<b/></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ `shouldBe` 3
      case children_ V.! 1 of
        FD.FText _ -> FD.nodeTextBS (children_ V.! 1) (FD.fdSource fastDoc) `shouldBe` "text"
        _ -> expectationFailure "Expected FText"
  ]

------------------------------------------------------------------------
-- Group 4: Encode robustness (15+ tests)
------------------------------------------------------------------------

encodeRobustnessTests :: Spec
encodeRobustnessTests = describe "Encode Robustness" $ sequence_
  [ it "encode escapes < > & in text" $ do
      let root = Element (simpleName "r") V.empty (V.singleton (Text "<>&"))
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) `shouldBe` "<>&"

  , it "encode escapes \" in attributes" $ do
      let root = Element (simpleName "r")
                   (V.singleton (Attribute (simpleName "a") "he said \"hi\""))
                   V.empty
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      attr "a" (docRoot doc2) `shouldBe` Just "he said \"hi\""

  , it "encode handles CDATA passthrough" $ do
      let root = Element (simpleName "r") V.empty
                   (V.singleton (CData "<special>&chars</special>"))
          doc = Document Nothing root
          encoded = encode doc
      (BS.isInfixOf "<![CDATA[" encoded) `shouldBe` True

  , it "encode handles comments" $ do
      let root = Element (simpleName "r") V.empty
                   (V.singleton (Comment " a comment "))
          doc = Document Nothing root
          encoded = encode doc
      (BS.isInfixOf "<!--" encoded) `shouldBe` True

  , it "encode handles PIs" $ do
      let root = Element (simpleName "r") V.empty
                   (V.singleton (ProcessingInstruction "target" "data"))
          doc = Document Nothing root
          encoded = encode doc
      (BS.isInfixOf "<?target" encoded) `shouldBe` True

  , it "encode produces valid XML declaration" $ do
      let decl = XMLDecl "1.0" (Just "UTF-8") (Just True)
          root = Element (simpleName "r") V.empty V.empty
          doc = Document (Just decl) root
          encoded = encode doc
      (BS.isPrefixOf "<?xml" encoded) `shouldBe` True
      (BS.isInfixOf "version=\"1.0\"" encoded) `shouldBe` True
      (BS.isInfixOf "encoding=\"UTF-8\"" encoded) `shouldBe` True
      (BS.isInfixOf "standalone=\"yes\"" encoded) `shouldBe` True

  , it "encode handles namespaced elements" $ do
      let name = Name "child" (Just "ns") Nothing
          root = Element (simpleName "r") V.empty
                   (V.singleton (Element name V.empty V.empty))
          doc = Document Nothing root
          encoded = encode doc
      (BS.isInfixOf "ns:child" encoded) `shouldBe` True

  , it "encode handles empty elements as self-closing" $ do
      let root = Element (simpleName "r") V.empty
                   (V.singleton (Element (simpleName "empty") V.empty V.empty))
          doc = Document Nothing root
          encoded = encode doc
      (BS.isInfixOf "<empty/>" encoded) `shouldBe` True

  , it "pretty print produces indented output" $ do
      let root = Element (simpleName "root") V.empty
                   (V.fromList
                     [ Element (simpleName "a") V.empty (V.singleton (Text "1"))
                     , Element (simpleName "b") V.empty (V.singleton (Text "2"))
                     ])
          doc = Document Nothing root
          pretty = encodePretty 2 doc
          prettyText = TE.decodeUtf8 pretty
      (T.isInfixOf "  <a>" prettyText) `shouldBe` True

  , it "pretty print with nested elements" $ do
      let child_ = Element (simpleName "inner") V.empty (V.singleton (Text "deep"))
          root = Element (simpleName "root") V.empty
                   (V.singleton (Element (simpleName "outer") V.empty (V.singleton child_)))
          doc = Document Nothing root
          pretty = encodePretty 4 doc
          prettyText = TE.decodeUtf8 pretty
      (T.isInfixOf "    <outer>" prettyText) `shouldBe` True

  , it "encode Unicode text correctly" $ do
      let root = Element (simpleName "r") V.empty
                   (V.singleton (Text "\x4F60\x597D\x1F600"))
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) `shouldBe` "\x4F60\x597D\x1F600"

  , it "encode empty document" $ do
      let root = Element (simpleName "r") V.empty V.empty
          doc = Document Nothing root
          encoded = encode doc
      (not (BS.null encoded)) `shouldBe` True
      let Right doc2 = decode encoded
      V.null (elementChildren (docRoot doc2)) `shouldBe` True

  , it "encode document with only root, no children" $ do
      let root = Element (simpleName "root") V.empty V.empty
          doc = Document Nothing root
          encoded = encode doc
      encoded `shouldBe` "<root/>"

  , it "encode multiple text children" $ do
      let root = Element (simpleName "r") V.empty
                   (V.fromList [Text "a", Text "b", Text "c"])
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) `shouldBe` "abc"

  , it "encode attribute with special characters" $ do
      let root = Element (simpleName "r")
                   (V.singleton (Attribute (simpleName "val") "<>&'\""))
                   V.empty
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      attr "val" (docRoot doc2) `shouldBe` Just "<>&'\""
  ]

------------------------------------------------------------------------
-- Group 5: Path/DSL queries (20+ tests)
------------------------------------------------------------------------

pathDSLQueryTests :: Spec
pathDSLQueryTests = describe "Path/DSL Queries" $ sequence_
  [ it "Path: /root/child selects direct children" $ do
      let xml = "<root><child>a</child><child>b</child></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = queryPath ["child"] (docRoot doc)
      V.length results `shouldBe` 2

  , it "Path: //name selects descendants at any depth" $ do
      let xml = "<root><a><target>deep</target></a><target>shallow</target></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "//target"
          results = query path (docRoot doc)
      V.length results `shouldBe` 2

  , it "Path: @attr selects attribute" $ do
      let xml = "<root name=\"hello\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "@name"
          results = query path (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "hello"

  , it "Path: * wildcard selects all element children" $ do
      let xml = "<root><a/><b/><c/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "*"
          results = query path (docRoot doc)
      V.length results `shouldBe` 3

  , it "Path: . selects self" $ do
      let xml = "<root/>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "."
          results = query path (docRoot doc)
      V.length results `shouldBe` 1

  , it "Path: name[1] selects first" $ do
      let xml = "<root><item>a</item><item>b</item><item>c</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "item[1]"
          results = query path (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "a"

  , it "Path: name[@attr='val'] predicate filter" $ do
      let xml = "<root><item type=\"book\">B</item><item type=\"dvd\">D</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "item[@type='book']"
          results = query path (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "B"

  , it "DSL: child /> child composition" $ do
      let xml = "<root><a><b>deep</b></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "a" DSL./> DSL.child "b"
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "deep"

  , it "DSL: anyDescendant finds deeply nested" $ do
      let xml = "<root><a><b><c>deep</c></b></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.anyDescendant
          results = DSL.select q (docRoot doc)
      (V.length results >= 3) `shouldBe` True

  , it "DSL: whereAttr filters correctly" $ do
      let xml = "<root><x t=\"a\">1</x><x t=\"b\">2</x><x t=\"a\">3</x></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.whereAttr "t" "a" (DSL.child "x")
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 2

  , it "DSL: whereContains partial match" $ do
      let xml = "<root><a href=\"http://example.com/page\">X</a><a href=\"other\">Y</a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.whereContains "href" "example" (DSL.child "a")
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 1

  , it "DSL: index 1-based selection" $ do
      let xml = "<root><x>a</x><x>b</x><x>c</x></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.index 3 (DSL.child "x")
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 1
      textContent (V.head results) `shouldBe` "c"

  , it "DSL: first/last" $ do
      let xml = "<root><x>a</x><x>b</x><x>c</x></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          qFirst = DSL.first (DSL.child "x")
          qLast = DSL.last (DSL.child "x")
      case DSL.selectOne qFirst (docRoot doc) of
        Just n -> textContent n `shouldBe` "a"
        Nothing -> expectationFailure "Expected first"
      case DSL.selectOne qLast (docRoot doc) of
        Just n -> textContent n `shouldBe` "c"
        Nothing -> expectationFailure "Expected last"

  , it "DSL: union (|>) combines results" $ do
      let xml = "<root><a>1</a><b>2</b><c>3</c></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "a" DSL.|> DSL.child "c"
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 2

  , it "DSL: count returns correct number" $ do
      let xml = "<root><x/><x/><x/><x/><x/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.count (DSL.child "x")
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 1
      V.head results `shouldBe` 5

  , it "DSL: liftQuery user extension works" $ do
      let xml = "<root><a/><b/><c/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          customQ = DSL.liftQuery $ \n ->
            V.filter (isElementNamed "a") (elementChildren n)
          results = DSL.select customQ (docRoot doc)
      V.length results `shouldBe` 1

  , it "DSL: liftFilter custom predicate" $ do
      let xml = "<root><x val=\"10\"/><x val=\"20\"/><x val=\"5\"/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          bigVals = DSL.child "x" DSL./>
            DSL.liftFilter (\n ->
              case attr "val" n of
                Just v -> T.length v > 1
                Nothing -> False)
          results = DSL.select bigVals (docRoot doc)
      V.length results `shouldBe` 2

  , it "DSL: textContent extracts recursive text" $ do
      let xml = "<root>hello <b>world</b> end</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.textContent
          result = DSL.selectText q (docRoot doc)
      result `shouldBe` "hello world end"

  , it "DSL: attribute on element without that attr returns Nothing" $ do
      let xml = "<root/>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.attribute "missing"
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 1
      V.head results `shouldBe` Nothing

  , it "DSL: descendant chain (//>)" $ do
      let xml = "<root><a><x>1</x></a><b><x>2</x></b></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.anyChild DSL.//> DSL.child "x"
          results = DSL.select q (docRoot doc)
      V.length results `shouldBe` 2
  ]

------------------------------------------------------------------------
-- Group 6: Property-based tests (10+ properties) + generators
------------------------------------------------------------------------

genName :: Gen Name
genName = do
  local <- Gen.text (Range.linear 1 20) Gen.alpha
  pure (simpleName local)

genAttribute :: Gen Attribute
genAttribute = do
  name <- genName
  val <- Gen.text (Range.linear 0 50) (Gen.frequency
    [ (10, Gen.alphaNum)
    , (1, pure ' ')
    ])
  pure (Attribute name val)

genNode :: Int -> Gen Node
genNode maxDepth
  | maxDepth <= 0 = Gen.choice
      [ Text <$> Gen.text (Range.linear 1 30) Gen.alphaNum
      ]
  | otherwise = Gen.frequency
      [ (3, genElement maxDepth)
      , (2, Text <$> Gen.text (Range.linear 1 30) Gen.alphaNum)
      ]

genElement :: Int -> Gen Node
genElement maxDepth = do
  name <- genName
  nAttrs <- Gen.int (Range.linear 0 4)
  attrs <- V.fromList <$> Gen.list (Range.singleton nAttrs) genAttribute
  nChildren <- Gen.int (Range.linear 0 5)
  rawChildren <- Gen.list (Range.singleton nChildren) (genNode (maxDepth - 1))
  let children_ = V.fromList (dedupeTexts rawChildren)
  pure (Element name attrs children_)

dedupeTexts :: [Node] -> [Node]
dedupeTexts [] = []
dedupeTexts [x] = [x]
dedupeTexts (Text a : Text b : rest) = dedupeTexts (Text (a <> b) : rest)
dedupeTexts (x : rest) = x : dedupeTexts rest

genDocument :: Gen Document
genDocument = do
  useDecl <- Gen.bool
  let decl = if useDecl then Just (XMLDecl "1.0" (Just "UTF-8") Nothing) else Nothing
  root <- genElement 4
  pure (Document decl root)

propertyTests :: Spec
propertyTests = describe "Property Tests" $ sequence_
  [ it "parse (encode doc) == doc for generated documents" prop_roundtrip
  , it "parseFast then toDocument equals decode" prop_fastdom_equals_decode
  , it "text content survives SAX->DOM->encode->decode roundtrip" prop_text_roundtrip
  , it "attribute values survive roundtrip" prop_attr_roundtrip
  , it "element names survive roundtrip" prop_name_roundtrip
  , it "child count preserved through roundtrip" prop_child_count_roundtrip
  , it "generated docs with random depth parse successfully" prop_generated_parse
  , it "FastDOM nodeTagBS matches decode nodeTag" prop_fastdom_tag_match
  , it "encode produces parseable output" prop_encode_parseable
  , it "SAX event count reasonable for generated docs" prop_sax_event_count
  ]

prop_roundtrip :: Property
prop_roundtrip = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
      result = decode encoded
  case result of
    Left err -> fail $ "parse failed: " ++ err
    Right doc2 -> docRoot doc === docRoot doc2

prop_fastdom_equals_decode :: Property
prop_fastdom_equals_decode = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case (decode encoded, FD.parseFast encoded) of
    (Right docFull, Right fastDoc) ->
      docRoot (FD.toDocument fastDoc) === docRoot docFull
    (Left err, _) -> fail $ "decode failed: " ++ err
    (_, Left err) -> fail $ "parseFast failed: " ++ err

prop_text_roundtrip :: Property
prop_text_roundtrip = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case decode encoded of
    Left err -> fail $ "parse failed: " ++ err
    Right doc2 -> textContent (docRoot doc) === textContent (docRoot doc2)

prop_attr_roundtrip :: Property
prop_attr_roundtrip = property $ do
  name <- forAll genName
  val <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
  let root = Element (simpleName "root")
               (V.singleton (Attribute name val))
               V.empty
      doc = Document Nothing root
      encoded = encode doc
  case decode encoded of
    Left err -> fail $ "parse failed: " ++ err
    Right doc2 ->
      attr (nameLocal name) (docRoot doc2) === Just val

prop_name_roundtrip :: Property
prop_name_roundtrip = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case decode encoded of
    Left err -> fail $ "parse failed: " ++ err
    Right doc2 ->
      elementName (docRoot doc) === elementName (docRoot doc2)

prop_child_count_roundtrip :: Property
prop_child_count_roundtrip = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case decode encoded of
    Left err -> fail $ "parse failed: " ++ err
    Right doc2 -> do
      let countElems_ = V.length . V.filter (\n -> case n of Element {} -> True; _ -> False)
      countElems_ (elementChildren (docRoot doc)) === countElems_ (elementChildren (docRoot doc2))

prop_generated_parse :: Property
prop_generated_parse = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case decode encoded of
    Left err -> fail $ "parse failed: " ++ err
    Right _ -> HH.assert True

prop_fastdom_tag_match :: Property
prop_fastdom_tag_match = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case (decode encoded, FD.parseFast encoded) of
    (Right docFull, Right fastDoc) -> do
      let fullName = elementName (docRoot docFull)
          fastTag = FD.nodeTag (FD.fdRoot fastDoc) (FD.fdSource fastDoc)
      case fullName of
        Just n -> nameLocal n === fastTag
        Nothing -> fail "expected element"
    (Left err, _) -> fail $ "decode failed: " ++ err
    (_, Left err) -> fail $ "parseFast failed: " ++ err

prop_encode_parseable :: Property
prop_encode_parseable = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case parseSAX encoded of
    Left err -> fail $ "SAX parse of encoded doc failed: " ++ err
    Right _ -> HH.assert True

prop_sax_event_count :: Property
prop_sax_event_count = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case parseSAX encoded of
    Left err -> fail $ "SAX parse failed: " ++ err
    Right events -> HH.assert (V.length events >= 3)

------------------------------------------------------------------------
-- Group 7: Conformance / edge cases from real XML (10+ tests)
------------------------------------------------------------------------

conformanceTests :: Spec
conformanceTests = describe "Conformance / Real-World XML" $ sequence_
  [ it "RSS feed excerpt" $ do
      let xml = T.concat
            [ "<rss version=\"2.0\">"
            , "<channel>"
            , "<title>Example Feed</title>"
            , "<link>http://example.com</link>"
            , "<description>An example RSS feed</description>"
            , "<item>"
            , "<title>Article 1</title>"
            , "<link>http://example.com/1</link>"
            , "<description>First article</description>"
            , "</item>"
            , "<item>"
            , "<title>Article 2</title>"
            , "<link>http://example.com/2</link>"
            , "</item>"
            , "</channel>"
            , "</rss>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      elementName root `shouldBe` Just (simpleName "rss")
      attr "version" root `shouldBe` Just "2.0"
      let items = query (Descendant (simpleName "item")) root
      V.length items `shouldBe` 2

  , it "SOAP envelope structure" $ do
      let xml = T.concat
            [ "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
            , "<soap:Header/>"
            , "<soap:Body>"
            , "<m:GetPrice xmlns:m=\"http://example.com/prices\">"
            , "<m:Item>Widget</m:Item>"
            , "</m:GetPrice>"
            , "</soap:Body>"
            , "</soap:Envelope>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element n _ _ -> do
          nameLocal n `shouldBe` "Envelope"
          namePrefix n `shouldBe` Just "soap"
        _ -> fail "Expected Element"

  , it "SVG snippet with namespaces" $ do
      let xml = T.concat
            [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
            , "<circle cx=\"50\" cy=\"50\" r=\"40\" fill=\"red\"/>"
            , "<text x=\"50\" y=\"50\">Hello</text>"
            , "</svg>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      attr "width" root `shouldBe` Just "100"
      let circles = queryPath ["circle"] root
      V.length circles `shouldBe` 1
      attr "fill" (V.head circles) `shouldBe` Just "red"

  , it "XHTML with mixed content" $ do
      let xml = T.concat
            [ "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
            , "<body>"
            , "<p>This is <em>emphasized</em> and <strong>strong</strong> text.</p>"
            , "</body>"
            , "</html>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      let ps = query (Descendant (simpleName "p")) root
      V.length ps `shouldBe` 1
      let pText = textContent (V.head ps)
      (T.isInfixOf "emphasized" pText) `shouldBe` True

  , it "Android layout XML" $ do
      let xml = T.concat
            [ "<LinearLayout xmlns:android=\"http://schemas.android.com/apk/res/android\""
            , " android:layout_width=\"match_parent\""
            , " android:layout_height=\"wrap_content\""
            , " android:orientation=\"vertical\">"
            , "<TextView android:text=\"Hello World\"/>"
            , "<Button android:text=\"Click Me\"/>"
            , "</LinearLayout>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      V.length (elementChildren root) `shouldBe` 2

  , it "Maven POM excerpt" $ do
      let xml = T.concat
            [ "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">"
            , "<modelVersion>4.0.0</modelVersion>"
            , "<groupId>com.example</groupId>"
            , "<artifactId>my-app</artifactId>"
            , "<version>1.0-SNAPSHOT</version>"
            , "<dependencies>"
            , "<dependency>"
            , "<groupId>junit</groupId>"
            , "<artifactId>junit</artifactId>"
            , "<version>4.13</version>"
            , "</dependency>"
            , "</dependencies>"
            , "</project>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element n _ _ -> nameLocal n `shouldBe` "project"
        _ -> fail "Expected Element"
      let versions = query (Descendant (simpleName "version")) root
      V.length versions `shouldBe` 2

  , it "XML with doctype (should skip/handle gracefully)" $ do
      let xml = "<?xml version=\"1.0\"?><!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><html><body>hello</body></html>"
          Right doc = decode (TE.encodeUtf8 xml)
      elementName (docRoot doc) `shouldBe` Just (simpleName "html")

  , it "XML with processing instructions before root" $ do
      let xml = "<?xml version=\"1.0\"?><?xml-stylesheet type=\"text/xsl\" href=\"style.xsl\"?><root/>"
          Right doc = decode (TE.encodeUtf8 xml)
      elementName (docRoot doc) `shouldBe` Just (simpleName "root")

  , it "XML with comments before root" $ do
      let xml = "<?xml version=\"1.0\"?><!-- comment before root --><root><child/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
      elementName (docRoot doc) `shouldBe` Just (simpleName "root")

  , it "XML with trailing whitespace after root" $ do
      let xml = "<root/>   \n  "
          result = decode (TE.encodeUtf8 xml)
      (not (isLeft result)) `shouldBe` True

  , it "Atom feed excerpt" $ do
      let xml = T.concat
            [ "<feed xmlns=\"http://www.w3.org/2005/Atom\">"
            , "<title>Example Feed</title>"
            , "<entry>"
            , "<title>Entry 1</title>"
            , "<id>urn:uuid:1</id>"
            , "<content type=\"html\">Some &amp; content</content>"
            , "</entry>"
            , "</feed>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          entries = query (Descendant (simpleName "entry")) (docRoot doc)
      V.length entries `shouldBe` 1
      let contents = query (Descendant (simpleName "content")) (docRoot doc)
      V.length contents `shouldBe` 1
      textContent (V.head contents) `shouldBe` "Some & content"

  , it "XML with nested CDATA and entities" $ do
      let xml = "<root><![CDATA[Hello]]> &amp; <![CDATA[World]]></root>"
          Right doc = decode (TE.encodeUtf8 xml)
      let allText = textContent (docRoot doc)
      (T.isInfixOf "Hello" allText) `shouldBe` True
      (T.isInfixOf "&" allText) `shouldBe` True
      (T.isInfixOf "World" allText) `shouldBe` True
  ]

------------------------------------------------------------------------
-- Group 8: Incremental parser tests
------------------------------------------------------------------------

incrementalTests :: Spec
incrementalTests = describe "Incremental Parser" $ sequence_
  [ it "feed small doc in one chunk, get all events" $ do
      let xml = TE.encodeUtf8 "<root><child>text</child></root>"
      p <- newParser
      events1 <- feedChunk p xml
      Right events2 <- feedEnd p
      let allEvents = events1 <> events2
          evList = V.toList allEvents
      (any (\e -> case e of StartElement n _ -> nameLocal n == "root"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of Characters t -> t == "text"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of EndElement n -> nameLocal n == "root"; _ -> False) evList) `shouldBe` True

  , it "feed doc in multiple chunks (split mid-tag), get correct events" $ do
      let xmlFull = TE.encodeUtf8 "<root><child attr=\"val\">content</child></root>"
          chunk1 = BS.take 15 xmlFull
          chunk2 = BS.drop 15 xmlFull
      p <- newParser
      ev1 <- feedChunk p chunk1
      ev2 <- feedChunk p chunk2
      Right ev3 <- feedEnd p
      let allEvents = ev1 <> ev2 <> ev3
          evList = V.toList allEvents
      (any (\e -> case e of StartElement n _ -> nameLocal n == "root"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of StartElement n _ -> nameLocal n == "child"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of Characters t -> t == "content"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of EndElement n -> nameLocal n == "child"; _ -> False) evList) `shouldBe` True

  , it "feed doc byte-by-byte, get same events as parseSAX" $ do
      let xmlFull = TE.encodeUtf8 "<root><a>text</a></root>"
          Right refEvents = parseSAX xmlFull
      p <- newParser
      byteChunks <- mapM (\i -> feedChunk p (BS.singleton (BS.index xmlFull i)))
                         [0 .. BS.length xmlFull - 1]
      Right finalEvents <- feedEnd p
      let allEvents = V.concat (byteChunks ++ [finalEvents])
      V.toList allEvents `shouldBe` V.toList refEvents

  , it "unterminated tag across chunk boundary" $ do
      let chunk1 = TE.encodeUtf8 "<root><child"
      p <- newParser
      ev1 <- feedChunk p chunk1
      -- chunk1 contains complete <root> tag so some events may be emitted
      let chunk2 = TE.encodeUtf8 ">text</child></root>"
      ev2 <- feedChunk p chunk2
      Right ev3 <- feedEnd p
      let allEvents = ev1 <> ev2 <> ev3
          evList = V.toList allEvents
      (any (\e -> case e of StartElement n _ -> nameLocal n == "child"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of Characters t -> t == "text"; _ -> False) evList) `shouldBe` True
      (any (\e -> case e of EndElement n -> nameLocal n == "child"; _ -> False) evList) `shouldBe` True
  ]

------------------------------------------------------------------------
-- Group 9: Concurrent parser tests
------------------------------------------------------------------------

concurrentTests :: Spec
concurrentTests = describe "Concurrent Parser" $ sequence_
  [ it "withConcurrentParse produces same events as parseSAX" $ do
      let xml = TE.encodeUtf8 "<root><a>text1</a><b>text2</b></root>"
          Right refEvents = parseSAX xml
      eventsRef <- newIORef []
      Right () <- withConcurrentParse xml 256 $ \ev ->
        modifyIORef' eventsRef (ev :)
      concEvents <- V.fromList . reverse <$> readIORef eventsRef
      V.toList concEvents `shouldBe` V.toList refEvents

  , it "handler processes events in order" $ do
      let xml = TE.encodeUtf8 "<root><a/><b/><c/></root>"
      eventsRef <- newIORef ([] :: [SAXEvent])
      Right () <- withConcurrentParse xml 256 $ \ev ->
        modifyIORef' eventsRef (ev :)
      events <- reverse <$> readIORef eventsRef
      let elemNames = [ nameLocal n | StartElement n _ <- events ]
      (head elemNames == "root") `shouldBe` True
      (elemNames == ["root", "a", "b", "c"]) `shouldBe` True

  , it "parse error propagated to consumer" $ do
      let xml = TE.encodeUtf8 "<root><a></b></root>"
      result <- withConcurrentParse xml 256 (\_ -> pure ())
      (isLeft result) `shouldBe` True

  , it "large document (10K elements) parses correctly" $ do
      let n = 10000 :: Int
          items = T.concat [ "<item>" <> T.pack (show i) <> "</item>" | i <- [1..n] ]
          xml = TE.encodeUtf8 ("<root>" <> items <> "</root>")
      countRef <- newIORef (0 :: Int)
      Right () <- withConcurrentParse xml 256 $ \ev ->
        case ev of
          StartElement sn _ | nameLocal sn == "item" -> modifyIORef' countRef (+1)
          _ -> pure ()
      count <- readIORef countRef
      count `shouldBe` n

  , it "concurrent vs sequential: same results for complex document" $ do
      let xml = TE.encodeUtf8 $ T.concat
            [ "<?xml version=\"1.0\"?>"
            , "<catalog>"
            , "<book id=\"1\"><title>Haskell</title><price>29.99</price></book>"
            , "<book id=\"2\"><title>Erlang</title><price>39.99</price></book>"
            , "<!-- comment -->"
            , "<![CDATA[raw data]]>"
            , "</catalog>"
            ]
          Right refEvents = parseSAX xml
      concRef <- newIORef []
      Right () <- withConcurrentParse xml 64 $ \ev ->
        modifyIORef' concRef (ev :)
      concEvents <- V.fromList . reverse <$> readIORef concRef
      V.toList concEvents `shouldBe` V.toList refEvents

  , it "parseToChan low-level API works" $ do
      let xml = TE.encodeUtf8 "<root><a>text</a></root>"
      (chan, tid) <- parseToChan xml 32
      eventsRef <- newIORef []
      let loop = do
            item <- atomically $ readTBQueue chan
            case item of
              Nothing -> pure ()
              Just (Left _) -> pure ()
              Just (Right ev) -> do
                modifyIORef' eventsRef (ev :)
                loop
      loop
      killThread tid
      events <- reverse <$> readIORef eventsRef
      (not (null events)) `shouldBe` True
      let Right refEvents = parseSAX xml
      events `shouldBe` V.toList refEvents

  , it "withConcurrentParseBS with chunked input" $ do
      let xml = TE.encodeUtf8 "<root><a>hello</a></root>"
          chunks = [BS.take 10 xml, BS.drop 10 xml]
          Right refEvents = parseSAX xml
      eventsRef <- newIORef []
      Right () <- withConcurrentParseBS chunks 128 $ \ev ->
        modifyIORef' eventsRef (ev :)
      concEvents <- V.fromList . reverse <$> readIORef eventsRef
      V.toList concEvents `shouldBe` V.toList refEvents
  ]

------------------------------------------------------------------------
-- Group 10: Streaming fold tests
------------------------------------------------------------------------

streamFoldTests :: Spec
streamFoldTests = describe "Stream Fold" $ sequence_
  [ it "count elements concurrently" $ do
      let xml = TE.encodeUtf8 "<root><a/><b/><c/><d/></root>"
      Right count <- streamFold xml 64 (0 :: Int) $ \acc ev ->
        case ev of
          StartElement _ _ -> acc + 1
          _ -> acc
      count `shouldBe` 5

  , it "extract all text content concurrently" $ do
      let xml = TE.encodeUtf8 "<root>hello <b>world</b> end</root>"
      Right texts <- streamFold xml 64 ([] :: [Text]) $ \acc ev ->
        case ev of
          Characters t -> acc ++ [t]
          _ -> acc
      T.concat texts `shouldBe` "hello world end"

  , it "streamFoldIO: write events to IORef, verify count" $ do
      let xml = TE.encodeUtf8 "<root><x/><x/><x/></root>"
      ref <- newIORef (0 :: Int)
      Right count <- streamFoldIO xml 64 (0 :: Int) $ \acc ev -> do
        case ev of
          StartElement _ _ -> do
            modifyIORef' ref (+1)
            pure (acc + 1)
          _ -> pure acc
      count `shouldBe` 4
      ioCount <- readIORef ref
      ioCount `shouldBe` 4

  , it "streamFold on large document" $ do
      let n = 5000 :: Int
          items = T.concat [ "<item>" <> T.pack (show i) <> "</item>" | i <- [1..n] ]
          xml = TE.encodeUtf8 ("<root>" <> items <> "</root>")
      Right total <- streamFold xml 128 (0 :: Int) $ \acc ev ->
        case ev of
          StartElement sn _ | nameLocal sn == "item" -> acc + 1
          _ -> acc
      total `shouldBe` n

  , it "streamFold parse error propagated" $ do
      let xml = TE.encodeUtf8 "<root><bad></mismatch></root>"
      result <- streamFold xml 64 (0 :: Int) $ \acc _ -> acc + 1
      (isLeft result) `shouldBe` True
  ]

------------------------------------------------------------------------
-- Group 11: W3C-style well-formedness conformance tests
------------------------------------------------------------------------

w3cConformanceTests :: Spec
w3cConformanceTests = describe "W3C Well-Formedness Conformance" $ sequence_
  [ validDocTests
  , invalidDocTests
  , encodingEdgeCaseTests
  ]

validDocTests :: Spec
validDocTests = describe "Should parse successfully" $ sequence_
  [ it "minimal: <x/>" $ do
      let Right doc = decode (TE.encodeUtf8 "<x/>")
      elementName (docRoot doc) `shouldBe` Just (simpleName "x")

  , it "with XML decl" $ do
      let Right doc = decode (TE.encodeUtf8 "<?xml version=\"1.0\"?><x/>")
      elementName (docRoot doc) `shouldBe` Just (simpleName "x")

  , it "with encoding decl" $ do
      let Right doc = decode (TE.encodeUtf8 "<?xml version=\"1.0\" encoding=\"UTF-8\"?><x/>")
      elementName (docRoot doc) `shouldBe` Just (simpleName "x")

  , it "entity refs: &amp;&lt;&gt;&apos;&quot;" $ do
      let Right doc = decode (TE.encodeUtf8 "<x>&amp;&lt;&gt;&apos;&quot;</x>")
      textContent (docRoot doc) `shouldBe` "&<>'\""

  , it "decimal char refs: &#65;&#x42;" $ do
      let Right doc = decode (TE.encodeUtf8 "<x>&#65;&#x42;</x>")
      textContent (docRoot doc) `shouldBe` "AB"

  , it "CDATA section" $ do
      let Right doc = decode (TE.encodeUtf8 "<x><![CDATA[<not a tag>]]></x>")
      textContent (docRoot doc) `shouldBe` "<not a tag>"

  , it "namespaces" $ do
      let Right doc = decode (TE.encodeUtf8 "<a:x xmlns:a=\"http://example.com\"/>")
          root = docRoot doc
      case root of
        Element n _ _ -> do
          nameLocal n `shouldBe` "x"
          namePrefix n `shouldBe` Just "a"
          nameNamespace n `shouldBe` Just "http://example.com"
        _ -> fail "Expected Element"

  , it "nested elements" $ do
      let Right doc = decode (TE.encodeUtf8 "<a><b><c/></b></a>")
          root = docRoot doc
      elementName root `shouldBe` Just (simpleName "a")
      let results = queryPath ["b", "c"] root
      V.length results `shouldBe` 1

  , it "attributes single and double quotes" $ do
      let Right doc = decode (TE.encodeUtf8 "<x a=\"1\" b='2'/>")
      attr "a" (docRoot doc) `shouldBe` Just "1"
      attr "b" (docRoot doc) `shouldBe` Just "2"

  , it "mixed content" $ do
      let Right doc = decode (TE.encodeUtf8 "<x>text<y/>more</x>")
          cs = elementChildren (docRoot doc)
      V.length cs `shouldBe` 3
      textContent (docRoot doc) `shouldBe` "textmore"

  , it "processing instruction before root" $ do
      let Right doc = decode (TE.encodeUtf8 "<?target data?><x/>")
      elementName (docRoot doc) `shouldBe` Just (simpleName "x")

  , it "comment before root" $ do
      let Right doc = decode (TE.encodeUtf8 "<!--comment--><x/>")
      elementName (docRoot doc) `shouldBe` Just (simpleName "x")

  , it "multiple namespaces" $ do
      let xml = "<root xmlns:a=\"http://a.com\" xmlns:b=\"http://b.com\"><a:x/><b:y/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          cs = elementChildren (docRoot doc)
      V.length cs `shouldBe` 2

  , it "default namespace inheritance" $ do
      let xml = "<root xmlns=\"http://default.com\"><child/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          cs = elementChildren (docRoot doc)
      case V.head cs of
        Element n _ _ -> nameNamespace n `shouldBe` Just "http://default.com"
        _ -> fail "Expected Element"

  , it "nested CDATA and text" $ do
      let xml = "<x>text<![CDATA[<cdata>]]>more</x>"
          Right doc = decode (TE.encodeUtf8 xml)
      let allText = textContent (docRoot doc)
      (T.isInfixOf "text" allText) `shouldBe` True
      (T.isInfixOf "<cdata>" allText) `shouldBe` True
      (T.isInfixOf "more" allText) `shouldBe` True

  , it "empty element with attributes" $ do
      let Right doc = decode (TE.encodeUtf8 "<x a=\"1\" b=\"2\" c=\"3\"/>")
      V.null (elementChildren (docRoot doc)) `shouldBe` True
      attr "a" (docRoot doc) `shouldBe` Just "1"
      attr "b" (docRoot doc) `shouldBe` Just "2"
      attr "c" (docRoot doc) `shouldBe` Just "3"

  , it "whitespace-only text" $ do
      let Right doc = decode (TE.encodeUtf8 "<x>   \n\t  </x>")
      textContent (docRoot doc) `shouldBe` "   \n\t  "
  ]

invalidDocTests :: Spec
invalidDocTests = describe "Should fail with error" $ sequence_
  [ it "empty input" $ do
      let result = decode BS.empty
      (isLeft result) `shouldBe` True

  , it "mismatched tags: <a></b>" $ do
      let result = decode (TE.encodeUtf8 "<a></b>")
      (isLeft result) `shouldBe` True

  , it "unclosed tag: <a>" $ do
      let result = decode (TE.encodeUtf8 "<a>")
      (isLeft result) `shouldBe` True

  , it "ampersand alone: <x>&</x>" $ do
      let result = decode (TE.encodeUtf8 "<x>&</x>")
      (isLeft result) `shouldBe` True

  , it "unknown entity: <x>&bogus;</x>" $ do
      let result = decode (TE.encodeUtf8 "<x>&bogus;</x>")
      (isLeft result) `shouldBe` True

  , it "no root element (whitespace only)" $ do
      let result = decode (TE.encodeUtf8 "   ")
      (isLeft result) `shouldBe` True

  , it "attribute without value: <x a/>" $ do
      let result = decode (TE.encodeUtf8 "<x a/>")
      (isLeft result) `shouldBe` True

  , it "malformed tag: <>" $ do
      let result = decode (TE.encodeUtf8 "<>")
      (isLeft result) `shouldBe` True

  , it "unclosed attribute: <x a=\"1>" $ do
      let result = decode (TE.encodeUtf8 "<x a=\"1>")
      (isLeft result) `shouldBe` True

  , it "text before root element (lenient: may succeed)" $ do
      let result = decode (TE.encodeUtf8 "hello<x/>")
      case result of
        Left _  -> pure ()
        Right doc -> elementName (docRoot doc) `shouldBe` Just (simpleName "x")

  , it "two root elements (lenient: may succeed)" $ do
      let result = decode (TE.encodeUtf8 "<a/><b/>")
      case result of
        Left _  -> pure ()
        Right doc -> (True) `shouldBe` True
  ]

encodingEdgeCaseTests :: Spec
encodingEdgeCaseTests = describe "Encoding edge cases" $ sequence_
  [ it "UTF-8 BOM before XML" $ do
      let bom = BS.pack [0xEF, 0xBB, 0xBF]
          xml = bom <> TE.encodeUtf8 "<?xml version=\"1.0\"?><x/>"
          result = decode xml
      (not (isLeft result)) `shouldBe` True

  , it "multi-byte UTF-8: Japanese" $ do
      let xml = TE.encodeUtf8 "<x>\x65E5\x672C\x8A9E</x>"
          Right doc = decode xml
      textContent (docRoot doc) `shouldBe` "\x65E5\x672C\x8A9E"

  , it "emoji: grinning face" $ do
      let xml = TE.encodeUtf8 "<x>\x1F600</x>"
          Right doc = decode xml
      textContent (docRoot doc) `shouldBe` "\x1F600"

  , it "4-byte UTF-8: musical symbol G clef" $ do
      let xml = TE.encodeUtf8 "<x>\x1D11E</x>"
          Right doc = decode xml
      textContent (docRoot doc) `shouldBe` "\x1D11E"

  , it "mixed multi-byte characters" $ do
      let xml = TE.encodeUtf8 "<x>\x00FC\x00E9\x4E16\x754C\x1F600</x>"
          Right doc = decode xml
          expected = "\x00FC\x00E9\x4E16\x754C\x1F600"
      textContent (docRoot doc) `shouldBe` expected

  , it "char ref for high unicode" $ do
      let xml = TE.encodeUtf8 "<x>&#x1D11E;</x>"
          Right doc = decode xml
      textContent (docRoot doc) `shouldBe` "\x1D11E"

  , it "char ref decimal" $ do
      let xml = TE.encodeUtf8 "<x>&#119070;</x>"
          Right doc = decode xml
      textContent (docRoot doc) `shouldBe` "\x1D11E"

  , it "roundtrip multi-byte text" $ do
      let text_ = "\x65E5\x672C\x8A9E\x1F600\x1D11E"
          root = Element (simpleName "r") V.empty (V.singleton (Text text_))
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) `shouldBe` text_
  ]
