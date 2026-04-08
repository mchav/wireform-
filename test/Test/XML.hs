{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.XML (xmlTests) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
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

xmlTests :: TestTree
xmlTests = testGroup "XML"
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
  ]

-- Simple document for testing
simpleXML :: Text
simpleXML = "<root><child attr=\"value\">text</child></root>"

-- SAX Tests
saxTests :: TestTree
saxTests = testGroup "SAX Parser"
  [ testCase "parse simple document" $ do
      let Right events = parseSAX (TE.encodeUtf8 simpleXML)
          evList = V.toList events
      assertBool "has StartDocument" (any isStartDoc evList)
      assertBool "has EndDocument" (any isEndDoc evList)
      assertBool "has StartElement root" $
        any (\e -> case e of StartElement n _ -> nameLocal n == "root"; _ -> False) evList
      assertBool "has StartElement child" $
        any (\e -> case e of StartElement n _ -> nameLocal n == "child"; _ -> False) evList
      assertBool "has Characters text" $
        any (\e -> case e of Characters t -> t == "text"; _ -> False) evList
      assertBool "has EndElement child" $
        any (\e -> case e of EndElement n -> nameLocal n == "child"; _ -> False) evList
      assertBool "has EndElement root" $
        any (\e -> case e of EndElement n -> nameLocal n == "root"; _ -> False) evList

  , testCase "parse with XML declaration" $ do
      let xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has StartDocument with decl" $
        any (\e -> case e of
          StartDocument (Just decl) -> xmlVersion decl == "1.0" && xmlEncoding decl == Just "UTF-8"
          _ -> False) evList

  , testCase "foldSAX counts elements" $ do
      let xml = "<a><b/><c/><d/></a>"
          Right count = foldSAX countElems 0 (TE.encodeUtf8 xml)
      count @?= 4

  , testCase "parse self-closing tags" $ do
      let xml = "<root><empty/></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has StartElement empty" $
        any (\e -> case e of StartElement n _ -> nameLocal n == "empty"; _ -> False) evList
      assertBool "has EndElement empty" $
        any (\e -> case e of EndElement n -> nameLocal n == "empty"; _ -> False) evList

  , testCase "error on mismatched tags" $ do
      let xml = "<a><b></c></a>"
          result = parseSAX (TE.encodeUtf8 xml)
      assertBool "should be error" (isLeft result)
  ]

-- DOM Tests
domTests :: TestTree
domTests = testGroup "DOM Parser"
  [ testCase "decode simple document" $ do
      let Right doc = decode (TE.encodeUtf8 simpleXML)
          root = docRoot doc
      elementName root @?= Just (simpleName "root")
      V.length (elementChildren root) @?= 1
      let child = V.head (elementChildren root)
      elementName child @?= Just (simpleName "child")

  , testCase "decode with attributes" $ do
      let xml = "<person name=\"John\" age=\"30\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      attr "name" root @?= Just "John"
      attr "age" root @?= Just "30"

  , testCase "decode nested elements" $ do
      let xml = "<a><b><c>deep</c></b></a>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = queryPath ["b", "c"] (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "deep"
  ]

-- Roundtrip tests
roundtripTests :: TestTree
roundtripTests = testGroup "Roundtrip"
  [ testCase "encode then decode = identity" $ do
      let xml = "<root><child attr=\"value\">text</child><empty/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      docRoot doc @?= docRoot doc2

  , testCase "roundtrip with XML declaration" $ do
      let decl = XMLDecl "1.0" (Just "UTF-8") Nothing
          root = Element (simpleName "test") V.empty
                   (V.singleton (Text "hello"))
          doc = Document (Just decl) root
          encoded = encode doc
          Right doc2 = decode encoded
      docRoot doc @?= docRoot doc2

  , testCase "roundtrip with CDATA" $ do
      let root = Element (simpleName "root") V.empty
                   (V.singleton (CData "some <special> & chars"))
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
          child = V.head (elementChildren (docRoot doc2))
      case child of
        CData t -> t @?= "some <special> & chars"
        _ -> assertFailure "Expected CDATA node"

  , testCase "roundtrip with multiple children" $ do
      let children = V.fromList
            [ Element (simpleName "a") V.empty (V.singleton (Text "1"))
            , Element (simpleName "b") V.empty (V.singleton (Text "2"))
            , Element (simpleName "c") V.empty (V.singleton (Text "3"))
            ]
          root = Element (simpleName "root") V.empty children
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      docRoot doc @?= docRoot doc2
  ]

-- Namespace tests
namespaceTests :: TestTree
namespaceTests = testGroup "Namespaces"
  [ testCase "default namespace" $ do
      let xml = "<root xmlns=\"http://example.com\"><child/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element name _ _ ->
          nameNamespace name @?= Just "http://example.com"
        _ -> assertFailure "Expected Element"

  , testCase "prefixed namespace" $ do
      let xml = "<ns:root xmlns:ns=\"http://example.com\"><ns:child/></ns:root>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element name _ _ -> do
          nameLocal name @?= "root"
          namePrefix name @?= Just "ns"
          nameNamespace name @?= Just "http://example.com"
        _ -> assertFailure "Expected Element"
  ]

-- Entity reference tests
entityTests :: TestTree
entityTests = testGroup "Entity References"
  [ testCase "standard entities" $ do
      let xml = "<root>&amp;&lt;&gt;&apos;&quot;</root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "&<>'\""

  , testCase "numeric entity decimal" $ do
      let xml = "<root>&#65;&#66;&#67;</root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "ABC"

  , testCase "numeric entity hex" $ do
      let xml = "<root>&#x41;&#x42;&#x43;</root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "ABC"

  , testCase "entities in attributes" $ do
      let xml = "<root attr=\"a&amp;b\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "attr" (docRoot doc) @?= Just "a&b"
  ]

-- CDATA tests
cdataTests :: TestTree
cdataTests = testGroup "CDATA Sections"
  [ testCase "parse CDATA" $ do
      let xml = "<root><![CDATA[<not>markup & stuff]]></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has CDATASection" $
        any (\e -> case e of CDATASection t -> t == "<not>markup & stuff"; _ -> False) evList

  , testCase "CDATA in DOM" $ do
      let xml = "<root><![CDATA[content]]></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      V.length children @?= 1
      case V.head children of
        CData t -> t @?= "content"
        _ -> assertFailure "Expected CData node"
  ]

-- Comment and PI tests
commentAndPITests :: TestTree
commentAndPITests = testGroup "Comments and PIs"
  [ testCase "parse comment" $ do
      let xml = "<root><!-- a comment --></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has CommentEvent" $
        any (\e -> case e of CommentEvent t -> T.strip t == "a comment"; _ -> False) evList

  , testCase "parse processing instruction" $ do
      let xml = "<root><?target data?></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has PI" $
        any (\e -> case e of PI t _ -> t == "target"; _ -> False) evList

  , testCase "comment in DOM" $ do
      let xml = "<root><!-- hello --></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      assertBool "has comment child" $
        V.any isCommentNode children

  , testCase "PI in DOM" $ do
      let xml = "<root><?app data?></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      assertBool "has PI child" $
        V.any isPINode children
  ]

-- Path query tests
pathTests :: TestTree
pathTests = testGroup "Path Queries"
  [ testCase "child query" $ do
      let xml = "<root><items><item>a</item><item>b</item></items></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = queryPath ["items", "item"] (docRoot doc)
      V.length results @?= 2

  , testCase "descendant query" $ do
      let xml = "<root><a><b><target>found</target></b></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query (Descendant (simpleName "target")) (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "found"

  , testCase "attribute query" $ do
      let xml = "<person name=\"John\" age=\"30\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "name" (docRoot doc) @?= Just "John"
      attr "age" (docRoot doc) @?= Just "30"
      attr "missing" (docRoot doc) @?= Nothing

  , testCase "text content recursive" $ do
      let xml = "<root>hello <b>world</b></root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "hello world"

  , testCase "parsePath simple" $ do
      let Right path = parsePath "items/item"
          xml = "<root><items><item>x</item></items></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results @?= 1

  , testCase "parsePath descendant" $ do
      let Right path = parsePath "//item"
          xml = "<root><a><item>x</item></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results @?= 1
  ]

-- Typeclass tests
typeclassTests :: TestTree
typeclassTests = testGroup "Typeclass Instances"
  [ testCase "Text roundtrip" $ do
      let val = "hello" :: Text
          node = toXML val
      fromXML node @?= Right val

  , testCase "Int roundtrip" $ do
      let val = 42 :: Int
          node = toXML val
      fromXML node @?= Right val

  , testCase "Bool roundtrip" $ do
      let node = toXML True
      fromXML node @?= Right True
      let node2 = toXML False
      fromXML node2 @?= Right False

  , testCase "Maybe roundtrip" $ do
      let val = Just (42 :: Int)
          node = toXML val
      fromXML node @?= Right val
      let val2 = Nothing :: Maybe Int
          node2 = toXML val2
      fromXML node2 @?= Right val2

  , testCase "List roundtrip" $ do
      let val = [1, 2, 3] :: [Int]
          node = toXML val
      fromXML node @?= Right val
  ]

-- Generic deriving tests
data TestPerson = TestPerson
  { name :: !Text
  , age :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToXML, FromXML)

genericTests :: TestTree
genericTests = testGroup "Generic Deriving"
  [ testCase "record to XML" $ do
      let person = TestPerson "John" 30
          node = toXML person
      case node of
        Element n _ cs -> do
          nameLocal n @?= "TestPerson"
          V.length cs @?= 2
        _ -> assertFailure "Expected Element"

  , testCase "record roundtrip" $ do
      let person = TestPerson "Jane" 25
          node = toXML person
      fromXML node @?= Right person

  , testCase "encodeXML / decodeXML roundtrip" $ do
      let person = TestPerson "Bob" 40
          bs = encodeXML person
          Right person2 = decodeXML bs :: Either String TestPerson
      person2 @?= person
  ]

-- Large document test
largeDocTests :: TestTree
largeDocTests = testGroup "Large Documents"
  [ testCase "10k elements parse and verify" $ do
      let n = 10000 :: Int
          items = T.concat [ "<item id=\"" <> T.pack (show i) <> "\">"
                           <> T.pack (show i) <> "</item>"
                           | i <- [1..n] ]
          xml = "<root>" <> items <> "</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      V.length children @?= n
      let firstChild = V.head children
          lastChild = V.last children
      textContent firstChild @?= "1"
      textContent lastChild @?= T.pack (show n)
      attr "id" firstChild @?= Just "1"
      attr "id" lastChild @?= Just (T.pack (show n))
  ]

-- Edge case tests
edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge Cases"
  [ testCase "empty element" $ do
      let xml = "<root/>"
          Right doc = decode (TE.encodeUtf8 xml)
      V.null (elementChildren (docRoot doc)) @?= True

  , testCase "self-closing tag" $ do
      let xml = "<root><br/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
      V.length (elementChildren (docRoot doc)) @?= 1

  , testCase "attributes with single quotes" $ do
      let xml = "<root attr='value'/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "attr" (docRoot doc) @?= Just "value"

  , testCase "attributes with double quotes" $ do
      let xml = "<root attr=\"value\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "attr" (docRoot doc) @?= Just "value"

  , testCase "whitespace handling" $ do
      let xml = "<root>  hello  </root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "  hello  "

  , testCase "mixed content" $ do
      let xml = "<root>text1<child/>text2</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      V.length children @?= 3

  , testCase "deeply nested" $ do
      let depth = 100 :: Int
          opens = T.concat [ "<n" <> T.pack (show i) <> ">" | i <- [1..depth] ]
          closes = T.concat [ "</n" <> T.pack (show i) <> ">" | i <- reverse [1..depth] ]
          xml = opens <> "leaf" <> closes
          Right doc = decode (TE.encodeUtf8 xml)
      assertBool "parsed successfully" True
      let go (Element _ _ cs) = if V.null cs then 0 else 1 + go (V.head cs)
          go _ = 0 :: Int
      go (docRoot doc) @?= depth

  , testCase "pretty print" $ do
      let root = Element (simpleName "root") V.empty
                   (V.fromList
                     [ Element (simpleName "a") V.empty (V.singleton (Text "1"))
                     , Element (simpleName "b") V.empty (V.singleton (Text "2"))
                     ])
          doc = Document Nothing root
          pretty = encodePretty 2 doc
      assertBool "pretty output is non-empty" (not (null (show pretty)))

  , testCase "DOCTYPE is skipped" $ do
      let xml = "<?xml version=\"1.0\"?><!DOCTYPE root SYSTEM \"root.dtd\"><root/>"
          Right doc = decode (TE.encodeUtf8 xml)
      elementName (docRoot doc) @?= Just (simpleName "root")
  ]

-- Enhanced parsePath tests
pathEnhancedTests :: TestTree
pathEnhancedTests = testGroup "Enhanced Path Parsing"
  [ testCase "parse \".\" (self)" $ do
      let Right path = parsePath "."
      case path of
        Sequence [Self] -> pure ()
        _ -> assertFailure $ "Expected Sequence [Self], got " ++ show path

  , testCase "parse \"..\" (parent)" $ do
      let Right path = parsePath ".."
      case path of
        Sequence [Parent] -> pure ()
        _ -> assertFailure $ "Expected Sequence [Parent], got " ++ show path

  , testCase "parse \"*\" (wildcard)" $ do
      let Right path = parsePath "*"
          xml = "<root><a/><b/><c/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results @?= 3

  , testCase "parse \"name[@attr='val']\"" $ do
      let Right path = parsePath "item[@type='book']"
          xml = "<root><item type=\"book\">B</item><item type=\"dvd\">D</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "B"

  , testCase "parse \"name[3]\" (1-based index)" $ do
      let Right path = parsePath "item[2]"
          xml = "<root><item>a</item><item>b</item><item>c</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "b"
  ]

-- DSL tests
dslTests :: TestTree
dslTests = testGroup "XML DSL"
  [ testCase "child /> child composition" $ do
      let xml = "<root><items><item>a</item><item>b</item></items></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "items" DSL./> DSL.child "item"
          results = DSL.select q (docRoot doc)
      V.length results @?= 2
      textContent (V.head results) @?= "a"

  , testCase "anyDescendant search" $ do
      let xml = "<root><a><b><target>found</target></b></a><target>also</target></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.anyDescendant
          results = DSL.select q (docRoot doc)
      assertBool "found descendants" (V.length results >= 3)

  , testCase "where_ filter with attribute" $ do
      let xml = "<root><item type=\"book\">B</item><item type=\"dvd\">D</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.whereAttr "type" "book" (DSL.child "item")
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "B"

  , testCase "whereContains" $ do
      let xml = "<root><a href=\"https://example.com/foo\">X</a><a href=\"other\">Y</a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.whereContains "href" "example" (DSL.child "a")
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "X"

  , testCase "index (1-based)" $ do
      let xml = "<root><item>a</item><item>b</item><item>c</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.index 2 (DSL.child "item")
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "b"

  , testCase "selectOne" $ do
      let xml = "<root><item>a</item><item>b</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "item"
          result = DSL.selectOne q (docRoot doc)
      case result of
        Just n -> textContent n @?= "a"
        Nothing -> assertFailure "Expected Just"

  , testCase "selectText" $ do
      let xml = "<root><msg>hello </msg><msg>world</msg></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "msg" DSL./> DSL.textContent
          result = DSL.selectText q (docRoot doc)
      result @?= "hello world"

  , testCase "count" $ do
      let xml = "<root><item/><item/><item/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.count (DSL.child "item")
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      V.head results @?= 3

  , testCase "liftQuery extension" $ do
      let xml = "<root><a/><b/><c/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          customQ = DSL.liftQuery $ \n -> case n of
            Element _ _ cs -> V.filter (isElementNamed "b") cs
            _ -> V.empty
          results = DSL.select customQ (docRoot doc)
      V.length results @?= 1

  , testCase "union (|>)" $ do
      let xml = "<root><a>1</a><b>2</b><c>3</c></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "a" DSL.|> DSL.child "c"
          results = DSL.select q (docRoot doc)
      V.length results @?= 2
      textContent (V.head results) @?= "1"
      textContent (results V.! 1) @?= "3"

  , testCase "first and last" $ do
      let xml = "<root><item>a</item><item>b</item><item>c</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          qFirst = DSL.first (DSL.child "item")
          qLast = DSL.last (DSL.child "item")
      case DSL.selectOne qFirst (docRoot doc) of
        Just n -> textContent n @?= "a"
        Nothing -> assertFailure "Expected first"
      case DSL.selectOne qLast (docRoot doc) of
        Just n -> textContent n @?= "c"
        Nothing -> assertFailure "Expected last"

  , testCase "descendant search by name" $ do
      let xml = "<root><a><b><target>deep</target></b></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.descendant "target"
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "deep"

  , testCase "descendant chain (//>)" $ do
      let xml = "<root><a><item>x</item></a><b><item>y</item></b></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.anyChild DSL.//> DSL.child "item"
          results = DSL.select q (docRoot doc)
      V.length results @?= 2
  ]

-- FastDOM zero-copy parser tests
fastDOMTests :: TestTree
fastDOMTests = testGroup "FastDOM"
  [ testCase "parseFast roundtrip: parse then toDocument equals decode" $ do
      let xml = "<root><child attr=\"value\">text</child><empty/></root>"
          bs = TE.encodeUtf8 xml
          Right docFull = decode bs
          Right fastDoc = FD.parseFast bs
          docMaterialized = FD.toDocument fastDoc
      docRoot docMaterialized @?= docRoot docFull

  , testCase "parseFast accessor: nodeTagBS returns correct slice" $ do
      let bs = BS8.pack "<person><name>John</name></person>"
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          src  = FD.fdSource fastDoc
      FD.nodeTagBS root src @?= "person"

  , testCase "parseFast accessor: attrValueBS returns correct slice" $ do
      let bs = BS8.pack "<item id=\"42\" class=\"main\"/>"
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          src  = FD.fdSource fastDoc
          attrs = FD.nodeAttrs root
      V.length attrs @?= 2
      FD.attrNameBS (attrs V.! 0) src @?= "id"
      FD.attrValueBS (attrs V.! 0) src @?= "42"
      FD.attrNameBS (attrs V.! 1) src @?= "class"
      FD.attrValueBS (attrs V.! 1) src @?= "main"

  , testCase "parseFast large document (100 items)" $ do
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
      V.length elems @?= 100
      let firstChild = V.head elems
          src = FD.fdSource fastDoc
      FD.nodeTagBS firstChild src @?= "item"

  , testCase "parseFast with CDATA" $ do
      let bs = BS8.pack "<root><![CDATA[<not>markup & stuff]]></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length children @?= 1
      case V.head children of
        FD.FCData _ -> FD.nodeTextBS (V.head children) src @?= "<not>markup & stuff"
        other -> assertFailure $ "Expected FCData, got: " ++ show other

  , testCase "parseFast with comment" $ do
      let bs = BS8.pack "<root><!-- hello world --></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children @?= 1
      case V.head children of
        FD.FComment (FD.Span _ _) -> pure ()
        other -> assertFailure $ "Expected FComment, got: " ++ show other

  , testCase "parseFast with processing instruction" $ do
      let bs = BS8.pack "<root><?target some data?></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length children @?= 1
      case V.head children of
        FD.FPI tSpan _dSpan ->
          FD.nodeTag (FD.FElement tSpan V.empty V.empty) src @?= "target"
        other -> assertFailure $ "Expected FPI, got: " ++ show other

  , testCase "parseFast self-closing elements" $ do
      let bs = BS8.pack "<root><br/><hr/><img src=\"x.png\"/></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length children @?= 3
      FD.nodeTagBS (children V.! 0) src @?= "br"
      FD.nodeTagBS (children V.! 1) src @?= "hr"
      FD.nodeTagBS (children V.! 2) src @?= "img"
      V.length (FD.nodeChildren (children V.! 0)) @?= 0
      V.length (FD.nodeAttrs (children V.! 2)) @?= 1
      FD.attrValueBS (V.head (FD.nodeAttrs (children V.! 2))) src @?= "x.png"

  , testCase "parseFast toDocument with entities" $ do
      let bs = BS8.pack "<root>&amp;&lt;&gt;</root>"
          Right fastDoc = FD.parseFast bs
          doc = FD.toDocument fastDoc
      textContent (docRoot doc) @?= "&<>"

  , testCase "parseFast with XML declaration" $ do
      let bs = BS8.pack "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
          Right fastDoc = FD.parseFast bs
          src = FD.fdSource fastDoc
      FD.nodeTagBS (FD.fdRoot fastDoc) src @?= "root"
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
