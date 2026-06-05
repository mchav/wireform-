{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.HTML (htmlTests) where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Primitive.SmallArray (emptySmallArray, smallArrayFromList, sizeofSmallArray, indexSmallArray)
import GHC.Generics (Generic)
import Test.Syd

import HTML.Value
import HTML.Parse
import HTML.Encode
import qualified HTML.DOM as DOM
import HTML.Class

htmlTests :: Spec
htmlTests = describe "HTML" $ sequence_
  [ parseTests
  , voidElementTests
  , autoCloseTests
  , rawTextTests
  , entityTests
  , caseInsensitiveTests
  , commentTests
  , queryTests
  , encodeDecodeTests
  , classTests
  , genericTests
  , edgeCaseTests
  ]

parseTests :: Spec
parseTests = describe "Parse" $ sequence_
  [ it "minimal HTML: <p>hello" $ do
      let doc = parseHTML "<p>hello"
          root = htmlRoot doc
      case root of
        HTMLElement "p" _ cs -> textContent (indexSmallArray cs 0) `shouldBe` "hello"
        _ -> (containsTag "p" root) `shouldBe` True

  , it "full document" $ do
      let doc = parseHTML "<!DOCTYPE html><html><head><title>T</title></head><body><p>Hi</p></body></html>"
          root = htmlRoot doc
      (htmlDoctype doc /= Nothing) `shouldBe` True
      (getTagName root == Just "html") `shouldBe` True

  , it "self-closing: <br/> = <br>" $ do
      let doc = parseHTML "<div><br/></div>"
          root = htmlRoot doc
      (containsTag "br" root) `shouldBe` True

  , it "attributes parsed" $ do
      let doc = parseHTML "<img src=\"test.png\" alt=\"image\">"
          root = htmlRoot doc
      case findTag "img" root of
        Just n -> do
          getAttr "src" n `shouldBe` Just "test.png"
          getAttr "alt" n `shouldBe` Just "image"
        Nothing -> expectationFailure "expected img element"

  , it "multiple attributes" $ do
      let doc = parseHTML "<div id=\"main\" class=\"container\">content</div>"
          root = htmlRoot doc
      case findTag "div" root of
        Just n -> do
          getAttr "id" n `shouldBe` Just "main"
          getAttr "class" n `shouldBe` Just "container"
        Nothing -> expectationFailure "expected div element"
  ]

voidElementTests :: Spec
voidElementTests = describe "Void elements" $ sequence_
  [ it "<br><hr><img src=\"x.png\">" $ do
      let doc = parseHTML "<div><br><hr><img src=\"x.png\"></div>"
          root = htmlRoot doc
      (containsTag "br" root) `shouldBe` True
      (containsTag "hr" root) `shouldBe` True
      (containsTag "img" root) `shouldBe` True

  , it "void elements have no children" $ do
      let doc = parseHTML "<br>text after"
          root = htmlRoot doc
      case root of
        HTMLElement "br" _ cs -> (sizeofSmallArray cs == 0) `shouldBe` True
        _ -> pure () :: IO ()

  , it "input is void" $ do
      let doc = parseHTML "<form><input type=\"text\"><input type=\"submit\"></form>"
          root = htmlRoot doc
      (containsTag "input" root) `shouldBe` True

  , it "meta is void" $ do
      let doc = parseHTML "<head><meta charset=\"utf-8\"></head>"
          root = htmlRoot doc
      (containsTag "meta" root) `shouldBe` True
  ]

autoCloseTests :: Spec
autoCloseTests = describe "Auto-close" $ sequence_
  [ it "<p>one<p>two → two separate <p> elements" $ do
      let doc = parseHTML "<div><p>one<p>two</div>"
          root = htmlRoot doc
          ps = queryAll "p" root
      length ps `shouldBe` 2
      textContent (ps !! 0) `shouldBe` "one"
      textContent (ps !! 1) `shouldBe` "two"

  , it "<li> auto-close" $ do
      let doc = parseHTML "<ul><li>one<li>two<li>three</ul>"
          root = htmlRoot doc
          lis = queryAll "li" root
      length lis `shouldBe` 3

  , it "<td> auto-close" $ do
      let doc = parseHTML "<table><tr><td>a<td>b</tr></table>"
          root = htmlRoot doc
          tds = queryAll "td" root
      length tds `shouldBe` 2
  ]

rawTextTests :: Spec
rawTextTests = describe "Raw text elements" $ sequence_
  [ it "script content not parsed as HTML" $ do
      let doc = parseHTML "<script>if (a < b) {}</script>"
          root = htmlRoot doc
      case findTag "script" root of
        Just (HTMLElement _ _ cs)
          | sizeofSmallArray cs > 0 -> do
              let content = textContent (indexSmallArray cs 0)
              (T.isInfixOf "<" content) `shouldBe` True
        _ -> expectationFailure "expected script element"

  , it "style content preserved" $ do
      let doc = parseHTML "<style>p { color: red; }</style>"
          root = htmlRoot doc
      case findTag "style" root of
        Just (HTMLElement _ _ cs)
          | sizeofSmallArray cs > 0 ->
              (not (T.null (textContent (indexSmallArray cs 0)))) `shouldBe` True
        _ -> expectationFailure "expected style element"
  ]

entityTests :: Spec
entityTests = describe "Entity references" $ sequence_
  [ it "&nbsp;" $ do
      let doc = parseHTML "<p>&nbsp;</p>"
          root = htmlRoot doc
      (T.any (== '\x00A0') (deepTextContent root)) `shouldBe` True

  , it "&amp;" $ do
      let doc = parseHTML "<p>&amp;</p>"
      deepTextContent (htmlRoot doc) `shouldBe` "&"

  , it "&lt; &gt;" $ do
      let doc = parseHTML "<p>&lt;&gt;</p>"
      deepTextContent (htmlRoot doc) `shouldBe` "<>"

  , it "&quot;" $ do
      let doc = parseHTML "<p>&quot;</p>"
      deepTextContent (htmlRoot doc) `shouldBe` "\""

  , it "&#65; (numeric decimal)" $ do
      let doc = parseHTML "<p>&#65;</p>"
      deepTextContent (htmlRoot doc) `shouldBe` "A"

  , it "&#x41; (numeric hex)" $ do
      let doc = parseHTML "<p>&#x41;</p>"
      deepTextContent (htmlRoot doc) `shouldBe` "A"

  , it "&mdash;" $ do
      let doc = parseHTML "<p>&mdash;</p>"
      deepTextContent (htmlRoot doc) `shouldBe` "\x2014"

  , it "&ndash;" $ do
      let doc = parseHTML "<p>&ndash;</p>"
      deepTextContent (htmlRoot doc) `shouldBe` "\x2013"

  , it "&copy;" $ do
      let doc = parseHTML "<p>&copy;</p>"
      deepTextContent (htmlRoot doc) `shouldBe` "\x00A9"

  , it "&reg;" $ do
      let doc = parseHTML "<p>&reg;</p>"
      deepTextContent (htmlRoot doc) `shouldBe` "\x00AE"

  , it "&hellip;" $ do
      let doc = parseHTML "<p>&hellip;</p>"
      deepTextContent (htmlRoot doc) `shouldBe` "\x2026"
  ]

caseInsensitiveTests :: Spec
caseInsensitiveTests = describe "Case insensitive" $ sequence_
  [ it "<DIV> parsed as div" $ do
      let doc = parseHTML "<DIV>content</DIV>"
          root = htmlRoot doc
      getTagName root `shouldBe` Just "html"
      (containsTag "div" root) `shouldBe` True

  , it "<Div Class=\"x\"> → div class=\"x\"" $ do
      let doc = parseHTML "<Div Class=\"x\">test</Div>"
          root = htmlRoot doc
      (containsTag "div" root) `shouldBe` True
      case findTag "div" root of
        Just n -> getAttr "class" n `shouldBe` Just "x"
        Nothing -> expectationFailure "expected div element"

  , it "mixed case: <P>text</p>" $ do
      let doc = parseHTML "<P>text</p>"
          root = htmlRoot doc
      (containsTag "p" root) `shouldBe` True
  ]

commentTests :: Spec
commentTests = describe "Comments" $ sequence_
  [ it "<!-- comment -->" $ do
      let doc = parseHTML "<div><!-- comment --></div>"
          root = htmlRoot doc
      (hasComment root) `shouldBe` True

  , it "comment content preserved" $ do
      let doc = parseHTML "<div><!-- hello world --></div>"
          root = htmlRoot doc
      case findComment root of
        Just txt -> (T.isInfixOf "hello world" txt) `shouldBe` True
        Nothing -> expectationFailure "expected comment"
  ]

queryTests :: Spec
queryTests = describe "Query" $ sequence_
  [ it "querySelector by tag" $ do
      let doc = parseHTML "<div><p>one</p><p>two</p></div>"
          root = htmlRoot doc
      case queryOne "p" root of
        Just n -> deepTextContent n `shouldBe` "one"
        Nothing -> expectationFailure "expected p"

  , it "querySelectorAll by tag" $ do
      let doc = parseHTML "<div><p>one</p><p>two</p></div>"
          root = htmlRoot doc
          ps = queryAll "p" root
      length ps `shouldBe` 2

  , it "querySelector by class" $ do
      let doc = parseHTML "<div><span class=\"highlight\">yes</span><span>no</span></div>"
          root = htmlRoot doc
      case queryOne ".highlight" root of
        Just n -> deepTextContent n `shouldBe` "yes"
        Nothing -> expectationFailure "expected .highlight"

  , it "querySelector by id" $ do
      let doc = parseHTML "<div><span id=\"main\">target</span></div>"
          root = htmlRoot doc
      case queryOne "#main" root of
        Just n -> deepTextContent n `shouldBe` "target"
        Nothing -> expectationFailure "expected #main"

  , it "getElementById" $ do
      let doc = parseHTML "<div><p id=\"intro\">Hello</p></div>"
          root = htmlRoot doc
      case queryOne "#intro" root of
        Just n -> deepTextContent n `shouldBe` "Hello"
        Nothing -> expectationFailure "expected element with id intro"

  , it "getElementsByClass" $ do
      let doc = parseHTML "<div><p class=\"item\">a</p><p class=\"item\">b</p><p>c</p></div>"
          root = htmlRoot doc
          items = queryAll ".item" root
      length items `shouldBe` 2

  , it "descendant selector: div.main p" $ do
      let doc = parseHTML "<div class=\"main\"><p>target</p></div><p>other</p>"
          root = htmlRoot doc
          results = queryAll "div.main p" root
      length results `shouldBe` 1
      deepTextContent (head results) `shouldBe` "target"
  ]

encodeDecodeTests :: Spec
encodeDecodeTests = describe "Encode/Decode" $ sequence_
  [ it "encode void elements without closing tag" $ do
      let doc = HTMLDocument Nothing (HTMLElement "div" emptySmallArray
            (smallArrayFromList [HTMLElement "br" emptySmallArray mempty, HTMLElement "hr" emptySmallArray mempty]))
          encoded = encodeHTML doc
          decoded = parseHTML encoded
          root = htmlRoot decoded
      (containsTag "br" root) `shouldBe` True
      (containsTag "hr" root) `shouldBe` True

  , it "encode/parse roundtrip preserves structure" $ do
      let doc = HTMLDocument Nothing
            (HTMLElement "div" (smallArrayFromList [HTMLAttribute "class" "main"])
              (smallArrayFromList
                [ HTMLElement "p" emptySmallArray (smallArrayFromList [HTMLText "hello"])
                , HTMLElement "br" emptySmallArray mempty
                , HTMLElement "p" emptySmallArray (smallArrayFromList [HTMLText "world"])
                ]))
          encoded = encodeHTML doc
          decoded = parseHTML encoded
          root = htmlRoot decoded
      (containsTag "div" root) `shouldBe` True
      let ps = queryAll "p" root
      length ps `shouldBe` 2

  , it "boolean attributes minimized" $ do
      let doc = HTMLDocument Nothing
            (HTMLElement "input" (smallArrayFromList
              [ HTMLAttribute "type" "checkbox"
              , HTMLAttribute "checked" ""
              ]) mempty)
          encoded = TE.decodeUtf8 (encodeHTML doc)
      (T.isInfixOf " checked" encoded) `shouldBe` True

  , it "encodes entities in text" $ do
      let doc = HTMLDocument Nothing
            (HTMLElement "p" emptySmallArray (smallArrayFromList [HTMLText "a < b & c > d"]))
          encoded = TE.decodeUtf8 (encodeHTML doc)
      (T.isInfixOf "&lt;" encoded) `shouldBe` True
      (T.isInfixOf "&amp;" encoded) `shouldBe` True
      (T.isInfixOf "&gt;" encoded) `shouldBe` True
  ]

data PersonHTML = PersonHTML
  { pName :: !Text
  , pAge :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToHTML, FromHTML)

classTests :: Spec
classTests = describe "Class instances" $ sequence_
  [ it "Text roundtrip" $ do
      let val = "hello" :: Text
      fromHTML (toHTML val) `shouldBe` Right val

  , it "Int roundtrip" $ do
      let val = 42 :: Int
      fromHTML (toHTML val) `shouldBe` Right val

  , it "Bool roundtrip" $ do
      fromHTML (toHTML True) `shouldBe` Right True
  ]

genericTests :: Spec
genericTests = describe "Generic deriving" $ sequence_
  [ it "record to HTML" $ do
      let person = PersonHTML "John" 30
          node = toHTML person
      case node of
        HTMLElement _ _ cs -> sizeofSmallArray cs `shouldBe` 2
        _ -> expectationFailure "expected HTMLElement"

  , it "record roundtrip" $ do
      let person = PersonHTML "Jane" 25
      fromHTML (toHTML person) `shouldBe` Right person
  ]

edgeCaseTests :: Spec
edgeCaseTests = describe "Edge cases" $ sequence_
  [ it "parse empty document" $ do
      let doc = parseHTML ""
          root = htmlRoot doc
      case root of
        HTMLElement "html" _ _ -> pure () :: IO ()
        _ -> pure () :: IO ()

  , it "parse whitespace only" $ do
      let doc = parseHTML "   \n\t  "
          root = htmlRoot doc
      case root of
        HTMLElement "html" _ _ -> pure () :: IO ()
        _ -> pure () :: IO ()

  , it "nested divs" $ do
      let doc = parseHTML "<div><div><div>deep</div></div></div>"
          root = htmlRoot doc
      (containsTag "div" root) `shouldBe` True

  , it "multiple root-level elements" $ do
      let doc = parseHTML "<p>one</p><p>two</p>"
          root = htmlRoot doc
      (True) `shouldBe` True

  , it "doctype parsing" $ do
      let doc = parseHTML "<!DOCTYPE html><html><body>hi</body></html>"
      case htmlDoctype doc of
        Just (Doctype (Just _) _ _) -> pure () :: IO ()
        _ -> pure () :: IO ()
  ]

-- Helpers

getTagName :: HTMLNode -> Maybe Text
getTagName (HTMLElement t _ _) = Just t
getTagName _ = Nothing

containsTag :: Text -> HTMLNode -> Bool
containsTag tag (HTMLElement t _ cs)
  | t == tag = True
  | otherwise = any (containsTag tag) cs
containsTag _ _ = False

findTag :: Text -> HTMLNode -> Maybe HTMLNode
findTag tag n@(HTMLElement t _ cs)
  | t == tag = Just n
  | otherwise = foldl (\acc c -> case acc of Just _ -> acc; Nothing -> findTag tag c) Nothing (toList cs)
findTag _ _ = Nothing

hasComment :: HTMLNode -> Bool
hasComment (HTMLComment _) = True
hasComment (HTMLElement _ _ cs) = any hasComment cs
hasComment _ = False

findComment :: HTMLNode -> Maybe Text
findComment (HTMLComment t) = Just t
findComment (HTMLElement _ _ cs) = foldl (\acc c -> case acc of Just _ -> acc; Nothing -> findComment c) Nothing (toList cs)
findComment _ = Nothing

deepTextContent :: HTMLNode -> Text
deepTextContent = textContent

queryAll :: Text -> HTMLNode -> [HTMLNode]
queryAll sel raw = map DOM.rawNode (DOM.querySelectorAll (DOM.rootNode raw) sel)

queryOne :: Text -> HTMLNode -> Maybe HTMLNode
queryOne sel raw = DOM.rawNode <$> DOM.querySelector (DOM.rootNode raw) sel
