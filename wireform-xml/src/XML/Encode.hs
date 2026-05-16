{-# LANGUAGE BangPatterns #-}

{- | XML DOM serializer.

Serializes a 'Document' or 'Node' to 'ByteString' or 'Text'.
Uses direct buffer construction for speed: pre-computes output size,
allocates once, writes in a single pass.
-}
module XML.Encode (
  encode,
  encodeText,
  encodePretty,
  encodeNode,
  encodeNodeText,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Wireform.Builder (Builder)
import Wireform.Builder qualified as B
import XML.Value


-- | Serialize a Document to ByteString.
encode :: Document -> ByteString
encode doc = BL.toStrict (B.toLazyByteString (buildDocument doc))


-- | Serialize to Text.
encodeText :: Document -> Text
encodeText doc = TL.toStrict (TLE.decodeUtf8 (B.toLazyByteString (buildDocument doc)))


-- | Pretty-print with indentation (indent = number of spaces per level).
encodePretty :: Int -> Document -> ByteString
encodePretty indent doc = BL.toStrict (B.toLazyByteString (buildDocumentPretty indent doc))


-- | Serialize just a Node.
encodeNode :: Node -> ByteString
encodeNode node = BL.toStrict (B.toLazyByteString (buildNode node))


-- | Serialize just a Node to Text.
encodeNodeText :: Node -> Text
encodeNodeText node = TL.toStrict (TLE.decodeUtf8 (B.toLazyByteString (buildNode node)))


buildDocument :: Document -> Builder
buildDocument (Document mDecl root) =
  maybe mempty buildXMLDecl mDecl <> buildNode root


buildDocumentPretty :: Int -> Document -> Builder
buildDocumentPretty indent (Document mDecl root) =
  maybe mempty buildXMLDecl mDecl <> buildNodePretty indent 0 root


buildXMLDecl :: XMLDecl -> Builder
buildXMLDecl (XMLDecl ver mEnc mSa) =
  B.string7 "<?xml version=\""
    <> B.byteString (TE.encodeUtf8 ver)
    <> B.char7 '"'
    <> maybe mempty (\e -> B.string7 " encoding=\"" <> B.byteString (TE.encodeUtf8 e) <> B.char7 '"') mEnc
    <> maybe mempty (\s -> B.string7 " standalone=\"" <> B.string7 (if s then "yes" else "no") <> B.char7 '"') mSa
    <> B.string7 "?>"


buildNode :: Node -> Builder
buildNode (Element name attrs children)
  | V.null children =
      B.char7 '<' <> buildName name <> buildAttrs attrs <> B.string7 "/>"
  | otherwise =
      B.char7 '<'
        <> buildName name
        <> buildAttrs attrs
        <> B.char7 '>'
        <> V.foldl' (\acc c -> acc <> buildNode c) mempty children
        <> B.string7 "</"
        <> buildName name
        <> B.char7 '>'
buildNode (Text t) = escapeXMLText t
buildNode (CData t) = B.string7 "<![CDATA[" <> B.byteString (TE.encodeUtf8 t) <> B.string7 "]]>"
buildNode (Comment t) = B.string7 "<!--" <> B.byteString (TE.encodeUtf8 t) <> B.string7 "-->"
buildNode (ProcessingInstruction target content) =
  B.string7 "<?"
    <> B.byteString (TE.encodeUtf8 target)
    <> (if T.null content then mempty else B.char7 ' ' <> B.byteString (TE.encodeUtf8 content))
    <> B.string7 "?>"


buildNodePretty :: Int -> Int -> Node -> Builder
buildNodePretty indent level (Element name attrs children)
  | V.null children =
      indentB indent level
        <> B.char7 '<'
        <> buildName name
        <> buildAttrs attrs
        <> B.string7 "/>\n"
  | V.length children == 1 && isTextLike (V.head children) =
      indentB indent level
        <> B.char7 '<'
        <> buildName name
        <> buildAttrs attrs
        <> B.char7 '>'
        <> buildNode (V.head children)
        <> B.string7 "</"
        <> buildName name
        <> B.string7 ">\n"
  | otherwise =
      indentB indent level
        <> B.char7 '<'
        <> buildName name
        <> buildAttrs attrs
        <> B.string7 ">\n"
        <> V.foldl' (\acc c -> acc <> buildNodePretty indent (level + 1) c) mempty children
        <> indentB indent level
        <> B.string7 "</"
        <> buildName name
        <> B.string7 ">\n"
buildNodePretty indent level (Text t) =
  indentB indent level <> escapeXMLText t <> B.char7 '\n'
buildNodePretty indent level node =
  indentB indent level <> buildNode node <> B.char7 '\n'


isTextLike :: Node -> Bool
isTextLike (Text _) = True
isTextLike (CData _) = True
isTextLike _ = False


indentB :: Int -> Int -> Builder
indentB indent level =
  let !n = indent * level
  in mconcat (replicate n (B.char7 ' '))


buildName :: Name -> Builder
buildName (Name local mPrefix _) =
  case mPrefix of
    Nothing -> B.byteString (TE.encodeUtf8 local)
    Just pfx -> B.byteString (TE.encodeUtf8 pfx) <> B.char7 ':' <> B.byteString (TE.encodeUtf8 local)


buildAttrs :: Vector Attribute -> Builder
buildAttrs attrs = V.foldl' (\acc a -> acc <> buildAttr a) mempty attrs


buildAttr :: Attribute -> Builder
buildAttr (Attribute name val) =
  B.char7 ' ' <> buildName name <> B.string7 "=\"" <> escapeXMLAttr val <> B.char7 '"'


escapeXMLText :: Text -> Builder
escapeXMLText t = T.foldl' (\acc c -> acc <> escChar c) mempty t
  where
    escChar '<' = B.string7 "&lt;"
    escChar '>' = B.string7 "&gt;"
    escChar '&' = B.string7 "&amp;"
    escChar c = B.charUtf8 c


escapeXMLAttr :: Text -> Builder
escapeXMLAttr t = T.foldl' (\acc c -> acc <> escChar c) mempty t
  where
    escChar '<' = B.string7 "&lt;"
    escChar '>' = B.string7 "&gt;"
    escChar '&' = B.string7 "&amp;"
    escChar '"' = B.string7 "&quot;"
    escChar '\'' = B.string7 "&apos;"
    escChar c = B.charUtf8 c
