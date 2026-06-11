{- | DOM parser — builds an XML tree from SAX events.

Uses 'XML.SAX.parseSAX' internally and folds the event stream into
a 'Document' via an element stack.
-}
module XML.Decode (
  decode,
  decodeText,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import XML.SAX (SAXEvent (..), parseSAX)
import XML.Value


-- | Parse XML bytes into a DOM Document.
decode :: ByteString -> Either String Document
decode bs = do
  events <- parseSAX bs
  buildDocument events


-- | Parse XML Text into a DOM Document.
decodeText :: Text -> Either String Document
decodeText = decode . TE.encodeUtf8


data BuildState = BuildState
  { bsStack :: ![StackFrame]
  , bsDecl :: !(Maybe XMLDecl)
  , bsRoot :: !(Maybe Node)
  }


data StackFrame = StackFrame
  { sfName :: !Name
  , sfAttrs :: !(Vector Attribute)
  , sfChildren :: ![Node]
  }


buildDocument :: Vector SAXEvent -> Either String Document
buildDocument events = go (BuildState [] Nothing Nothing) 0
  where
    !len = V.length events
    go !st !i
      | i >= len =
          case bsRoot st of
            Nothing -> Left "No root element found"
            Just root -> Right (Document (bsDecl st) root)
      | otherwise =
          case events V.! i of
            StartDocument mDecl ->
              go (st {bsDecl = mDecl}) (i + 1)
            EndDocument ->
              go st (i + 1)
            StartElement name attrs ->
              let !frame = StackFrame name attrs []
              in go (st {bsStack = frame : bsStack st}) (i + 1)
            EndElement _name ->
              case bsStack st of
                [] -> Left "EndElement without matching StartElement"
                (frame : rest) ->
                  let !node =
                        Element
                          (sfName frame)
                          (sfAttrs frame)
                          (V.fromList (reverse (sfChildren frame)))
                  in case rest of
                       [] -> go (st {bsStack = [], bsRoot = Just node}) (i + 1)
                       (parent : grandparents) ->
                         let !parent' = parent {sfChildren = node : sfChildren parent}
                         in go (st {bsStack = parent' : grandparents}) (i + 1)
            Characters txt ->
              case bsStack st of
                [] -> go st (i + 1)
                (frame : rest) ->
                  let !frame' = frame {sfChildren = Text txt : sfChildren frame}
                  in go (st {bsStack = frame' : rest}) (i + 1)
            CDATASection txt ->
              case bsStack st of
                [] -> go st (i + 1)
                (frame : rest) ->
                  let !frame' = frame {sfChildren = CData txt : sfChildren frame}
                  in go (st {bsStack = frame' : rest}) (i + 1)
            CommentEvent txt ->
              case bsStack st of
                [] -> go st (i + 1)
                (frame : rest) ->
                  let !frame' = frame {sfChildren = Comment txt : sfChildren frame}
                  in go (st {bsStack = frame' : rest}) (i + 1)
            PI target content ->
              case bsStack st of
                [] -> go st (i + 1)
                (frame : rest) ->
                  let !frame' = frame {sfChildren = ProcessingInstruction target content : sfChildren frame}
                  in go (st {bsStack = frame' : rest}) (i + 1)
