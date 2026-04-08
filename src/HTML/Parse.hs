{-# LANGUAGE BangPatterns #-}
-- | Permissive HTML5 parser.
--
-- Handles the key differences from XML:
-- * Void elements (br, hr, img, …) — no closing tag required
-- * Optional closing tags (p, li, …) — auto-closed by incompatible nesting
-- * Raw text elements (script, style) — content passed through unparsed
-- * Self-closing tags (<br/>) treated same as <br>
-- * Case-insensitive tag/attribute names (stored lowercase)
-- * Named entity references (&amp;, &nbsp;, &#NNN;, &#xHHH;)
-- * Error recovery — permissive parsing
module HTML.Parse
  ( parseHTML
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (chr, digitToInt, isDigit, isHexDigit, toLower, isAlphaNum)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)

import HTML.Value

parseHTML :: ByteString -> HTMLDocument
parseHTML bs =
  let !nodes = parseNodes bs 0
      (!doctype, !root) = extractDocAndRoot nodes
  in HTMLDocument doctype root

extractDocAndRoot :: [HTMLNode] -> (Maybe Doctype, HTMLNode)
extractDocAndRoot nodes =
  let (dts, rest) = span isDoctypeOrWhitespace nodes
      !dt = case [d | HTMLDoctype d <- dts] of
              (d:_) -> Just (Doctype (Just d) Nothing Nothing)
              []    -> Nothing
      !elements = filter isElement rest
  in case elements of
       (e:_) -> (dt, e)
       [] ->
         let !allContent = filter (not . isDoctypeOrWhitespace) nodes
         in case allContent of
              [] -> (dt, HTMLElement "html" V.empty V.empty)
              _  -> (dt, HTMLElement "html" V.empty (V.fromList allContent))

isDoctypeOrWhitespace :: HTMLNode -> Bool
isDoctypeOrWhitespace (HTMLDoctype _) = True
isDoctypeOrWhitespace (HTMLText t) = T.all (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t') t
isDoctypeOrWhitespace _ = False

isElement :: HTMLNode -> Bool
isElement (HTMLElement _ _ _) = True
isElement _ = False

type Offset = Int

parseNodes :: ByteString -> Offset -> [HTMLNode]
parseNodes bs = go []
  where
    !bsLen = BS.length bs
    go !acc !off
      | off >= bsLen = reverse acc
      | BSU.unsafeIndex bs off == 0x3C = -- '<'
          case parseTag bs (off + 1) of
            TagOpen tag attrs selfClose off' ->
              if isVoidElement tag || selfClose
                then go (HTMLElement tag (V.fromList attrs) V.empty : acc) off'
                else if isRawTextElement tag
                  then let (content, off'') = parseRawText bs off' tag
                       in go (HTMLElement tag (V.fromList attrs) (V.singleton (HTMLText content)) : acc) off''
                  else let (children, off'') = parseChildren bs off' tag
                       in go (HTMLElement tag (V.fromList attrs) (V.fromList children) : acc) off''
            TagClose _ off' -> go acc off'
            TagComment txt off' -> go (HTMLComment txt : acc) off'
            TagDoctype txt off' -> go (HTMLDoctype txt : acc) off'
            TagError off' -> go acc off'
      | otherwise =
          let (txt, off') = parseText bs off
          in if T.null txt
               then go acc off'
               else go (HTMLText txt : acc) off'

data TagResult
  = TagOpen !Text ![HTMLAttribute] !Bool !Offset
  | TagClose !Text !Offset
  | TagComment !Text !Offset
  | TagDoctype !Text !Offset
  | TagError !Offset

parseTag :: ByteString -> Offset -> TagResult
parseTag bs off
  | off >= BS.length bs = TagError off
  | BSU.unsafeIndex bs off == 0x21 = -- '!'
      if off + 2 < BS.length bs
         && BSU.unsafeIndex bs (off + 1) == 0x2D
         && BSU.unsafeIndex bs (off + 2) == 0x2D
        then parseComment bs (off + 3)
        else if matchCaseInsensitive bs (off + 1) "doctype"
          then parseDoctype bs (off + 8)
          else TagError (skipToGt bs off)
  | BSU.unsafeIndex bs off == 0x2F = -- '/'
      let (name, off') = readTagName bs (off + 1)
          !off'' = skipToGt bs off'
      in TagClose (T.toLower name) off''
  | otherwise =
      let (name, off') = readTagName bs off
          !lcName = T.toLower name
      in if T.null lcName
           then TagError (skipToGt bs off')
           else let (attrs, selfClose, off'') = readAttributes bs off'
                in TagOpen lcName attrs selfClose off''

matchCaseInsensitive :: ByteString -> Offset -> String -> Bool
matchCaseInsensitive bs off str = go off str
  where
    !bsLen = BS.length bs
    go !_ [] = True
    go !i (c:cs)
      | i >= bsLen = False
      | toLower (chr (fromIntegral (BSU.unsafeIndex bs i))) == c = go (i + 1) cs
      | otherwise = False

parseComment :: ByteString -> Offset -> TagResult
parseComment bs off = go off
  where
    !bsLen = BS.length bs
    go !i
      | i + 2 >= bsLen = TagComment (decodeSlice bs off (bsLen - off)) bsLen
      | BSU.unsafeIndex bs i == 0x2D
        && BSU.unsafeIndex bs (i + 1) == 0x2D
        && BSU.unsafeIndex bs (i + 2) == 0x3E =
          TagComment (T.strip (decodeSlice bs off (i - off))) (i + 3)
      | otherwise = go (i + 1)

parseDoctype :: ByteString -> Offset -> TagResult
parseDoctype bs off =
  let !off' = skipWS bs off
      !gtPos = skipToGt bs off'
      !content = T.strip (decodeSlice bs off' (gtPos - 1 - off'))
  in TagDoctype content gtPos

readTagName :: ByteString -> Offset -> (Text, Offset)
readTagName bs off = go off
  where
    !bsLen = BS.length bs
    go !i
      | i >= bsLen = (decodeSlice bs off (i - off), i)
      | isTagNameChar (BSU.unsafeIndex bs i) = go (i + 1)
      | otherwise = (decodeSlice bs off (i - off), i)

isTagNameChar :: Word8 -> Bool
isTagNameChar b =
  (b >= 0x61 && b <= 0x7A) || -- a-z
  (b >= 0x41 && b <= 0x5A) || -- A-Z
  (b >= 0x30 && b <= 0x39) || -- 0-9
  b == 0x2D || b == 0x5F || b == 0x3A -- - _ :
{-# INLINE isTagNameChar #-}

readAttributes :: ByteString -> Offset -> ([HTMLAttribute], Bool, Offset)
readAttributes bs = go []
  where
    !bsLen = BS.length bs
    go !acc !off =
      let !off' = skipWS bs off
      in if off' >= bsLen
           then (reverse acc, False, off')
           else case BSU.unsafeIndex bs off' of
             0x3E -> (reverse acc, False, off' + 1) -- '>'
             0x2F | off' + 1 < bsLen && BSU.unsafeIndex bs (off' + 1) == 0x3E -> -- '/>'
                    (reverse acc, True, off' + 2)
             _ ->
               let (name, off1) = readAttrName bs off'
                   !lcName = T.toLower name
               in if T.null lcName
                    then (reverse acc, False, skipToGt bs off1)
                    else let !off2 = skipWS bs off1
                         in if off2 < bsLen && BSU.unsafeIndex bs off2 == 0x3D -- '='
                              then let !off3 = skipWS bs (off2 + 1)
                                       (val, off4) = readAttrValue bs off3
                                   in go (HTMLAttribute lcName val : acc) off4
                              else go (HTMLAttribute lcName T.empty : acc) off2

readAttrName :: ByteString -> Offset -> (Text, Offset)
readAttrName bs off = go off
  where
    !bsLen = BS.length bs
    go !i
      | i >= bsLen = (decodeSlice bs off (i - off), i)
      | isAttrNameChar (BSU.unsafeIndex bs i) = go (i + 1)
      | otherwise = (decodeSlice bs off (i - off), i)

isAttrNameChar :: Word8 -> Bool
isAttrNameChar b =
  b /= 0x20 && b /= 0x09 && b /= 0x0A && b /= 0x0D -- not whitespace
  && b /= 0x3D -- not =
  && b /= 0x3E -- not >
  && b /= 0x2F -- not /
  && b /= 0x3C -- not <
  && b /= 0x22 -- not "
  && b /= 0x27 -- not '
{-# INLINE isAttrNameChar #-}

readAttrValue :: ByteString -> Offset -> (Text, Offset)
readAttrValue bs off
  | off >= BS.length bs = (T.empty, off)
  | BSU.unsafeIndex bs off == 0x22 = readQuotedValue bs (off + 1) 0x22 -- double quote
  | BSU.unsafeIndex bs off == 0x27 = readQuotedValue bs (off + 1) 0x27 -- single quote
  | otherwise = readUnquotedValue bs off

readQuotedValue :: ByteString -> Offset -> Word8 -> (Text, Offset)
readQuotedValue bs off quote = go off []
  where
    !bsLen = BS.length bs
    go !i !acc
      | i >= bsLen = (resolveEntities (decodeSlice bs off (i - off)), i)
      | BSU.unsafeIndex bs i == quote =
          (resolveEntities (decodeSlice bs off (i - off)), i + 1)
      | otherwise = go (i + 1) acc

readUnquotedValue :: ByteString -> Offset -> (Text, Offset)
readUnquotedValue bs off = go off
  where
    !bsLen = BS.length bs
    go !i
      | i >= bsLen = (resolveEntities (decodeSlice bs off (i - off)), i)
      | let b = BSU.unsafeIndex bs i
      , b == 0x20 || b == 0x09 || b == 0x0A || b == 0x0D
        || b == 0x3E || b == 0x2F =
          (resolveEntities (decodeSlice bs off (i - off)), i)
      | otherwise = go (i + 1)

parseText :: ByteString -> Offset -> (Text, Offset)
parseText bs off = go off
  where
    !bsLen = BS.length bs
    go !i
      | i >= bsLen = (resolveEntities (decodeSlice bs off (i - off)), i)
      | BSU.unsafeIndex bs i == 0x3C = -- '<'
          (resolveEntities (decodeSlice bs off (i - off)), i)
      | otherwise = go (i + 1)

parseRawText :: ByteString -> Offset -> Text -> (Text, Offset)
parseRawText bs off tag = go off
  where
    !bsLen = BS.length bs
    !closeTag = TE.encodeUtf8 ("</" <> tag <> ">")
    !closeLen = BS.length closeTag
    go !i
      | i + closeLen > bsLen = (decodeSlice bs off (bsLen - off), bsLen)
      | matchCloseTag bs i tag =
          (decodeSlice bs off (i - off), skipToGt bs (i + 2))
      | otherwise = go (i + 1)

matchCloseTag :: ByteString -> Offset -> Text -> Bool
matchCloseTag bs off tag
  | off + 2 >= BS.length bs = False
  | BSU.unsafeIndex bs off /= 0x3C = False  -- '<'
  | BSU.unsafeIndex bs (off + 1) /= 0x2F = False  -- '/'
  | otherwise = matchCaseInsensitive bs (off + 2) (T.unpack tag)

parseChildren :: ByteString -> Offset -> Text -> ([HTMLNode], Offset)
parseChildren bs off0 parentTag = go [] off0
  where
    !bsLen = BS.length bs
    go !acc !off
      | off >= bsLen = (reverse acc, off)
      | BSU.unsafeIndex bs off == 0x3C
        && off + 1 < bsLen
        && BSU.unsafeIndex bs (off + 1) == 0x2F =
          let (name, off') = readTagName bs (off + 2)
              !lcName = T.toLower name
              !off'' = skipToGt bs off'
          in if lcName == parentTag
               then (reverse acc, off'')
               else go acc off''
      | BSU.unsafeIndex bs off == 0x3C =
          case parseTag bs (off + 1) of
            TagOpen tag attrs selfClose off' ->
              if shouldAutoClose parentTag tag
                then (reverse acc, off)
                else if isVoidElement tag || selfClose
                  then go (HTMLElement tag (V.fromList attrs) V.empty : acc) off'
                  else if isRawTextElement tag
                    then let (content, off'') = parseRawText bs off' tag
                         in go (HTMLElement tag (V.fromList attrs) (V.singleton (HTMLText content)) : acc) off''
                    else let (children, off'') = parseChildren bs off' tag
                         in go (HTMLElement tag (V.fromList attrs) (V.fromList children) : acc) off''
            TagClose _name off' -> (reverse acc, off)
            TagComment txt off' -> go (HTMLComment txt : acc) off'
            TagDoctype txt off' -> go (HTMLDoctype txt : acc) off'
            TagError off' -> go acc off'
      | otherwise =
          let (txt, off') = parseText bs off
          in if T.null txt
               then go acc off'
               else go (HTMLText txt : acc) off'

shouldAutoClose :: Text -> Text -> Bool
shouldAutoClose parent child =
  (parent == "p" && child `elem` pAutoClose)
  || (parent == "li" && child == "li")
  || (parent == "dt" && (child == "dt" || child == "dd"))
  || (parent == "dd" && (child == "dt" || child == "dd"))
  || (parent == "tr" && child == "tr")
  || (parent == "td" && (child == "td" || child == "th"))
  || (parent == "th" && (child == "td" || child == "th"))

pAutoClose :: [Text]
pAutoClose =
  [ "address", "article", "aside", "blockquote", "details", "div"
  , "dl", "fieldset", "figcaption", "figure", "footer", "form"
  , "h1", "h2", "h3", "h4", "h5", "h6", "header", "hgroup", "hr"
  , "main", "menu", "nav", "ol", "p", "pre", "section", "table", "ul"
  ]

skipWS :: ByteString -> Offset -> Offset
skipWS bs = go
  where
    !bsLen = BS.length bs
    go !i
      | i >= bsLen = i
      | let b = BSU.unsafeIndex bs i
      , b == 0x20 || b == 0x09 || b == 0x0A || b == 0x0D = go (i + 1)
      | otherwise = i
{-# INLINE skipWS #-}

skipToGt :: ByteString -> Offset -> Offset
skipToGt bs = go
  where
    !bsLen = BS.length bs
    go !i
      | i >= bsLen = i
      | BSU.unsafeIndex bs i == 0x3E = i + 1 -- '>'
      | otherwise = go (i + 1)
{-# INLINE skipToGt #-}

decodeSlice :: ByteString -> Offset -> Int -> Text
decodeSlice bs off len
  | len <= 0 = T.empty
  | otherwise = TE.decodeUtf8Lenient (BSU.unsafeTake len (BSU.unsafeDrop off bs))

resolveEntities :: Text -> Text
resolveEntities t
  | T.any (== '&') t = T.pack (resolveChars (T.unpack t))
  | otherwise = t

resolveChars :: String -> String
resolveChars [] = []
resolveChars ('&':rest) =
  case break (== ';') rest of
    (entity, ';':after) ->
      case resolveEntity entity of
        Just chars -> chars ++ resolveChars after
        Nothing -> '&' : entity ++ ";" ++ resolveChars after
    _ -> '&' : resolveChars rest
resolveChars (c:rest) = c : resolveChars rest

resolveEntity :: String -> Maybe String
resolveEntity ('#':'x':hex)
  | all isHexDigit hex, not (null hex) =
      Just [chr (foldl' (\a d -> a * 16 + digitToInt d) 0 hex)]
resolveEntity ('#':dec)
  | all isDigit dec, not (null dec) =
      Just [chr (foldl' (\a d -> a * 10 + digitToInt d) 0 dec)]
resolveEntity name = lookup name namedEntities

namedEntities :: [(String, String)]
namedEntities =
  [ ("amp", "&"), ("lt", "<"), ("gt", ">"), ("quot", "\""), ("apos", "'")
  , ("nbsp", "\x00A0"), ("iexcl", "\x00A1"), ("cent", "\x00A2")
  , ("pound", "\x00A3"), ("curren", "\x00A4"), ("yen", "\x00A5")
  , ("brvbar", "\x00A6"), ("sect", "\x00A7"), ("uml", "\x00A8")
  , ("copy", "\x00A9"), ("ordf", "\x00AA"), ("laquo", "\x00AB")
  , ("not", "\x00AC"), ("shy", "\x00AD"), ("reg", "\x00AE")
  , ("macr", "\x00AF"), ("deg", "\x00B0"), ("plusmn", "\x00B1")
  , ("sup2", "\x00B2"), ("sup3", "\x00B3"), ("acute", "\x00B4")
  , ("micro", "\x00B5"), ("para", "\x00B6"), ("middot", "\x00B7")
  , ("cedil", "\x00B8"), ("sup1", "\x00B9"), ("ordm", "\x00BA")
  , ("raquo", "\x00BB"), ("frac14", "\x00BC"), ("frac12", "\x00BD")
  , ("frac34", "\x00BE"), ("iquest", "\x00BF")
  , ("ndash", "\x2013"), ("mdash", "\x2014"), ("lsquo", "\x2018")
  , ("rsquo", "\x2019"), ("ldquo", "\x201C"), ("rdquo", "\x201D")
  , ("bull", "\x2022"), ("hellip", "\x2026"), ("prime", "\x2032")
  , ("Prime", "\x2033"), ("lsaquo", "\x2039"), ("rsaquo", "\x203A")
  , ("oline", "\x203E"), ("euro", "\x20AC"), ("trade", "\x2122")
  , ("larr", "\x2190"), ("uarr", "\x2191"), ("rarr", "\x2192")
  , ("darr", "\x2193"), ("harr", "\x2194"), ("spades", "\x2660")
  , ("clubs", "\x2663"), ("hearts", "\x2665"), ("diams", "\x2666")
  , ("Alpha", "\x0391"), ("Beta", "\x0392"), ("Gamma", "\x0393")
  , ("Delta", "\x0394"), ("Epsilon", "\x0395"), ("Zeta", "\x0396")
  , ("Eta", "\x0397"), ("Theta", "\x0398"), ("Iota", "\x0399")
  , ("Kappa", "\x039A"), ("Lambda", "\x039B"), ("Mu", "\x039C")
  , ("Nu", "\x039D"), ("Xi", "\x039E"), ("Omicron", "\x039F")
  , ("Pi", "\x03A0"), ("Rho", "\x03A1"), ("Sigma", "\x03A3")
  , ("Tau", "\x03A4"), ("Upsilon", "\x03A5"), ("Phi", "\x03A6")
  , ("Chi", "\x03A7"), ("Psi", "\x03A8"), ("Omega", "\x03A9")
  , ("alpha", "\x03B1"), ("beta", "\x03B2"), ("gamma", "\x03B3")
  , ("delta", "\x03B4"), ("epsilon", "\x03B5"), ("zeta", "\x03B6")
  , ("eta", "\x03B7"), ("theta", "\x03B8"), ("iota", "\x03B9")
  , ("kappa", "\x03BA"), ("lambda", "\x03BB"), ("mu", "\x03BC")
  , ("nu", "\x03BD"), ("xi", "\x03BE"), ("omicron", "\x03BF")
  , ("pi", "\x03C0"), ("rho", "\x03C1"), ("sigmaf", "\x03C2")
  , ("sigma", "\x03C3"), ("tau", "\x03C4"), ("upsilon", "\x03C5")
  , ("phi", "\x03C6"), ("chi", "\x03C7"), ("psi", "\x03C8")
  , ("omega", "\x03C9")
  ]
