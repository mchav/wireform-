{-# LANGUAGE BangPatterns #-}
-- | SAX (Simple API for XML) event-based parser.
--
-- Uses SIMD C primitives from @cbits\/fast_xml.c@ for bulk character
-- scanning, giving significant speedups on text-heavy documents.
module XML.SAX
  ( SAXEvent(..)
  , parseSAX
  , parseSAXStream
  , foldSAX
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (chr, isDigit, isHexDigit, digitToInt, ord)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Foreign.C.Types (CInt(..), CUChar(..))
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)

import XML.Value (Name(..), Attribute(..), XMLDecl(..), simpleName, qualifiedName)

data SAXEvent
  = StartElement !Name !(Vector Attribute)
  | EndElement !Name
  | Characters !Text
  | CDATASection !Text
  | CommentEvent !Text
  | PI !Text !Text
  | StartDocument !(Maybe XMLDecl)
  | EndDocument
  deriving stock (Show, Eq)

-- FFI imports for SIMD scanning
foreign import ccall unsafe "hs_xml_find_byte"
  c_find_byte :: Ptr Word8 -> CInt -> CInt -> CUChar -> IO CInt

foreign import ccall unsafe "hs_xml_find_text_end"
  c_find_text_end :: Ptr Word8 -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "hs_xml_find_cdata_end"
  c_find_cdata_end :: Ptr Word8 -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "hs_xml_find_comment_end"
  c_find_comment_end :: Ptr Word8 -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "hs_xml_find_attr_end"
  c_find_attr_end :: Ptr Word8 -> CInt -> CInt -> CUChar -> IO CInt

findByte :: ByteString -> Int -> Word8 -> Int
findByte bs off target = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    fromIntegral <$> c_find_byte (castPtr ptr) (fromIntegral off) (fromIntegral len) (CUChar target)

findTextEnd :: ByteString -> Int -> Int
findTextEnd bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    fromIntegral <$> c_find_text_end (castPtr ptr) (fromIntegral off) (fromIntegral len)

findCDataEnd :: ByteString -> Int -> Int
findCDataEnd bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    fromIntegral <$> c_find_cdata_end (castPtr ptr) (fromIntegral off) (fromIntegral len)

findCommentEnd :: ByteString -> Int -> Int
findCommentEnd bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    fromIntegral <$> c_find_comment_end (castPtr ptr) (fromIntegral off) (fromIntegral len)

findAttrEnd :: ByteString -> Int -> Word8 -> Int
findAttrEnd bs off q = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    fromIntegral <$> c_find_attr_end (castPtr ptr) (fromIntegral off) (fromIntegral len) (CUChar q)

-- | Parse XML, emitting SAX events. Uses SIMD for character scanning.
parseSAX :: ByteString -> Either String (Vector SAXEvent)
parseSAX bs = unsafePerformIO $ do
  ref <- newIORef []
  result <- parseSAXImpl bs (\ev -> modifyIORef' ref (ev:))
  case result of
    Left err -> pure (Left err)
    Right () -> do
      evs <- readIORef ref
      pure (Right (V.fromList (reverse evs)))

-- | Streaming SAX: process events one at a time with a callback.
parseSAXStream :: ByteString -> (SAXEvent -> IO ()) -> IO (Either String ())
parseSAXStream = parseSAXImpl

-- | Fold over SAX events (pure).
foldSAX :: (a -> SAXEvent -> a) -> a -> ByteString -> Either String a
foldSAX f z bs = unsafePerformIO $ do
  ref <- newIORef z
  result <- parseSAXImpl bs (\ev -> modifyIORef' ref (\acc -> f acc ev))
  case result of
    Left err -> pure (Left err)
    Right () -> Right <$> readIORef ref

data PState = PState
  { psOffset :: !Int
  , psInput :: !ByteString
  , psLength :: !Int
  , psNsStack :: ![(Text, Text)]
  }

parseSAXImpl :: ByteString -> (SAXEvent -> IO ()) -> IO (Either String ())
parseSAXImpl bs emit = do
  let !len = BS.length bs
      initState = PState 0 bs len []
  -- Check for XML declaration first, then emit StartDocument
  result <- parseProlog initState emit
  case result of
    Left err -> pure (Left err)
    Right (st', _mDecl) -> do
      contentResult <- parseContent st' emit []
      case contentResult of
        Left err -> pure (Left err)
        Right (_st, []) -> do
          emit EndDocument
          pure (Right ())
        Right (_st, stack) ->
          pure (Left $ "Unclosed elements: " ++ show (map (T.unpack . nameLocal) stack))

type TagStack = [Name]

parseProlog :: PState -> (SAXEvent -> IO ()) -> IO (Either String (PState, Maybe XMLDecl))
parseProlog !st emit = do
  let !bs = psInput st
      !len = psLength st
      !off = skipSpaces bs 0 len
  if off + 5 < len &&
     BSU.unsafeIndex bs off == 0x3C &&
     BSU.unsafeIndex bs (off+1) == 0x3F &&
     BSU.unsafeIndex bs (off+2) == 0x78 &&
     BSU.unsafeIndex bs (off+3) == 0x6D &&
     BSU.unsafeIndex bs (off+4) == 0x6C &&
     isSpaceByte (BSU.unsafeIndex bs (off+5))
    then do
      let !nameEnd = off + 5
      parseXMLDeclAttrs bs nameEnd len $ \decl endOff -> do
        emit (StartDocument (Just decl))
        let !st' = st { psOffset = skipSpaces bs endOff len }
        -- Skip DOCTYPE if present
        st'' <- skipDoctypeIfPresent st' emit
        pure (Right (st'', Just decl))
    else do
      emit (StartDocument Nothing)
      let !st' = st { psOffset = off }
      st'' <- skipDoctypeIfPresent st' emit
      pure (Right (st'', Nothing))

skipDoctypeIfPresent :: PState -> (SAXEvent -> IO ()) -> IO PState
skipDoctypeIfPresent !st _emit = do
  let !bs = psInput st
      !len = psLength st
      !off = skipSpaces bs (psOffset st) len
  if off + 8 < len &&
     BSU.unsafeIndex bs off == 0x3C &&
     BSU.unsafeIndex bs (off+1) == 0x21 &&
     matchBytes bs (off + 2) [0x44, 0x4F, 0x43, 0x54, 0x59, 0x50, 0x45]
    then do
      let go !i !depth
            | i >= len = i
            | BSU.unsafeIndex bs i == 0x3E && depth == 0 = i + 1
            | BSU.unsafeIndex bs i == 0x5B = go (i + 1) (depth + 1 :: Int)
            | BSU.unsafeIndex bs i == 0x5D = go (i + 1) (depth - 1)
            | otherwise = go (i + 1) depth
          !newOff = go (off + 2) 0
      pure (st { psOffset = newOff })
    else pure (st { psOffset = off })

parseXMLDeclAttrs :: ByteString -> Int -> Int
                  -> (XMLDecl -> Int -> IO (Either String (PState, Maybe XMLDecl)))
                  -> IO (Either String (PState, Maybe XMLDecl))
parseXMLDeclAttrs !bs !off !len cont = go off Nothing Nothing Nothing
  where
    go !i !ver !enc !sa = do
      let !j = skipSpaces bs i len
      if j + 1 < len && BSU.unsafeIndex bs j == 0x3F && BSU.unsafeIndex bs (j+1) == 0x3E
        then do
          let decl = XMLDecl (maybe "1.0" id ver) enc sa
          cont decl (j + 2)
        else if j >= len
          then pure (Left "Unterminated XML declaration")
          else do
            let !attrNameEnd = skipNameChars bs j len
                !attrName = decodeSlice bs j (attrNameEnd - j)
                !eqPos = skipSpaces bs attrNameEnd len
            if eqPos >= len || BSU.unsafeIndex bs eqPos /= 0x3D
              then pure (Left $ "Expected '=' in XML declaration after " ++ T.unpack attrName)
              else do
                let !afterEq = skipSpaces bs (eqPos + 1) len
                if afterEq >= len
                  then pure (Left "Unterminated XML declaration attribute")
                  else do
                    let !q = BSU.unsafeIndex bs afterEq
                    if q /= 0x22 && q /= 0x27
                      then pure (Left "Expected quote in XML declaration")
                      else do
                        let !valStart = afterEq + 1
                            !valEnd = findAttrEnd bs valStart q
                        if valEnd < 0
                          then pure (Left "Unterminated attribute in XML declaration")
                          else do
                            let !val = decodeSlice bs valStart (valEnd - valStart)
                            case attrName of
                              "version"    -> go (valEnd + 1) (Just val) enc sa
                              "encoding"   -> go (valEnd + 1) ver (Just val) sa
                              "standalone" -> go (valEnd + 1) ver enc (Just (val == "yes"))
                              _ -> go (valEnd + 1) ver enc sa

parseContent :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseContent !st emit !stack
  | psOffset st >= psLength st = pure (Right (st, stack))
  | otherwise = do
      let !off = psOffset st
          !bs = psInput st
          !len = psLength st
      case BSU.unsafeIndex bs off of
        0x3C -> do -- '<'
          if off + 1 >= len
            then pure (Left "Unexpected end of input after '<'")
            else case BSU.unsafeIndex bs (off + 1) of
              0x2F -> parseEndTag st emit stack
              0x21 -> parseBangMarkup st emit stack
              0x3F -> parsePITag st emit stack
              _    -> parseStartTag st emit stack
        _ -> parseTextContent st emit stack

parseTextContent :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseTextContent !st emit !stack = do
  let !off = psOffset st
      !bs = psInput st
      !len = psLength st
  result <- collectText bs off len
  case result of
    Left err -> pure (Left err)
    Right (txt, newOff) -> do
      when (not (T.null txt)) $
        emit (Characters txt)
      parseContent (st { psOffset = newOff }) emit stack

collectText :: ByteString -> Int -> Int -> IO (Either String (Text, Int))
collectText !bs !off !len = go off []
  where
    go !i !acc
      | i >= len =
          pure (Right (T.concat (reverse acc), i))
      | BSU.unsafeIndex bs i == 0x3C =
          pure (Right (T.concat (reverse acc), i))
      | BSU.unsafeIndex bs i == 0x26 = do
          -- Entity reference: find ';'
          let !semicPos = findByte bs (i + 1) 0x3B
          if semicPos < 0
            then pure (Left "Unterminated entity reference")
            else do
              let !entityName = decodeSlice bs (i + 1) (semicPos - i - 1)
              case resolveEntity entityName of
                Nothing -> pure (Left $ "Unknown entity: &" ++ T.unpack entityName ++ ";")
                Just replacement ->
                  go (semicPos + 1) (replacement : acc)
      | otherwise = do
          let !endPos = findTextEnd bs i
              !chunk = decodeSlice bs i (endPos - i)
          go endPos (chunk : acc)

parseBangMarkup :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseBangMarkup !st emit !stack = do
  let !off = psOffset st
      !bs = psInput st
      !len = psLength st
  if off + 3 >= len
    then pure (Left "Unexpected end of input in markup")
    else if off + 3 < len && matchBytes bs (off + 2) [0x2D, 0x2D]
      then parseCommentTag st emit stack
      else if off + 9 <= len && matchBytes bs (off + 2) [0x5B, 0x43, 0x44, 0x41, 0x54, 0x41, 0x5B]
        then parseCDataTag st emit stack
        else if off + 8 <= len && matchBytes bs (off + 2) [0x44, 0x4F, 0x43, 0x54, 0x59, 0x50, 0x45]
          then skipDoctypeTag st emit stack
          else pure (Left $ "Unknown markup at offset " ++ show off)

parseCommentTag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseCommentTag !st emit !stack = do
  let !off = psOffset st
      !bs = psInput st
      !startContent = off + 4
      !endPos = findCommentEnd bs startContent
  if endPos < 0
    then pure (Left "Unterminated comment")
    else do
      let !txt = decodeSlice bs startContent (endPos - startContent)
      emit (CommentEvent txt)
      parseContent (st { psOffset = endPos + 3 }) emit stack

parseCDataTag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseCDataTag !st emit !stack = do
  let !off = psOffset st
      !bs = psInput st
      !startContent = off + 9
      !endPos = findCDataEnd bs startContent
  if endPos < 0
    then pure (Left "Unterminated CDATA section")
    else do
      let !txt = decodeSlice bs startContent (endPos - startContent)
      emit (CDATASection txt)
      parseContent (st { psOffset = endPos + 3 }) emit stack

skipDoctypeTag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
skipDoctypeTag !st emit !stack = do
  let !off = psOffset st
      !bs = psInput st
      !len = psLength st
      go !i !depth
        | i >= len = Left "Unterminated DOCTYPE"
        | BSU.unsafeIndex bs i == 0x3E && depth == 0 = Right (i + 1)
        | BSU.unsafeIndex bs i == 0x5B = go (i + 1) (depth + 1 :: Int)
        | BSU.unsafeIndex bs i == 0x5D = go (i + 1) (depth - 1)
        | otherwise = go (i + 1) depth
  case go (off + 2) 0 of
    Left err -> pure (Left err)
    Right newOff -> parseContent (st { psOffset = newOff }) emit stack

parsePITag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parsePITag !st emit !stack = do
  let !off = psOffset st
      !bs = psInput st
      !len = psLength st
      !nameStart = off + 2
  if nameStart >= len
    then pure (Left "Unexpected end in processing instruction")
    else do
      let !nameEnd = skipNameChars bs nameStart len
          !piTarget = decodeSlice bs nameStart (nameEnd - nameStart)
          !qEnd = findPIEnd bs nameEnd len
      case qEnd of
        Nothing -> pure (Left "Unterminated processing instruction")
        Just end -> do
          let !contentStart = skipSpaces bs nameEnd len
              !content = if contentStart < end
                           then decodeSlice bs contentStart (end - contentStart)
                           else T.empty
          emit (PI piTarget content)
          parseContent (st { psOffset = end + 2 }) emit stack

findPIEnd :: ByteString -> Int -> Int -> Maybe Int
findPIEnd bs off len = go off
  where
    go !i
      | i + 1 >= len = Nothing
      | BSU.unsafeIndex bs i == 0x3F && BSU.unsafeIndex bs (i+1) == 0x3E = Just i
      | otherwise = go (i + 1)

parseStartTag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseStartTag !st emit !stack = do
  let !off = psOffset st
      !bs = psInput st
      !len = psLength st
      !nameStart = off + 1
      !nameEnd = skipNameChars bs nameStart len
      !rawName = decodeSlice bs nameStart (nameEnd - nameStart)
  parseAttributes bs nameEnd len [] $ \attrs endOff selfClose -> do
    let !nsStack' = foldl addNs (psNsStack st) (V.toList attrs)
        !st' = st { psOffset = endOff, psNsStack = nsStack' }
        !resolvedName = resolveNameNs rawName nsStack'
        !resolvedAttrs = V.map (resolveAttrNs nsStack') attrs
    emit (StartElement resolvedName resolvedAttrs)
    if selfClose
      then do
        emit (EndElement resolvedName)
        parseContent st' emit stack
      else
        parseContent st' emit (resolvedName : stack)

parseEndTag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseEndTag !st emit !stack = do
  let !off = psOffset st
      !bs = psInput st
      !nameStart = off + 2
      !nameEnd = skipNameChars bs nameStart (psLength st)
      !rawName = decodeSlice bs nameStart (nameEnd - nameStart)
      !gtPos = findByte bs nameEnd 0x3E
  if gtPos < 0
    then pure (Left "Unterminated end tag")
    else do
      let !name = resolveNameNs rawName (psNsStack st)
      case stack of
        [] -> pure (Left $ "Unexpected end tag: </" ++ T.unpack rawName ++ ">")
        (top : rest) ->
          if nameLocal top == nameLocal name &&
             namePrefix top == namePrefix name
            then do
              emit (EndElement name)
              parseContent (st { psOffset = gtPos + 1 }) emit rest
            else pure (Left $ "Mismatched tags: expected </" ++
                       T.unpack (nameLocal top) ++ ">, got </" ++
                       T.unpack rawName ++ ">")

parseAttributes :: ByteString -> Int -> Int -> [Attribute]
               -> (Vector Attribute -> Int -> Bool -> IO (Either String (PState, TagStack)))
               -> IO (Either String (PState, TagStack))
parseAttributes !bs !off !len !acc cont = do
  let !i = skipSpaces bs off len
  if i >= len
    then pure (Left "Unterminated start tag")
    else case BSU.unsafeIndex bs i of
      0x3E -> cont (V.fromList (reverse acc)) (i + 1) False
      0x2F | i + 1 < len && BSU.unsafeIndex bs (i+1) == 0x3E ->
        cont (V.fromList (reverse acc)) (i + 2) True
      _ -> do
        let !nameEnd = skipNameChars bs i len
            !rawAttrName = decodeSlice bs i (nameEnd - i)
            !eqPos = skipSpaces bs nameEnd len
        if eqPos >= len || BSU.unsafeIndex bs eqPos /= 0x3D
          then pure (Left $ "Expected '=' after attribute name '" ++ T.unpack rawAttrName ++ "'")
          else do
            let !afterEq = skipSpaces bs (eqPos + 1) len
            if afterEq >= len
              then pure (Left "Unterminated attribute value")
              else do
                let !q = BSU.unsafeIndex bs afterEq
                if q /= 0x22 && q /= 0x27
                  then pure (Left $ "Expected quote for attribute '" ++ T.unpack rawAttrName ++ "'")
                  else do
                    let !valStart = afterEq + 1
                        !valEnd = findAttrEnd bs valStart q
                    if valEnd < 0
                      then pure (Left "Unterminated attribute value")
                      else do
                        let !rawVal = sliceBS bs valStart (valEnd - valStart)
                        valResult <- resolveEntitiesBS rawVal
                        case valResult of
                          Left err -> pure (Left err)
                          Right val -> do
                            let !attrName = parseAttrName rawAttrName
                                !attr = Attribute attrName val
                            parseAttributes bs (valEnd + 1) len (attr : acc) cont

-- Name parsing

parseAttrName :: Text -> Name
parseAttrName raw =
  case T.breakOn ":" raw of
    (pfx, rest)
      | T.null rest -> simpleName pfx
      | otherwise -> qualifiedName pfx (T.drop 1 rest)

resolveNameNs :: Text -> [(Text, Text)] -> Name
resolveNameNs raw nsStack =
  case T.breakOn ":" raw of
    (local, rest)
      | T.null rest -> Name local Nothing (lookup "" nsStack)
      | otherwise ->
          let !pfx = local
              !localPart = T.drop 1 rest
          in Name localPart (Just pfx) (lookup pfx nsStack)

resolveAttrNs :: [(Text, Text)] -> Attribute -> Attribute
resolveAttrNs nsStack (Attribute name val) =
  let !resolved = case namePrefix name of
        Nothing -> name
        Just pfx -> name { nameNamespace = lookup pfx nsStack }
  in Attribute resolved val

addNs :: [(Text, Text)] -> Attribute -> [(Text, Text)]
addNs stack (Attribute name val)
  | nameLocal name == "xmlns" && namePrefix name == Nothing =
      ("", val) : stack
  | namePrefix name == Just "xmlns" =
      (nameLocal name, val) : stack
  | otherwise = stack

-- Low-level utilities

skipSpaces :: ByteString -> Int -> Int -> Int
skipSpaces !bs !off !len
  | off >= len = off
  | otherwise =
      let !b = BSU.unsafeIndex bs off
      in if isSpaceByte b
           then skipSpaces bs (off + 1) len
           else off

isSpaceByte :: Word8 -> Bool
isSpaceByte b = b == 0x20 || b == 0x09 || b == 0x0A || b == 0x0D
{-# INLINE isSpaceByte #-}

skipNameChars :: ByteString -> Int -> Int -> Int
skipNameChars !bs !off !len = go off
  where
    go !i
      | i >= len = i
      | isNameByte (BSU.unsafeIndex bs i) = go (i + 1)
      | otherwise = i

isNameByte :: Word8 -> Bool
isNameByte !b =
  (b >= 0x61 && b <= 0x7A) ||
  (b >= 0x41 && b <= 0x5A) ||
  (b >= 0x30 && b <= 0x39) ||
  b == 0x3A || b == 0x5F || b == 0x2D || b == 0x2E ||
  b >= 0x80
{-# INLINE isNameByte #-}

matchBytes :: ByteString -> Int -> [Word8] -> Bool
matchBytes bs off expected = go off expected
  where
    !len = BS.length bs
    go !_ [] = True
    go !i (b:rest)
      | i >= len = False
      | BSU.unsafeIndex bs i == b = go (i + 1) rest
      | otherwise = False

sliceBS :: ByteString -> Int -> Int -> ByteString
sliceBS bs off count
  | count <= 0 = BS.empty
  | otherwise = BS.take count (BS.drop off bs)

decodeSlice :: ByteString -> Int -> Int -> Text
decodeSlice bs off count = TE.decodeUtf8 (sliceBS bs off count)

resolveEntitiesBS :: ByteString -> IO (Either String Text)
resolveEntitiesBS bs = pure $ resolveEntities (TE.decodeUtf8 bs)

resolveEntities :: Text -> Either String Text
resolveEntities txt
  | T.null txt = Right T.empty
  | not (T.any (== '&') txt) = Right txt
  | otherwise = resolveLoop txt
  where
    resolveLoop t =
      case T.breakOn "&" t of
        (before, rest)
          | T.null rest -> Right before
          | otherwise ->
              let !afterAmp = T.drop 1 rest
              in case T.breakOn ";" afterAmp of
                (_, semicRest)
                  | T.null semicRest ->
                      Left $ "Unterminated entity reference: &" ++ T.unpack (T.take 10 afterAmp)
                  | otherwise ->
                      let !entityName = T.takeWhile (/= ';') afterAmp
                          !remaining = T.drop 1 (T.dropWhile (/= ';') afterAmp)
                      in case resolveEntity entityName of
                        Nothing -> Left $ "Unknown entity: &" ++ T.unpack entityName ++ ";"
                        Just replacement -> do
                          rest' <- resolveLoop remaining
                          Right (before <> replacement <> rest')

resolveEntity :: Text -> Maybe Text
resolveEntity "amp"  = Just "&"
resolveEntity "lt"   = Just "<"
resolveEntity "gt"   = Just ">"
resolveEntity "apos" = Just "'"
resolveEntity "quot" = Just "\""
resolveEntity t
  | T.isPrefixOf "#x" t || T.isPrefixOf "#X" t =
      let hex = T.drop 2 t
      in if T.null hex || not (T.all isHexDigit hex)
           then Nothing
           else let !n = T.foldl' (\acc c -> acc * 16 + digitToInt c) 0 hex
                in if n > 0x10FFFF then Nothing else Just (T.singleton (chr n))
  | T.isPrefixOf "#" t =
      let dec = T.drop 1 t
      in if T.null dec || not (T.all isDigit dec)
           then Nothing
           else let !n = T.foldl' (\acc c -> acc * 10 + (ord c - ord '0')) 0 dec
                in if n > 0x10FFFF then Nothing else Just (T.singleton (chr n))
  | otherwise = Nothing
