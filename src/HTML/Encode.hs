{-# LANGUAGE BangPatterns #-}
-- | HTML5 serializer.
--
-- Void elements serialize without closing tag. Boolean attributes
-- like @checked@ without value serialize as bare attribute names.
--
-- Escaping uses SIMD scanning from @cbits\/fast_xml.c@ to bulk-copy
-- clean byte ranges and only branch on the (rare) escapable bytes.
module HTML.Encode
  ( encodeHTML
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)

import HTML.Value

foreign import ccall unsafe "hs_html_find_text_escape"
  c_find_text_escape :: Ptr Word8 -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "hs_html_find_attr_escape"
  c_find_attr_escape :: Ptr Word8 -> CInt -> CInt -> IO CInt

encodeHTML :: HTMLDocument -> ByteString
encodeHTML doc = BL.toStrict (BB.toLazyByteString (buildDocument doc))

buildDocument :: HTMLDocument -> BB.Builder
buildDocument (HTMLDocument mdt root) =
  case mdt of
    Just (Doctype mName _ _) ->
      BB.byteString "<!DOCTYPE " <> BB.byteString (TE.encodeUtf8 (maybe "html" id mName))
        <> BB.char7 '>' <> BB.char7 '\n' <> buildNode root
    Nothing -> buildNode root

buildNode :: HTMLNode -> BB.Builder
buildNode = \case
  HTMLText t -> escapeText (TE.encodeUtf8 t)
  HTMLComment t ->
    BB.byteString "<!--" <> BB.byteString (TE.encodeUtf8 t) <> BB.byteString "-->"
  HTMLDoctype t _ _ ->
    BB.byteString "<!DOCTYPE " <> BB.byteString (TE.encodeUtf8 t) <> BB.char7 '>'
  HTMLElement tag attrs children
    | isVoidElement tag ->
        BB.char7 '<' <> BB.byteString (TE.encodeUtf8 tag) <> buildAttrs attrs <> BB.char7 '>'
    | V.null children ->
        BB.char7 '<' <> tagBS <> buildAttrs attrs <> BB.char7 '>'
        <> BB.byteString "</" <> tagBS <> BB.char7 '>'
    | otherwise ->
        BB.char7 '<' <> tagBS <> buildAttrs attrs <> BB.char7 '>'
        <> V.foldl' (\acc c -> acc <> buildNode c) mempty children
        <> BB.byteString "</" <> tagBS <> BB.char7 '>'
    where !tagBS = BB.byteString (TE.encodeUtf8 tag)

buildAttrs :: V.Vector HTMLAttribute -> BB.Builder
buildAttrs = V.foldl' (\acc a -> acc <> buildAttr a) mempty

buildAttr :: HTMLAttribute -> BB.Builder
buildAttr (HTMLAttribute name val)
  | T.null val && isBooleanAttr name =
      BB.char7 ' ' <> BB.byteString (TE.encodeUtf8 name)
  | otherwise =
      BB.char7 ' ' <> BB.byteString (TE.encodeUtf8 name)
        <> BB.byteString "=\"" <> escapeAttrValue (TE.encodeUtf8 val) <> BB.char7 '"'

{-# INLINE isBooleanAttr #-}
isBooleanAttr :: Text -> Bool
isBooleanAttr t = case t of
  "checked" -> True; "disabled" -> True; "readonly" -> True
  "selected" -> True; "autofocus" -> True; "autoplay" -> True
  "controls" -> True; "defer" -> True; "formnovalidate" -> True
  "hidden" -> True; "ismap" -> True; "loop" -> True
  "multiple" -> True; "muted" -> True; "nomodule" -> True
  "novalidate" -> True; "open" -> True; "required" -> True
  "reversed" -> True; "allowfullscreen" -> True; "async" -> True
  "default" -> True; "inert" -> True; "itemscope" -> True
  _ -> False

-- | Escape text content: replace <, >, & with entities.
-- Uses SIMD scanning to bulk-copy clean byte ranges.
escapeText :: ByteString -> BB.Builder
escapeText bs
  | BS.null bs = mempty
  | otherwise = unsafeDupablePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(cstr, len) -> do
        let !ptr = castPtr cstr :: Ptr Word8
        let go !off
              | off >= len = pure mempty
              | otherwise = do
                  CInt pos <- c_find_text_escape ptr (fromIntegral off) (fromIntegral len)
                  let !absPos = fromIntegral pos
                  if absPos >= len
                    then pure $! BB.byteString (bsDrop off bs)
                    else do
                      let !b = BSU.unsafeIndex bs absPos
                          !clean = bsSlice off (absPos - off) bs
                          !esc = textEscEntity b
                      rest <- go (absPos + 1)
                      pure $! BB.byteString clean <> esc <> rest
        go 0
{-# INLINE escapeText #-}

-- | Escape attribute values: replace ", &, <, > with entities.
escapeAttrValue :: ByteString -> BB.Builder
escapeAttrValue bs
  | BS.null bs = mempty
  | otherwise = unsafeDupablePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(cstr, len) -> do
        let !ptr = castPtr cstr :: Ptr Word8
        let go !off
              | off >= len = pure mempty
              | otherwise = do
                  CInt pos <- c_find_attr_escape ptr (fromIntegral off) (fromIntegral len)
                  let !absPos = fromIntegral pos
                  if absPos >= len
                    then pure $! BB.byteString (bsDrop off bs)
                    else do
                      let !b = BSU.unsafeIndex bs absPos
                          !clean = bsSlice off (absPos - off) bs
                          !esc = attrEscEntity b
                      rest <- go (absPos + 1)
                      pure $! BB.byteString clean <> esc <> rest
        go 0
{-# INLINE escapeAttrValue #-}

{-# INLINE textEscEntity #-}
textEscEntity :: Word8 -> BB.Builder
textEscEntity 0x3C = BB.byteString "&lt;"
textEscEntity 0x3E = BB.byteString "&gt;"
textEscEntity 0x26 = BB.byteString "&amp;"
textEscEntity b    = BB.word8 b

{-# INLINE attrEscEntity #-}
attrEscEntity :: Word8 -> BB.Builder
attrEscEntity 0x22 = BB.byteString "&quot;"
attrEscEntity 0x26 = BB.byteString "&amp;"
attrEscEntity 0x3C = BB.byteString "&lt;"
attrEscEntity 0x3E = BB.byteString "&gt;"
attrEscEntity b    = BB.word8 b

{-# INLINE bsSlice #-}
bsSlice :: Int -> Int -> ByteString -> ByteString
bsSlice off n bs = BS.take n (BS.drop off bs)

{-# INLINE bsDrop #-}
bsDrop :: Int -> ByteString -> ByteString
bsDrop = BS.drop
