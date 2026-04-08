{-# LANGUAGE BangPatterns #-}
-- | HTML5 serializer.
--
-- Void elements serialize without closing tag. Boolean attributes
-- like @checked@ without value serialize as bare attribute names.
module HTML.Encode
  ( encodeHTML
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, singleton)
import qualified Data.Vector as V

import HTML.Value

encodeHTML :: HTMLDocument -> ByteString
encodeHTML doc = TE.encodeUtf8 (TL.toStrict (toLazyText (buildDocument doc)))

buildDocument :: HTMLDocument -> Builder
buildDocument (HTMLDocument mdt root) =
  let !dtPart = case mdt of
        Just (Doctype mName _ _) ->
          fromText "<!DOCTYPE " <> fromText (maybe "html" id mName) <> singleton '>' <> singleton '\n'
        Nothing -> mempty
  in dtPart <> buildNode root

buildNode :: HTMLNode -> Builder
buildNode = \case
  HTMLText t -> escapeText t
  HTMLComment t -> fromText "<!--" <> fromText t <> fromText "-->"
  HTMLDoctype t -> fromText "<!DOCTYPE " <> fromText t <> singleton '>'
  HTMLElement tag attrs children
    | isVoidElement tag ->
        singleton '<' <> fromText tag <> buildAttrs attrs <> singleton '>'
    | V.null children ->
        singleton '<' <> fromText tag <> buildAttrs attrs <> singleton '>'
        <> fromText "</" <> fromText tag <> singleton '>'
    | otherwise ->
        singleton '<' <> fromText tag <> buildAttrs attrs <> singleton '>'
        <> V.foldl' (\acc c -> acc <> buildNode c) mempty children
        <> fromText "</" <> fromText tag <> singleton '>'

buildAttrs :: V.Vector HTMLAttribute -> Builder
buildAttrs = V.foldl' (\acc a -> acc <> buildAttr a) mempty

buildAttr :: HTMLAttribute -> Builder
buildAttr (HTMLAttribute name val)
  | T.null val && isBooleanAttr name =
      singleton ' ' <> fromText name
  | otherwise =
      singleton ' ' <> fromText name <> fromText "=\"" <> escapeAttrValue val <> singleton '"'

isBooleanAttr :: Text -> Bool
isBooleanAttr t = t `elem`
  [ "checked", "disabled", "readonly", "selected", "autofocus"
  , "autoplay", "controls", "defer", "formnovalidate", "hidden"
  , "ismap", "loop", "multiple", "muted", "nomodule", "novalidate"
  , "open", "required", "reversed", "allowfullscreen", "async"
  , "default", "inert", "itemscope"
  ]

escapeText :: Text -> Builder
escapeText = foldMap escChar . T.unpack
  where
    escChar '<' = fromText "&lt;"
    escChar '>' = fromText "&gt;"
    escChar '&' = fromText "&amp;"
    escChar c   = singleton c

escapeAttrValue :: Text -> Builder
escapeAttrValue = foldMap escChar . T.unpack
  where
    escChar '"' = fromText "&quot;"
    escChar '&' = fromText "&amp;"
    escChar '<' = fromText "&lt;"
    escChar '>' = fromText "&gt;"
    escChar c   = singleton c
