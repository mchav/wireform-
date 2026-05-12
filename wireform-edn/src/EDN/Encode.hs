{-# LANGUAGE BangPatterns #-}

{- | EDN (Extensible Data Notation) text encoding.

Renders an 'EDN.Value.Value' to its canonical text representation.
Supports all EDN types including tagged literals, keywords with
namespaces, and special float values (@##NaN@, @##Inf@, @##-Inf@).
Strings are properly escaped per the EDN specification.

String escaping uses the SIMD JSON escape scanner to skip safe regions
in bulk, falling back to per-character escaping only for special chars.
-}
module EDN.Encode (
  encode,
  encodeBS,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromLazyText, fromString, fromText, singleton, toLazyText)
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as V
import Data.Word (Word8)
import EDN.Value qualified as E
import Wireform.Builder qualified as BB
import Wireform.FFI (findJsonEscapeBS)


-- | Render an EDN 'E.Value' to 'Text'.
encode :: E.Value -> Text
encode = toStrict . toLazyText . buildValue


-- | Render an EDN 'E.Value' to a UTF-8 'ByteString'.
encodeBS :: E.Value -> ByteString
encodeBS = TE.encodeUtf8 . encode


buildValue :: E.Value -> Builder
buildValue = \case
  E.Nil -> fromText "nil"
  E.Bool True -> fromText "true"
  E.Bool False -> fromText "false"
  E.Integer n -> fromString (show n)
  E.Float d
    | isNaN d -> fromText "##NaN"
    | isInfinite d && d > 0 -> fromText "##Inf"
    | isInfinite d -> fromText "##-Inf"
    | otherwise -> fromString (show d)
  E.String t -> singleton '"' <> escapeString t <> singleton '"'
  E.Char c -> buildChar c
  E.Keyword ns name -> singleton ':' <> buildQualified ns name
  E.Symbol ns name -> buildQualified ns name
  E.List vs -> buildCollection '(' ')' vs
  E.Vector vs -> buildCollection '[' ']' vs
  E.Map pairs ->
    singleton '{' <> buildPairs pairs <> singleton '}'
  E.Set vs ->
    fromText "#{" <> buildElems vs <> singleton '}'
  E.Tagged ns tag val
    | T.null ns -> singleton '#' <> fromText tag <> singleton ' ' <> buildValue val
    | otherwise ->
        singleton '#'
          <> fromText ns
          <> singleton '/'
          <> fromText tag
          <> singleton ' '
          <> buildValue val


buildQualified :: Maybe Text -> Text -> Builder
buildQualified Nothing name = fromText name
buildQualified (Just ns) name = fromText ns <> singleton '/' <> fromText name


buildCollection :: Char -> Char -> V.Vector E.Value -> Builder
buildCollection open close vs =
  singleton open <> buildElems vs <> singleton close


buildElems :: V.Vector E.Value -> Builder
buildElems vs
  | V.null vs = mempty
  | otherwise =
      V.ifoldl'
        ( \acc i v ->
            if i == 0
              then acc <> buildValue v
              else acc <> singleton ' ' <> buildValue v
        )
        mempty
        vs


buildPairs :: V.Vector (E.Value, E.Value) -> Builder
buildPairs ps
  | V.null ps = mempty
  | otherwise =
      V.ifoldl'
        ( \acc i (k, v) ->
            let pair = buildValue k <> singleton ' ' <> buildValue v
            in if i == 0
                then acc <> pair
                else acc <> fromText ", " <> pair
        )
        mempty
        ps


escapeString :: Text -> Builder
escapeString t =
  let !bs = TE.encodeUtf8 t
  in fromLazyText (TLE.decodeUtf8 (BB.toLazyByteString (escapeStringBS bs)))


escapeStringBS :: ByteString -> BB.Builder
escapeStringBS !bs = go 0
  where
    !len = BS.length bs
    go !pos
      | pos >= len = mempty
      | otherwise =
          let !escPos = findJsonEscapeBS bs pos
              !safeLen = escPos - pos
          in ( if safeLen > 0
                then BB.byteString (BSU.unsafeTake safeLen (BSU.unsafeDrop pos bs))
                else mempty
             )
              <> if escPos >= len
                then mempty
                else
                  let !b = BSU.unsafeIndex bs escPos
                  in escByte b <> go (escPos + 1)

    escByte :: Word8 -> BB.Builder
    escByte 0x22 = BB.byteString "\\\""
    escByte 0x5C = BB.byteString "\\\\"
    escByte 0x0A = BB.byteString "\\n"
    escByte 0x09 = BB.byteString "\\t"
    escByte 0x0D = BB.byteString "\\r"
    escByte b =
      BB.byteString "\\u00"
        <> BB.word8 (hexNibble (b `div` 16))
        <> BB.word8 (hexNibble (b `mod` 16))

    hexNibble :: Word8 -> Word8
    hexNibble n
      | n < 10 = 0x30 + n
      | otherwise = 0x61 + n - 10


buildChar :: Char -> Builder
buildChar '\n' = fromText "\\newline"
buildChar '\r' = fromText "\\return"
buildChar ' ' = fromText "\\space"
buildChar '\t' = fromText "\\tab"
buildChar c = singleton '\\' <> singleton c
