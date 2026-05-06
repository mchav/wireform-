{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE UnboxedTuples #-}
-- | YAML 1.2 decoder.
--
-- Parses a YAML stream into a 'YAML.Value.Stream' / 'Document' /
-- 'Value' according to the YAML 1.2 specification, applying the
-- /core schema/ for plain-scalar resolution. Supports:
--
-- * Block-style mappings and sequences (with arbitrary indentation).
-- * Flow-style mappings (@{a: b}@) and sequences (@[a, b]@), nested.
-- * Plain, single-quoted, and double-quoted scalars (with all
--   YAML 1.2 escapes for the latter, including @\\xNN@, @\\uNNNN@,
--   @\\UNNNNNNNN@).
-- * Block literal (@|@) and block folded (@>@) scalars with all four
--   chomping indicators (@-@, @+@, default-clip) and optional
--   indentation indicators.
-- * Anchors (@&name@) and aliases (@*name@). Aliases are expanded
--   inline so that downstream code can treat the result as a tree.
-- * Explicit tags (@!!str@, @!!int@, @!\<tag:yaml.org,2002:bool\>@,
--   etc.) with the standard short-hand expansions for the
--   @tag:yaml.org,2002:@ family.
-- * Comments, blank lines, the @---@ document-start and @...@
--   document-end markers, and multi-document streams.
module YAML.Decode
  ( decode
  , decodeBS
  , decodeStream
  , decodeStreamBS
  , decodeDocuments
  , preprocess
  ) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TI
import Data.Char (chr, digitToInt, isDigit, isHexDigit)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Data.Word (Word8, Word64)
import qualified Data.Bits as Bits
import GHC.Exts
  ( ByteArray#, Int (..)
  , indexWord8ArrayAsWord64#
  , word64ToWord#, ctz#, uncheckedIShiftRL#
  , word2Int#
  )
import GHC.Word (Word64 (..))

import YAML.Value

-- ---------------------------------------------------------------------------
-- Fast byte-indexed Text access. The flow parser walks a buffer
-- one byte at a time looking for ASCII control characters
-- ('[', ']', '{', '}', ',', ':', '#', '\\1', '\\2'); doing that
-- through 'T.index' / 'T.length' is O(p) / O(N) per call because
-- Text positions are character indices and the implementation has
-- to walk UTF-8 bytes to convert. Operating on the underlying
-- byte array directly gives O(1) access without changing the
-- semantics for ASCII-only control bytes (multi-byte UTF-8
-- sequences in scalar content all start with a leading byte ≥
-- 0x80 that never matches our ASCII stoppers).
-- ---------------------------------------------------------------------------

-- | Byte length of a 'Text' (O(1)).
bLen :: Text -> Int
bLen (TI.Text _ _ l) = l
{-# INLINE bLen #-}

-- | Byte at position @p@ inside a 'Text' (O(1)). The caller is
-- responsible for keeping @p@ in @[0, bLen t)@.
bAt :: Text -> Int -> Word8
bAt (TI.Text arr off _) p = TA.unsafeIndex arr (off + p)
{-# INLINE bAt #-}

-- | Slice @t@ to the bytes @[s, e)@ (O(1)). The caller must
-- ensure both ends sit on UTF-8 character boundaries.
bSlice :: Text -> Int -> Int -> Text
bSlice (TI.Text arr off _) s e = TI.text arr (off + s) (max 0 (e - s))
{-# INLINE bSlice #-}

-- | Drop the first @p@ bytes of @t@ (O(1)). Caller must ensure
-- @p@ lies on a UTF-8 character boundary.
bDrop :: Int -> Text -> Text
bDrop p (TI.Text arr off l) =
  let !p' = min p l
  in TI.text arr (off + p') (l - p')
{-# INLINE bDrop #-}

-- | Take the first @p@ bytes of @t@ (O(1)). Caller must ensure
-- @p@ lies on a UTF-8 character boundary.
bTake :: Int -> Text -> Text
bTake p (TI.Text arr off l) =
  let !p' = max 0 (min p l)
  in TI.text arr off p'
{-# INLINE bTake #-}

-- ---------------------------------------------------------------------------
-- SWAR (SIMD-Within-A-Register) byte scanners.
--
-- Most of the parser's hot inner loops are looking for a single
-- ASCII byte (e.g. '#', '\\1', '\"') in a 'Text' body. The
-- Data.Text 'T.any' / 'T.findIndex' walk one /character/ at a
-- time via the UTF-8 stream interface; we can do much better by
-- treating the underlying byte array as 64-bit words and using
-- the classic "has-zero-byte" bit trick:
--
--   hasZeroByte x = ((x - 0x0101010101010101) & ~x & 0x8080808080808080) /= 0
--
-- After XOR-ing the input word with a broadcast of the target
-- byte, any byte equal to the target becomes zero, so a single
-- 64-bit operation tells us whether the eight bytes contain it.
-- ---------------------------------------------------------------------------

bcast64 :: Word8 -> Word64
bcast64 b = fromIntegral b * 0x0101010101010101
{-# INLINE bcast64 #-}

hasZeroByte :: Word64 -> Bool
hasZeroByte x =
  ((x - 0x0101010101010101) Bits..&. Bits.complement x
                            Bits..&. 0x8080808080808080) /= 0
{-# INLINE hasZeroByte #-}

-- | Read 8 unaligned bytes from a byte array starting at byte
-- index @i@ as a single 'Word64'.
indexWord64Unaligned :: ByteArray# -> Int -> Word64
indexWord64Unaligned ba# (I# i#) = W64# (indexWord8ArrayAsWord64# ba# i#)
{-# INLINE indexWord64Unaligned #-}

-- | True when @t@ contains the byte @c@. Pure byte-level scan
-- (no UTF-8 decoding) using a Word64-at-a-time loop.
bAnyByte :: Word8 -> Text -> Bool
bAnyByte !c (TI.Text (TA.ByteArray ba#) off len) =
  let !pat   = bcast64 c
      !endB  = off + len
      goWord !i
        | i + 8 > endB = goByte i
        | otherwise    =
            let !w     = indexWord64Unaligned ba# i
                !xored = w `Bits.xor` pat
            in if hasZeroByte xored
                 then True
                 else goWord (i + 8)
      goByte !i
        | i >= endB                      = False
        | TA.unsafeIndex (TA.ByteArray ba#) i == c = True
        | otherwise                      = goByte (i + 1)
  in goWord off
{-# INLINE bAnyByte #-}

-- | First index (within the slice — not the underlying array)
-- of byte @c@ in @t@, or @-1@ if absent. Same SWAR scan as
-- 'bAnyByte', plus 'ctz' to find the matching byte's offset
-- inside a hit word.
bFindByte :: Word8 -> Text -> Int
bFindByte !c (TI.Text (TA.ByteArray ba#) off len) =
  let !pat   = bcast64 c
      !endB  = off + len
      goWord !i
        | i + 8 > endB = goByte i
        | otherwise    =
            let !w     = indexWord64Unaligned ba# i
                !xored = w `Bits.xor` pat
            in if hasZeroByte xored
                 then
                   -- Locate the matching byte inside the word.
                   -- 'ctz' on the masked word returns the bit
                   -- index; divide by 8 for the byte index.
                   let !mask = (xored - 0x0101010101010101)
                                Bits..&. Bits.complement xored
                                Bits..&. 0x8080808080808080
                       !byteOff = wordCtzInBytes mask
                   in i + byteOff - off
                 else goWord (i + 8)
      goByte !i
        | i >= endB                      = -1
        | TA.unsafeIndex (TA.ByteArray ba#) i == c = i - off
        | otherwise                      = goByte (i + 1)
  in goWord off
{-# INLINE bFindByte #-}

-- | Index of the lowest-order byte set in a 'Word64' mask. The
-- mask must be non-zero (caller checks via 'hasZeroByte'). Uses
-- 'ctz#' / 8 to convert bit position to byte position.
wordCtzInBytes :: Word64 -> Int
wordCtzInBytes (W64# w#) =
  -- 'ctz#' takes a 'Word#'; on a 64-bit host the cast from
  -- 'Word64#' to 'Word#' via 'word64ToWord#' is identity-cost.
  I# (uncheckedIShiftRL# (word2Int# (ctz# (word64ToWord# w#))) 3#)
{-# INLINE wordCtzInBytes #-}

-- | First index of /any/ of three bytes in @t@, or @-1@ when
-- none is present. Three SWAR scans run on the same Word64 per
-- iteration; combined hi-bit mask is then 'ctz'-ed for the
-- in-word offset.
bFindAnyOf3 :: Word8 -> Word8 -> Word8 -> Text -> Int
bFindAnyOf3 !a !b !c (TI.Text (TA.ByteArray ba#) off len) =
  let !pa    = bcast64 a
      !pb    = bcast64 b
      !pc    = bcast64 c
      !endB  = off + len
      !ones  = 0x0101010101010101
      !his   = 0x8080808080808080

      mask !w !p =
        let !x = w `Bits.xor` p
        in (x - ones) Bits..&. Bits.complement x Bits..&. his

      goWord !i
        | i + 8 > endB = goByte i
        | otherwise    =
            let !w  = indexWord64Unaligned ba# i
                !m  = mask w pa Bits..|. mask w pb Bits..|. mask w pc
            in if m == 0
                 then goWord (i + 8)
                 else
                   let !bo = wordCtzInBytes m
                   in i + bo - off

      goByte !i
        | i >= endB = -1
        | otherwise =
            let !x = TA.unsafeIndex (TA.ByteArray ba#) i
            in if x == a || x == b || x == c
                 then i - off
                 else goByte (i + 1)
  in goWord off
{-# INLINE bFindAnyOf3 #-}

-- | First index of either of two bytes. See 'bFindAnyOf3'.
bFindAnyOf2 :: Word8 -> Word8 -> Text -> Int
bFindAnyOf2 !a !b (TI.Text (TA.ByteArray ba#) off len) =
  let !pa    = bcast64 a
      !pb    = bcast64 b
      !endB  = off + len
      !ones  = 0x0101010101010101
      !his   = 0x8080808080808080

      mask !w !p =
        let !x = w `Bits.xor` p
        in (x - ones) Bits..&. Bits.complement x Bits..&. his

      goWord !i
        | i + 8 > endB = goByte i
        | otherwise    =
            let !w  = indexWord64Unaligned ba# i
                !m  = mask w pa Bits..|. mask w pb
            in if m == 0
                 then goWord (i + 8)
                 else
                   let !bo = wordCtzInBytes m
                   in i + bo - off

      goByte !i
        | i >= endB = -1
        | otherwise =
            let !x = TA.unsafeIndex (TA.ByteArray ba#) i
            in if x == a || x == b
                 then i - off
                 else goByte (i + 1)
  in goWord off
{-# INLINE bFindAnyOf2 #-}

-- | Word8 character literals of common ASCII control bytes.
w8Comma, w8Colon, w8LBrack, w8RBrack, w8LBrace, w8RBrace
  , w8DQuote, w8SQuote, w8Hash, w8Space, w8Tab, w8Quest
  , w8SOH, w8STX, w8Bang, w8Amp, w8Star, w8Backslash :: Word8
w8Comma     = 44
w8Colon     = 58
w8LBrack    = 91
w8RBrack    = 93
w8LBrace    = 123
w8RBrace    = 125
w8DQuote    = 34
w8SQuote    = 39
w8Hash      = 35
w8Space     = 32
w8Tab       = 9
w8Quest     = 63
w8SOH       = 1   -- '\\1'
w8STX       = 2   -- '\\2'
w8Bang      = 33
w8Amp       = 38
w8Star      = 42
w8Backslash = 92

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

decode :: Text -> Either String Value
decode t = case decodeDocuments t of
  Left err     -> Left err
  Right []     -> Right YNull
  Right (d:_)  -> Right (docBody d)

decodeBS :: ByteString -> Either String Value
decodeBS = decode . TE.decodeUtf8Lenient

decodeStream :: Text -> Either String Stream
decodeStream t = (Stream . V.fromList) <$> decodeDocuments t

decodeStreamBS :: ByteString -> Either String Stream
decodeStreamBS = decodeStream . TE.decodeUtf8Lenient

decodeDocuments :: Text -> Either String [Document]
decodeDocuments src = parseStream (preprocess src)

-- ---------------------------------------------------------------------------
-- Pre-processing: split into structured lines
-- ---------------------------------------------------------------------------

data PLine = PLine
  { lineNo     :: !Int
  , lineIndent :: !Int
  , lineKind   :: !LineKind
  , lineBody   :: !Text       -- ^ content after stripping indent
                              --   AND trailing whitespace (the form
                              --   most parser paths want)
  , lineRawBody :: !Text      -- ^ content after stripping indent
                              --   only — trailing whitespace kept,
                              --   for block-scalar collection
  } deriving (Show)

data LineKind
  = LBlank
  | LComment
  | LDocStart        -- ^ @---@
  | LDocEnd          -- ^ @...@
  | LDirective       -- ^ @%YAML 1.2@ etc.
  | LContent
  deriving (Eq, Show)

preprocess :: Text -> [PLine]
preprocess input@(TI.Text arr off blen) =
  goByte 1 off (-1) (-1)
  where
    !endByte = off + blen

    -- Walk the underlying byte array a line at a time, slicing
    -- each line into a 'PLine' without allocating intermediate
    -- '[Text]' chunks. The 'lineStart' field holds the byte
    -- offset where the current line begins; 'contentStart' is
    -- the byte offset of the first non-space character (or -1
    -- if not yet found).
    goByte :: Int -> Int -> Int -> Int -> [PLine]
    goByte !lno !lineStart !contentStart !lastNonWS
      | lineStart >= endByte = []
      | otherwise = scanFor lno lineStart contentStart lastNonWS lineStart

    -- Scan from position 'i' to find the line end (newline or
    -- end of input). Track 'contentStart' (first non-' ') and
    -- 'lastNonWS' (last non-WS char, used by stripEnd-style
    -- body slicing).
    scanFor :: Int -> Int -> Int -> Int -> Int -> [PLine]
    scanFor !lno !lineStart !contentStart !lastNonWS !i
      | i >= endByte =
          if i == lineStart
            then []
            -- Always emit the trailing line (with no terminating
            -- '\n'); 'mkLine' creates an LBlank with the correct
            -- leading-space count when 'contentStart < 0'.
            else mkLine lno lineStart endByte contentStart lastNonWS : []
      | otherwise = case TA.unsafeIndex arr i of
          10 -> -- '\n'
              let line = mkLine lno lineStart i contentStart lastNonWS
              in line : goByte (lno + 1) (i + 1) (-1) (-1)
          13 -> -- '\r' — treat as part of line, but the next
                -- '\n' (if any) terminates and we trim the '\r'.
                scanFor lno lineStart contentStart lastNonWS (i + 1)
          32 -> -- ' '
                scanFor lno lineStart contentStart lastNonWS (i + 1)
          9  -> -- '\t' — per YAML 1.2 §6.1, tabs are NEVER
                -- part of indentation. Treat the tab as the
                -- start of content if we haven't seen content
                -- yet (so the tab survives in 'lineRawBody'
                -- after indent stripping); otherwise leave
                -- contentStart alone. We do NOT update
                -- 'lastNonWS' because trailing tabs still get
                -- stripped from 'lineBody'.
                scanFor lno lineStart
                  (if contentStart < 0 then i else contentStart)
                  lastNonWS (i + 1)
          _  -> -- non-WS byte (or byte >= 0x80 in multi-byte
                -- UTF-8); treat as content.
                scanFor lno lineStart
                  (if contentStart < 0 then i else contentStart)
                  i (i + 1)

    -- Build a PLine for the byte range [lineStart, lineEnd)
    -- with first content byte at 'contentStart' and last non-
    -- whitespace byte at 'lastNonWS'. When 'contentStart < 0'
    -- the line has no non-whitespace content; we still record
    -- the count of leading SPACE characters as the indent (so
    -- block-scalar logic can decide whether a blank line is
    -- "more-indented" than the base) and stash any remaining
    -- whitespace (tabs, trailing spaces) into 'lineRawBody' so
    -- downstream code can detect tab-as-indent violations.
    mkLine :: Int -> Int -> Int -> Int -> Int -> PLine
    mkLine !lno !lineStart !lineEnd !contentStart !lastNonWS
      | contentStart < 0 =
          let !leadSp    = countLeadingSpaces lineStart lineEnd
              !endNoCR   = if lineEnd > lineStart
                              && TA.unsafeIndex arr (lineEnd - 1) == 13
                             then lineEnd - 1
                             else lineEnd
              !restStart = lineStart + leadSp
              !restLen   = max 0 (endNoCR - restStart)
              !lineRawB  = if restLen == 0
                              then T.empty
                              else TI.text arr restStart restLen
          in PLine lno leadSp LBlank T.empty lineRawB
      | otherwise =
          let !ind       = contentStart - lineStart
              !endNoCR   = if lineEnd > lineStart
                              && TA.unsafeIndex arr (lineEnd - 1) == 13
                             then lineEnd - 1
                             else lineEnd
              !rawLen    = endNoCR - contentStart
              -- 'lastNonWS' may sit before 'contentStart' when
              -- the line begins with a tab (we treat tabs as
              -- content-start markers but not as last-non-WS
              -- markers). Clamp the body length so we never
              -- splice a negative-length 'Text'.
              !bodyLen   = max 0 (lastNonWS + 1 - contentStart)
              !lineRawB  = TI.text arr contentStart rawLen
              !lineB     | bodyLen == 0       = T.empty
                         | bodyLen == rawLen  = lineRawB
                         | otherwise          = TI.text arr contentStart bodyLen
              !kind      = classify lineB
          in PLine lno ind kind lineB lineRawB

    countLeadingSpaces :: Int -> Int -> Int
    countLeadingSpaces !s !e = go s
      where
        go !i
          | i >= e                      = i - s
          | TA.unsafeIndex arr i == 32  = go (i + 1)
          | otherwise                   = i - s

stripCR :: Text -> Text
stripCR t = case T.unsnoc t of
  Just (rest, '\r') -> rest
  _                 -> t
{-# INLINE stripCR #-}

leadingSpaces :: Text -> Int
leadingSpaces = T.length . T.takeWhile (== ' ')
{-# INLINE leadingSpaces #-}

-- Top-level CAFs so 'classify' doesn't repack literals on every line.
dashesText, dashesSpace, dashesTab :: Text
dashesText  = T.pack "---"
dashesSpace = T.pack "--- "
dashesTab   = T.pack "---\t"

dotsText, dotsSpace, dotsTab :: Text
dotsText  = T.pack "..."
dotsSpace = T.pack "... "
dotsTab   = T.pack "...\t"

classify :: Text -> LineKind
classify t = case T.uncons t of
  Nothing      -> LBlank
  Just (h, _)
    | h == '#' -> LComment
    | h == '-'
    , t == dashesText
      || T.isPrefixOf dashesSpace t
      || T.isPrefixOf dashesTab   t -> LDocStart
    | h == '.'
    , t == dotsText
      || T.isPrefixOf dotsSpace t
      || T.isPrefixOf dotsTab   t   -> LDocEnd
    | h == '%' -> LDirective
    | otherwise -> LContent
{-# INLINE classify #-}

isSkippable :: PLine -> Bool
isSkippable l = case lineKind l of
  LBlank     -> True
  LComment   -> True
  LDirective -> True
  _          -> False
{-# INLINE isSkippable #-}

isSkippableNonDirective :: PLine -> Bool
isSkippableNonDirective l = case lineKind l of
  LBlank   -> True
  LComment -> True
  _        -> False
{-# INLINE isSkippableNonDirective #-}

-- ---------------------------------------------------------------------------
-- Parser monad: pure ([PLine], Map Text Value) -> Either String (a, ...)
-- ---------------------------------------------------------------------------

-- | Result of one parse step. Avoids the @Either String (a, PS)@
-- /pair-of-pair/ shape used previously, which allocated two
-- cells (the 'Either' constructor plus the @(a, PS)@ tuple) on
-- every successful @>>=@. 'Result' uses a single constructor
-- carrying both the value and the next state, halving the per-
-- bind allocation on the success path. ('Err' carries a 'String'
-- which is rarely allocated since failure stops the chain.)
data Result a
  = Ok  !a !PS
  | Err !String

newtype P a = P { unP :: PS -> Result a }

-- | Mode flags packed into a single 'Int' so that 'modifyS'
-- doesn't have to copy individual 'Int' / 'Bool' fields when
-- entering / leaving a scope.
--
-- * bits 0..29: 'parentInd' (biased by 'parentBias' so the
--   @-1@ sentinel fits in the unsigned slot)
-- * bit 30:    'inMapValue'
-- * bit 31:    'flowSpannedNewline'
data PS = PS
  { psLines     :: ![PLine]
  , psAnchors   :: !(Map Text Value)
    -- ^ Anchor environment, reset between documents.
  , psShortcuts :: !(Map Text Text)
    -- ^ %TAG shorthand prefixes ('!handle!' → expansion).
    -- Reset between documents.
  , psFlags     :: {-# UNPACK #-} !Int
  }

parentBias :: Int
parentBias = 1

parentMask :: Int
parentMask = 0x3FFFFFFF

inMapValueBit, flowSpannedBit :: Int
inMapValueBit  = 30
flowSpannedBit = 31

packFlags :: Int -> Bool -> Bool -> Int
packFlags pInd inMV fSpan =
  let !p   = (pInd + parentBias) Bits..&. parentMask
      !mv  = if inMV  then Bits.bit inMapValueBit  else 0
      !fs  = if fSpan then Bits.bit flowSpannedBit else 0
  in p Bits..|. mv Bits..|. fs
{-# INLINE packFlags #-}

psParentInd :: PS -> Int
psParentInd s = (psFlags s Bits..&. parentMask) - parentBias
{-# INLINE psParentInd #-}

psInMapValue :: PS -> Bool
psInMapValue s = Bits.testBit (psFlags s) inMapValueBit
{-# INLINE psInMapValue #-}

psFlowSpannedNewline :: PS -> Bool
psFlowSpannedNewline s = Bits.testBit (psFlags s) flowSpannedBit
{-# INLINE psFlowSpannedNewline #-}

setParentInd :: Int -> PS -> PS
setParentInd !i s = s { psFlags =
    (psFlags s Bits..&. Bits.complement parentMask)
    Bits..|. ((i + parentBias) Bits..&. parentMask) }
{-# INLINE setParentInd #-}

setInMapValue :: Bool -> PS -> PS
setInMapValue True  s = s { psFlags = Bits.setBit   (psFlags s) inMapValueBit }
setInMapValue False s = s { psFlags = Bits.clearBit (psFlags s) inMapValueBit }
{-# INLINE setInMapValue #-}

setFlowSpanned :: Bool -> PS -> PS
setFlowSpanned True  s = s { psFlags = Bits.setBit   (psFlags s) flowSpannedBit }
setFlowSpanned False s = s { psFlags = Bits.clearBit (psFlags s) flowSpannedBit }
{-# INLINE setFlowSpanned #-}

runP :: P a -> PS -> Either String (a, PS)
runP (P f) s = case f s of
  Ok  a s' -> Right (a, s')
  Err e    -> Left e
{-# INLINE runP #-}

instance Functor P where
  fmap f (P g) = P (\s -> case g s of
    Ok a s' -> Ok (f a) s'
    Err e   -> Err e)
  {-# INLINE fmap #-}

instance Applicative P where
  pure x = P (\s -> Ok x s)
  {-# INLINE pure #-}
  P pf <*> P px = P (\s -> case pf s of
    Err e   -> Err e
    Ok f s' -> case px s' of
      Err e    -> Err e
      Ok x s'' -> Ok (f x) s'')
  {-# INLINE (<*>) #-}
  P g *> P h = P (\s -> case g s of
    Err e   -> Err e
    Ok _ s' -> h s')
  {-# INLINE (*>) #-}

instance Monad P where
  P g >>= k = P (\s -> case g s of
    Err e   -> Err e
    Ok a s' -> unP (k a) s')
  {-# INLINE (>>=) #-}

instance MonadFail P where
  fail = failP
  {-# INLINE fail #-}

failP :: String -> P a
failP msg = P (\_ -> Err msg)
{-# INLINE failP #-}

getS :: P PS
getS = P (\s -> Ok s s)
{-# INLINE getS #-}

modifyS :: (PS -> PS) -> P ()
modifyS f = P (\s -> Ok () (f s))
{-# INLINE modifyS #-}

getLines :: P [PLine]
getLines = P (\s -> Ok (psLines s) s)
{-# INLINE getLines #-}

setLines :: [PLine] -> P ()
setLines ls = P (\s -> Ok () (s { psLines = ls }))
{-# INLINE setLines #-}

popLine :: P (Maybe PLine)
popLine = P $ \s ->
  let go [] = (Nothing, [])
      go (x:rest)
        | isSkippable x = go rest
        | otherwise     = (Just x, rest)
      (mx, ls') = go (psLines s)
  in Ok mx (s { psLines = ls' })
{-# INLINE popLine #-}

-- | Drop the next non-skippable line without allocating a
-- 'Maybe' wrapper. Used at sites that have already peeked /
-- pattern-matched the head and only need to advance the cursor.
dropLine :: P ()
dropLine = P $ \s ->
  let go []                       = []
      go (x:rest) | isSkippable x = go rest
                  | otherwise     = rest
  in Ok () (s { psLines = go (psLines s) })
{-# INLINE dropLine #-}

peekLine :: P (Maybe PLine)
peekLine = P $ \s ->
  let go [] = Nothing
      go (x:xs)
        | isSkippable x = go xs
        | otherwise     = Just x
  in Ok (go (psLines s)) s
{-# INLINE peekLine #-}

pushLine :: PLine -> P ()
pushLine l = P (\s -> Ok () (s { psLines = l : psLines s }))
{-# INLINE pushLine #-}

recordAnchor :: Text -> Value -> P ()
recordAnchor name v =
  modifyS (\s -> s { psAnchors = Map.insert name v (psAnchors s) })

resolveAnchor :: Text -> P Value
resolveAnchor name = do
  s <- getS
  case Map.lookup name (psAnchors s) of
    Just v  -> pure v
    Nothing -> failP ("YAML: alias *" ++ T.unpack name ++ " has no anchor")

resetAnchors :: P ()
resetAnchors = modifyS (\s -> s { psAnchors = Map.empty })

resetShortcuts :: P ()
resetShortcuts = modifyS (\s -> s { psShortcuts = Map.empty })

recordShortcut :: Text -> Text -> P ()
recordShortcut handle prefix =
  modifyS (\s -> s { psShortcuts = Map.insert handle prefix (psShortcuts s) })

-- | Run an action with 'psParentInd' temporarily set to @i@,
-- restoring the previous value afterwards.
withParentInd :: Int -> P a -> P a
withParentInd !i action = do
  saved <- psParentInd <$> getS
  modifyS (setParentInd i)
  x <- action
  modifyS (setParentInd saved)
  pure x

getParentInd :: P Int
getParentInd = psParentInd <$> getS

withInMapValue :: Bool -> P a -> P a
withInMapValue !b action = do
  saved <- psInMapValue <$> getS
  modifyS (setInMapValue b)
  x <- action
  modifyS (setInMapValue saved)
  pure x

getInMapValue :: P Bool
getInMapValue = psInMapValue <$> getS

lookupShortcut :: Text -> P (Maybe Text)
lookupShortcut handle = do
  s <- getS
  pure (Map.lookup handle (psShortcuts s))

-- ---------------------------------------------------------------------------
-- Stream / document
-- ---------------------------------------------------------------------------

parseStream :: [PLine] -> Either String [Document]
parseStream lns =
  case runP (loop True True)
            (PS lns Map.empty Map.empty (packFlags 0 False False)) of
    Left err      -> Left err
    Right (ds, _) -> Right ds
  where
    loop !first !prevExplicitEnd = do
      ls <- getLines
      case dropWhile isSkippableNonDirective ls of
        []      -> pure []
        (l : _) -> do
          let canBeBare = first || prevExplicitEnd
              isExplicitStart = lineKind l == LDocStart
              isDirective    = lineKind l == LDirective
          when (isDirective && not first && not prevExplicitEnd) $
            failP $ "directive without preceding '...' marker (line "
                    ++ show (lineNo l) ++ ")"
          unless (canBeBare || isExplicitStart || isDirective) $
            failP $ "second document without '---' marker (line "
                    ++ show (lineNo l) ++ ")"
          d        <- parseDocument
          progress <- checkProgress (lineNo l)
          if progress
            then do
              ds <- loop False (docExplicitEnd d)
              pure (d : ds)
            else
              failP $ "stray content (line " ++ show (lineNo l) ++ ")"

    checkProgress prevLine = do
      ls <- getLines
      pure $ case dropWhile isSkippable ls of
        []      -> True
        (l : _) -> lineNo l /= prevLine

-- | Consume any leading directives ('%YAML ...', '%TAG ...') from
-- the line stream. Returns whether at least one directive was
-- present. Validates the directive syntax: '%YAML' takes a single
-- version token, '%TAG' takes exactly two arguments, and '%YAML'
-- can appear at most once per document.
consumeDirectives :: P Bool
consumeDirectives = go False False
  where
    go !sawAny !sawYaml = do
      ls <- getLines
      case dropWhile isSkippableNonDirective ls of
        (l : rest) | lineKind l == LDirective -> do
          setLines rest
          let body = stripInlineComment (lineBody l)
              args = T.words (T.drop 1 body)   -- drop leading '%'
          case args of
            ("YAML" : ver : extra) -> do
              when sawYaml $
                failP ("duplicate %YAML directive (line "
                       ++ show (lineNo l) ++ ")")
              when (not (null extra)) $
                failP ("extra words on %YAML directive (line "
                       ++ show (lineNo l) ++ ")")
              when (not (validYamlVersion ver)) $
                failP ("invalid %YAML version " ++ T.unpack ver
                       ++ " (line " ++ show (lineNo l) ++ ")")
              go True True
            ("TAG" : handle : prefix : []) -> do
              recordShortcut handle prefix
              go True sawYaml
            ("TAG" : _) ->
              failP ("malformed %TAG directive (line "
                     ++ show (lineNo l) ++ ")")
            ("YAML" : _) ->
              failP ("malformed %YAML directive (line "
                     ++ show (lineNo l) ++ ")")
            _ -> go True sawYaml   -- unknown / reserved directive
        _ -> pure sawAny

validYamlVersion :: Text -> Bool
validYamlVersion t = case T.splitOn (T.pack ".") t of
  [maj, min_]
    | T.all isDigit_ maj && T.all isDigit_ min_
    , not (T.null maj) && not (T.null min_) -> True
  _ -> False
  where
    isDigit_ c = c >= '0' && c <= '9'

parseDocument :: P Document
parseDocument = do
  -- %TAG shortcuts are scoped to a single document — clear any
  -- left over from the previous one before we parse the new
  -- prologue (spec §6.8.2).
  resetShortcuts
  hadDirective <- consumeDirectives
  ls0' <- getLines
  let nextSig = dropWhile isSkippableNonDirective ls0'
  (directives, ls1) <- case nextSig of
    (l : rest) | lineKind l == LDocStart -> do
      let body = lineBody l
          tail_ = T.stripStart (T.drop 3 body)
          isInlineBlockScalar = case T.uncons tail_ of
            Just ('|', _) -> True
            Just ('>', _) -> True
            _             -> False
          virtInd | isInlineBlockScalar = -1
                  | otherwise           = lineIndent l
      -- '--- &anchor a: b' (an anchor immediately followed by a
      -- mapping pair on the same line as '---') is invalid per
      -- the test suite (CXX2 / mapping-with-anchor-on-document-
      -- start-line).
      case T.uncons tail_ of
        Just ('&', restA) ->
          let (_anchor, afterAnchor) = takeAnchorName restA
              afterStripped = T.stripStart afterAnchor
          in case findKeyValueSplit afterStripped of
               Just _ ->
                 failP $ "anchor immediately followed by mapping on '---' line (line "
                         ++ show (lineNo l) ++ ")"
               Nothing -> pure ()
        _ -> pure ()
      pure $ if T.null tail_
                  then (True, rest)
                  else (True,
                        PLine (lineNo l) virtInd
                              LContent tail_ tail_ : rest)
    (l : _) | hadDirective ->
      failP ("missing '---' after directive (line "
             ++ show (lineNo l) ++ ")")
    [] | hadDirective ->
      failP "directive without document"
    _ -> pure (False, ls0')
  setLines ls1
  resetAnchors
  body <- parseDocBody
  ls2 <- getLines
  (explicitEnd, ls3) <- case dropWhile isSkippable ls2 of
        (l : rest) | lineKind l == LDocEnd -> do
          let extra = T.stripStart (T.drop 3 (lineBody l))
              -- Strip an inline comment if any.
              extra' = T.stripEnd $ case T.uncons extra of
                Just ('#', _) -> T.empty
                _             -> stripInlineComment extra
          unless (T.null extra') $
            failP $ "trailing content after '...' marker: "
                  ++ show extra ++ " (line " ++ show (lineNo l) ++ ")"
          pure (True, rest)
        _ -> pure (False, ls2)
  setLines ls3
  pure (Document directives explicitEnd body)

parseDocBody :: P Value
parseDocBody = do
  mNext <- peekLine
  case mNext of
    Nothing -> pure YNull
    Just l
      | lineKind l == LDocStart -> pure YNull
      | lineKind l == LDocEnd   -> pure YNull
      -- A bare top-level '|' / '>' block scalar at column 0 is
      -- the same as '--- |' / '--- >' for indent purposes — body
      -- lines may live at column 0 (parent < 0).
      | lineIndent l == 0
      , isBlockScalarStart (lineBody l) -> do
          modifyS (\s -> case psLines s of
                           (h : rs) -> s { psLines = h { lineIndent = -1 } : rs }
                           []       -> s)
          parseNode (-1)
      | otherwise               -> parseNode (min 0 (lineIndent l))
  where
    isBlockScalarStart t = case T.uncons t of
      Just ('|', _) -> True
      Just ('>', _) -> True
      _             -> False

-- ---------------------------------------------------------------------------
-- Node dispatch
-- ---------------------------------------------------------------------------

-- | Parse a node whose left margin is at least @minInd@.
parseNode :: Int -> P Value
parseNode !minInd = do
  mNext <- peekLine
  case mNext of
    Nothing -> pure YNull
    Just l
      | lineKind l == LDocStart -> pure YNull
      | lineKind l == LDocEnd   -> pure YNull
      | lineIndent l < minInd   -> pure YNull
      | otherwise -> dispatch l

dispatch :: PLine -> P Value
dispatch l0 = do
  -- Strip leading TAB characters. They're not allowed as block
  -- indentation per spec §6.1 but real-world inputs use them as
  -- 'separation' whitespace between a structural marker and the
  -- following node — '\\t{}', '\\t- x', '\\t"…"' all parse OK
  -- in libfyaml etc.
  let body0 = lineBody l0
  case T.uncons body0 of
    Just ('\t', _) -> do
      let l = l0 { lineBody    = T.dropWhile (== '\t') body0
                 , lineRawBody = T.dropWhile (== '\t') (lineRawBody l0)
                 }
      modifyS (\s -> case psLines s of
                       (top : rs) | lineNo top == lineNo l0 ->
                         s { psLines = l : rs }
                       _ -> s)
      dispatchOn l
    _ -> dispatchOn l0
  where
    dispatchOn l = case T.uncons (lineBody l) of
       Just (h, _) -> case h of
         '!'  -> parseTagged
         '&'  -> parseAnchored
         '*'  -> case findAliasKeySplit (lineBody l) of
                   Just (aliasName, vRest) ->
                     parseBlockMapAliasFirst (lineIndent l)
                       aliasName vRest
                   Nothing -> parseAlias
         '|'  -> parseBlockScalar Literal
         '>'  -> parseBlockScalar Folded
         '['  -> consumeFlowFromHead >>= maybeFlowAsBlockKey (lineIndent l)
         '{'  -> consumeFlowFromHead >>= maybeFlowAsBlockKey (lineIndent l)
         '"'  -> parseQuotedScalarLine '"'  l
         '\'' -> parseQuotedScalarLine '\'' l
         _    -> parseBlockOrPlain l
       Nothing -> parseBlockOrPlain l

-- | Quoted scalars can be the entire node body, or the start of a
-- @key: \"…\"@ pair when the closing quote is followed by a colon. We
-- look ahead for a top-level @:@ after the closing quote and dispatch
-- to 'parseBlockMap' if found, otherwise consume the quoted scalar.
--
-- Quoted scalars may span multiple lines per YAML 1.2; if the close
-- quote is not on the same line we splice continuation lines into
-- the buffer until it is.
parseQuotedScalarLine :: Char -> PLine -> P Value
parseQuotedScalarLine q l =
  -- Fast path: for double-quoted strings, a body that ends with
  -- the closing '\"' can't have a trailing 'key: value' pair.
  -- Single-quoted strings can validly contain '\\\'' in the
  -- middle of a /plain/ value, so be conservative.
  let body = lineBody l
      fast = q == '"'
        && case T.unsnoc body of
             Just (_, c) -> c == q
             _           -> False
  in if fast
       then doQuoted
       else case findKeyValueSplit body of
         Just (k, vRest) -> parseBlockMap (lineIndent l) k vRest
         Nothing -> doQuoted
  where
    doQuoted = do
      dropLine
      consumeQuotedAt q (lineIndent l)
        (preserveTrailingEscape (lineBody l) (lineRawBody l))

-- | If the trimmed line body ends in @\\@ that's not itself
-- escaped, take the next character /verbatim/ from the raw body
-- (so an escape argument like @\\<TAB>@ survives the trailing-WS
-- strip done by 'preprocess'). Any whitespace /after/ that escape
-- argument is still stripped.
preserveTrailingEscape :: Text -> Text -> Text
preserveTrailingEscape stripped raw = case T.unsnoc stripped of
  Just (_, '\\') | not (endsEvenBackslashes stripped) ->
    -- Reach into raw at the position right after the trailing '\'.
    let idx = T.length stripped
    in if idx < T.length raw
         then stripped <> T.singleton (T.index raw idx)
         else stripped
  _ -> stripped
  where
    endsEvenBackslashes t =
      even (T.length (T.takeWhileEnd (== '\\') t))

-- | Greedily extend a quoted-scalar buffer with successor lines
-- until the matching close quote is found. Per YAML 1.2 §7.3.1-2:
--
-- * A single line break between non-empty lines folds to a single
--   space.
-- * A run of @n@ empty lines between non-empty content yields @n@
--   line breaks (the surrounding break itself is consumed).
--
-- Any text after the close quote on the final line is pushed back
-- as a virtual line so the surrounding context can keep parsing.
consumeQuoted :: Char -> Text -> P Value
consumeQuoted q = consumeQuotedAt q (-1)

-- | Like 'consumeQuoted' but the caller knows the indent of the
-- line that opened the quote; continuation lines must be at strictly
-- greater indent.
consumeQuotedAt :: Char -> Int -> Text -> P Value
consumeQuotedAt q !openInd = go0 False
  where
    parser = case q of '"' -> parseDQ; _ -> parseSQ

    -- The very first attempt; no fold prefix has been emitted yet.
    go0 !multi !buf = case parser 0 buf of
      Just (v, p)   -> finish multi v (bDrop p buf)
      Nothing       -> readMore multi buf 0

    -- @blanks@ counts consecutive empty continuation lines we've
    -- absorbed since the last non-empty (or the opening) line.
    -- We pop the raw next line (not 'popLine', which would skip
    -- blank / comment lines — those are significant inside a
    -- multi-line quoted scalar).
    readMore _multi !buf !blanks = do
      ls <- getLines
      case ls of
        []       -> failP "YAML: unterminated quoted scalar"
        (l' : _) | lineKind l' == LDocStart || lineKind l' == LDocEnd ->
          failP $ "document marker inside quoted scalar (line "
                  ++ show (lineNo l') ++ ")"
        -- Continuations must be at /at least/ the same indent as
        -- the line that opened the quote. A line at lower indent
        -- belongs to the surrounding scope.
        (l' : _) | openInd > 0
                 , lineKind l' == LContent
                 , lineIndent l' < openInd ->
          failP $ "wrong-indented quoted-scalar continuation (line "
                  ++ show (lineNo l') ++ ")"
        (l' : rest) -> do
          setLines rest
          let body0 = lineBody l'
              raw   = lineRawBody l'
              -- Use the raw body when the trimmed line body ends
              -- in '\\' so that '\\<TAB>' survives.
              body  = preserveTrailingEscape body0 raw
              isBlank = T.null (T.strip body)
              body' = T.dropWhile (\c -> c == ' ' || c == '\t') body
              -- DQ-only: a bare trailing backslash on the previous
              -- line eats the newline plus any leading whitespace
              -- on the next line (YAML 1.2 §5.7 / §7.5).
              endsWithEscape = q == '"'
                            && case T.unsnoc buf of
                                 Just (_, '\\') -> not (endsEvenBackslashes buf)
                                 _              -> False
          if isBlank
            then readMore True buf (blanks + 1)
            else
              let (buf', joined)
                    | endsWithEscape =
                        (T.init buf <> body', True)
                    | otherwise =
                        let joinSep
                              | blanks == 0 = tSpace
                              | otherwise   = T.replicate blanks tNL
                        in (buf <> joinSep <> body', True)
              in joined `seq` case parser 0 buf' of
                   Just (v, p)   -> finish True v (bDrop p buf')
                   Nothing       -> readMore True buf' 0

    -- A run of trailing backslashes counts as "even" when it
    -- pairs up to "\\\\…", which means no escape at end.
    endsEvenBackslashes t = even (T.length (T.takeWhileEnd (== '\\') t))

    finish multi v rest =
      let trimmed = T.stripStart rest
          -- Was there at least one whitespace char between the
          -- closing quote and 'rest'?
          hadSeparator = bLen trimmed < bLen rest
          stripped = case T.uncons trimmed of
            -- A '#' may only start a comment when preceded by
            -- whitespace; without one it's malformed.
            Just ('#', _)
              | hadSeparator -> T.empty
              | otherwise    -> trimmed
            _ -> T.stripEnd (stripInlineComment trimmed)
      in if T.null stripped
           then pure v
           else case T.uncons stripped of
                  Just (c, _)
                    | c == ':' && multi ->
                        failP "multi-line quoted scalar used as implicit key"
                    -- A ':' trailing after the close quote means
                    -- the scalar is acting as a key. That's only
                    -- valid when this consumeQuoted call wasn't
                    -- already inside a value position (openInd > 0
                    -- means we're inside a parent block context's
                    -- value). Otherwise treat the ':' as malformed
                    -- (e.g. ZL4Z's "a: 'b': c").
                    | c == ':' && openInd > 0 ->
                        failP $ "nested mapping after quoted scalar value"
                    | c == ','  -> pushBack stripped
                    | c == ':'  -> pushBack stripped
                    | c == ']'  -> pushBack stripped
                    | c == '}'  -> pushBack stripped
                    | otherwise ->
                        failP $ "trailing content after quoted scalar: "
                                ++ show stripped
                  Nothing -> pure v
      where
        pushBack s = do
          pushLine (PLine 0 0 LContent s s)
          pure v

parseTagged :: P Value
parseTagged = do
  Just l <- popLine
  let (tg, rest) = breakOnSpace (lineBody l)
      after0 = T.stripStart rest
      after = case T.uncons after0 of
        Just ('#', _) -> T.empty
        _             -> after0
  tag <- expandTagP (lineNo l) tg
  -- Tag tokens can contain URI characters (incl. ',') /inside/
  -- a verbatim '!<...>' wrapper, but a bare tag ('!!str' /
  -- '!foo') may not include ',' or flow indicators. Reject the
  -- bare-tag form when it contains them.
  let isVerbatim = case T.uncons (T.drop 1 tg) of
        Just ('<', _) -> True
        _             -> False
  unless isVerbatim $
    case T.find (\c -> c == ',' || c == '[' || c == ']'
                    || c == '{' || c == '}') tg of
      Just c -> failP $ "invalid tag character " ++ show c
                      ++ " in tag " ++ show tg
                      ++ " (line " ++ show (lineNo l) ++ ")"
      Nothing -> pure ()
  if T.null after
    then do
      mNext <- peekLine
      case mNext of
        Just l2 | lineIndent l2 >= lineIndent l -> do
          v <- parseNode (lineIndent l2)
          pure (YTagged tag v)
        Just l2 | lineKind l2 == LContent
                , let body2 = lineBody l2
                , isBlockScalarHeadOrProp body2 -> do
          -- Adjust the body line's stored indent to 0 so a
          -- following block-scalar's auto-baseline picks up
          -- content at any column > 0 (spec example 8.21).
          modifyS (\s -> case psLines s of
                          (h : rs) -> s { psLines = h { lineIndent = 0 } : rs }
                          _        -> s)
          v <- parseNode 0
          pure (YTagged tag v)
        _ -> pure (YTagged tag YNull)
    else do
      -- Update both 'lineBody' and 'lineRawBody' so that
      -- consumeQuoted's preserveTrailingEscape works on the
      -- right slice of the line.
      pushLine l { lineBody = after, lineRawBody = after }
      v <- parseNode (lineIndent l)
      pure (YTagged tag v)

parseAnchored :: P Value
parseAnchored = do
  inMapValue <- getInMapValue
  -- Once we descend below the immediate mapping-value dispatch,
  -- further parseNode calls are NOT in mapping-value position.
  withInMapValue False (parseAnchoredImpl inMapValue)

parseAnchoredImpl :: Bool -> P Value
parseAnchoredImpl !inMapValue = do
  Just l <- popLine
  let body = lineBody l
      (name, rest) = takeAnchorName (T.drop 1 body)
      after0 = T.stripStart rest
      after = case T.uncons after0 of
        Just ('#', _) -> T.empty
        _             -> after0
  -- '&anchor - foo' / '&anchor ? key' / '&anchor : value' are
  -- invalid: an anchor cannot label a block-sequence / explicit
  -- mapping marker on the same line (the marker introduces its
  -- own nested structure with its own indentation requirements).
  case T.uncons after of
    Just ('-', rest1) | startsWithSeparator rest1 ->
      failP $ "anchor immediately followed by block indicator '-' (line "
              ++ show (lineNo l) ++ ")"
    Just ('?', rest1) | startsWithSeparator rest1 ->
      failP $ "anchor immediately followed by explicit-key marker (line "
              ++ show (lineNo l) ++ ")"
    -- '&anchor *alias' is invalid: an alias is itself a node
    -- reference and can't be re-labelled with a new anchor.
    Just ('*', _) ->
      failP $ "anchor immediately followed by alias (line "
              ++ show (lineNo l) ++ ")"
    _ -> pure ()
  -- If the rest of the line introduces a mapping (i.e. there's a
  -- top-level @": "@ in the remainder), the anchor only binds to
  -- the /key/, not to the surrounding mapping. This matches the
  -- YAML 1.2 node-anchoring model where the anchor precedes the
  -- specific node it labels.
  case findKeyValueSplit after of
    Just (k, vRest) -> do
      let keyVal = YString k
      recordAnchor name keyVal
      pushLine l { lineBody = after, lineRawBody = after }
      parseBlockMap (lineIndent l) k vRest
    Nothing -> do
      v <- if T.null after
             then do
               mNext <- peekLine
               case mNext of
                 Just l2
                   | lineIndent l2 > lineIndent l
                   , isJustAnchorScalar (lineBody l2) -> do
                       -- '&outer' followed by '&inner scalar'
                       -- (no ':'/structural marker) is invalid:
                       -- two anchors on the same scalar node
                       -- (4JVG / scalar-value-with-two-anchors).
                       failP $ "node has two consecutive anchors (line "
                               ++ show (lineNo l2) ++ ")"
                 Just l2 | lineIndent l2 > lineIndent l ->
                     parseNode (lineIndent l2)
                 -- A bare anchor at the same column as a following
                 -- block sequence binds to that sequence.
                 Just l2
                   | lineIndent l2 == lineIndent l
                   , isSeqItem (lineBody l2) ->
                       parseBlockSeq (lineIndent l2)
                 -- A bare anchor whose next line at the same
                 -- column is another node-property line chains
                 -- into that property; the whole stack labels
                 -- the eventual node. EXCEPT when the anchor is
                 -- itself the value of a mapping entry: a
                 -- same-column property line then is a sibling
                 -- (not a chained property), so the anchor refers
                 -- to Null. See H7J7 (node-anchor-not-indented).
                 Just l2
                   | lineIndent l2 == lineIndent l
                   , isNodePropertyLine (lineBody l2)
                   , not inMapValue ->
                       parseNode (lineIndent l2)
                 -- A bare anchor on its own line at column > 0
                 -- binds to the next same-column content line.
                 Just l2
                   | lineIndent l2 == lineIndent l
                   , lineIndent l > 0
                   , lineKind l2 == LContent ->
                       parseNode (lineIndent l2)
                 -- An anchor whose line is more-indented than the
                 -- following block sequence binds to that
                 -- sequence (e.g. 'seq:\\n &anchor\\n- a\\n- b').
                 Just l2
                   | lineKind l2 == LContent
                   , lineIndent l2 < lineIndent l
                   , isSeqItem (lineBody l2) ->
                       parseBlockSeq (lineIndent l2)
                 -- At column 0 we only chain into a same-column
                 -- /plain scalar/. In mapping-value position the
                 -- same-column line is the next sibling.
                 Just l2
                   | lineIndent l2 == lineIndent l
                   , lineIndent l == 0
                   , lineKind l2 == LContent
                   , not inMapValue
                   , not (isSeqItem (lineBody l2))
                   , not (isExplicitKey (lineBody l2))
                   , not (isNodePropertyLine (lineBody l2))
                   , Nothing <- findKeyValueSplit (lineBody l2) ->
                       parseNode (lineIndent l2)
                 _ -> pure YNull
             else do
               pushLine l { lineBody = after, lineRawBody = after }
               parseNode (lineIndent l)
      recordAnchor name v
      pure v

-- | True when the line is shaped like '&anchor scalar' — i.e.
-- starts with an anchor whose remainder is a plain scalar (no
-- ':' / sequence marker).
isJustAnchorScalar :: Text -> Bool
isJustAnchorScalar t = case T.uncons t of
  Just ('&', rest) ->
    let (_name, after) = takeAnchorName rest
        body = T.stripStart after
    in not (T.null body)
       && case findKeyValueSplit body of
            Just _  -> False
            Nothing -> not (isSeqItem body)
                    && not (isExplicitKey body)
  _ -> False

-- | True when the line begins with a node-property indicator
-- ('!' tag or '&' anchor) followed by separator / EOL.
isNodePropertyLine :: Text -> Bool
isNodePropertyLine t = case T.uncons t of
  Just ('!', _) -> True
  Just ('&', _) -> True
  _             -> False

-- | True when the line begins with a block-scalar header indicator
-- ('|' / '>') or a node-property indicator.
isBlockScalarHeadOrProp :: Text -> Bool
isBlockScalarHeadOrProp t = case T.uncons t of
  Just ('|', _) -> True
  Just ('>', _) -> True
  Just ('!', _) -> True
  Just ('&', _) -> True
  _             -> False

parseAlias :: P Value
parseAlias = do
  Just l <- popLine
  let body = lineBody l
      (name, rest) = takeAnchorName (T.drop 1 body)
      after0 = T.stripStart rest
      after = case T.uncons after0 of
        Just ('#', _) -> T.empty
        _             -> after0
  if T.null after
    then resolveAnchor name
    else do
      pushLine (PLine (lineNo l) (lineIndent l) LContent after after)
      resolveAnchor name

-- | Characters legal in an anchor / alias name (YAML 1.2 §6.9.2).
-- Anchors exclude flow indicators and whitespace; the colon
-- /is/ allowed inside an anchor name (so @&an:chor@ is legal),
-- but only when followed by another anchor char — see
-- 'takeAnchorName'.
isAnchorChar :: Char -> Bool
isAnchorChar c =
  not (c == ',' || c == '[' || c == ']' || c == '{' || c == '}'
       || c == ' ' || c == '\t' || c == '\n' || c == '\r')

-- | Read an anchor / alias name. Treats @:@ as part of the name
-- only when followed by another anchor character (so
-- @&an:chor@ → @\"an:chor\"@) but as a terminator otherwise (so
-- @&a:@ followed by a space → @\"a\"@ + remainder @\":\"@).
takeAnchorName :: Text -> (Text, Text)
takeAnchorName t = goT 0
  where
    !len = bLen t
    goT !i
      | i >= len = (t, T.empty)
      | otherwise =
          let !b = bAt t i
          in if isAnchorByte b
               then goT (i + 1)
               else (bTake i t, bDrop i t)

isAnchorByte :: Word8 -> Bool
isAnchorByte b =
  -- ASCII byte: reject the same chars as 'isAnchorChar'.
  -- Non-ASCII bytes (≥ 0x80, including UTF-8 leading and
  -- continuation bytes) are always part of the name.
  case b of
    44  -> False    -- ','
    91  -> False    -- '['
    93  -> False    -- ']'
    123 -> False    -- '{'
    125 -> False    -- '}'
    32  -> False    -- ' '
    9   -> False    -- '\t'
    10  -> False    -- '\n'
    13  -> False    -- '\r'
    _   -> True
{-# INLINE isAnchorByte #-}

breakOnSpace :: Text -> (Text, Text)
breakOnSpace = T.break (\c -> c == ' ' || c == '\t')

-- ---------------------------------------------------------------------------
-- Flow style
-- ---------------------------------------------------------------------------

consumeFlowFromHead :: P Value
consumeFlowFromHead = do
  Just l <- popLine
  modifyS (setFlowSpanned False)
  inMV <- getInMapValue
  parent <- getParentInd
  -- Inside any block container (mapping value or non-zero
  -- parent ind), use the parent-+-1 column as the lower bound
  -- for flow continuations so we can detect tab-as-indent
  -- (Y79Y/003).
  let openInd | inMV       = max 1 (parent + 1)
              | parent >= 0 && parent < ourInd = parent + 1
              | otherwise  = -1
      ourInd = lineIndent l
  consumeFlowAt openInd (lineBody l)

-- | After a flow node has been consumed, see if there's a virtual
-- ':' line waiting in the stream — if so, the flow node is the
-- /key/ of a block mapping. Continue parsing as a block mapping
-- with this flow value as the first key.
maybeFlowAsBlockKey :: Int -> Value -> P Value
maybeFlowAsBlockKey !ind k = do
  mNext <- peekLine
  case mNext of
    Just l
      | lineIndent l == ind
      , let body = lineBody l
      , body == tColonStr
        || T.isPrefixOf tColonSpace body
        || T.isPrefixOf tColonTab   body -> do
          spanned <- psFlowSpannedNewline <$> getS
          when spanned $
            failP $ "flow node spanning newline used as block-mapping key (line "
                    ++ show (lineNo l) ++ ")"
          dropLine
          let after = if body == tColonStr then T.empty
                                            else T.drop 2 body
          v <- parseImplicitMapValue ind after
          rest <- collectFlowMapEntries ind
          pure (YMap (V.fromList ((k, v) : rest)))
    _ -> pure k

-- | Collect more block-mapping entries after a flow-as-key entry.
-- Mirrors 'parseBlockMap.collect' but specialized to start from
-- an arbitrary state.
collectFlowMapEntries :: Int -> P [(Value, Value)]
collectFlowMapEntries !ind = go []
  where
    go acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure (reverse acc)
        Just l
          | lineIndent l /= ind -> pure (reverse acc)
          | isSeqItem (lineBody l) -> pure (reverse acc)
          | startsWithTab (lineBody l) ->
              failP $ "tab character used as indentation (line "
                      ++ show (lineNo l) ++ ")"
          | isExplicitKey (lineBody l) -> do
              k <- readEntryKey
              v <- readEntryValue
              go ((k, v) : acc)
          | otherwise -> case findAliasKeySplit (lineBody l) of
              Just (aliasName, vRest) -> do
                dropLine
                k <- resolveAnchor aliasName
                v <- parseImplicitMapValue ind vRest
                go ((k, v) : acc)
              Nothing -> case findKeyValueSplit (lineBody l) of
                Just (k, vRest) -> do
                  dropLine
                  let (anchors, k') = stripKeyProperties k
                  v <- parseImplicitMapValue ind vRest
                  let kv = YString k'
                  mapM_ (\an -> recordAnchor an kv) anchors
                  go ((kv, v) : acc)
                Nothing -> pure (reverse acc)

    -- (cheap inline of parseExplicitMap.readExplicitPart "?")
    readEntryKey = do
      Just l <- popLine
      let body = lineBody l
          afterMarker = if body == tQuestStr then T.empty
                                              else T.drop 1 body
          rest = T.stripStart (T.drop 1 afterMarker)
      if T.null rest
        then do
          mNext <- peekLine
          case mNext of
            Just l2 | lineIndent l2 > lineIndent l -> parseNode (lineIndent l2)
            _ -> pure YNull
        else do
          pushLine (PLine (lineNo l) (lineIndent l + 2) LContent rest rest)
          parseNode (lineIndent l + 2)

    readEntryValue = do
      mPL <- peekLine
      case mPL of
        Just l | lineIndent l == ind
                 , (lineBody l == tColonStr
                    || T.isPrefixOf tColonSpace (lineBody l)
                    || T.isPrefixOf tColonTab   (lineBody l)) -> do
            Just l' <- popLine
            let body = lineBody l'
                afterMarker = if body == tColonStr then T.empty
                                                    else T.drop 1 body
                rest = T.stripStart (T.drop 1 afterMarker)
            if T.null rest
              then do
                mNext <- peekLine
                case mNext of
                  Just l2 | lineIndent l2 > lineIndent l' ->
                    parseNode (lineIndent l2)
                  _ -> pure YNull
              else do
                pushLine (PLine (lineNo l') (lineIndent l' + 2)
                                LContent rest rest)
                parseNode (lineIndent l' + 2)
        _ -> pure YNull

-- | Walk the parsed flow value and (a) register any embedded
-- 'YAnchored' nodes, (b) resolve any alias placeholders left by
-- 'parseFlowAlias'.
recordFlowAnchors :: Value -> P ()
recordFlowAnchors = goV
  where
    goV v = case v of
      YAnchored (Anchor n) inner -> do
        recordAnchor n inner
        goV inner
      YTagged _ inner            -> goV inner
      YSeq xs                    -> mapM_ goV (toListV xs)
      YMap kvs                   -> mapM_ (\(k, x) -> goV k >> goV x)
                                          (toListV kvs)
      _                          -> pure ()

    toListV v = V.toList v

resolveFlowAliases :: Value -> P Value
resolveFlowAliases = goV
  where
    goV (YString t)
      | T.isPrefixOf tAliasSentinel t = do
          let nm = bDrop (bLen tAliasSentinel) t
          resolveAnchor nm
    goV (YAnchored a v)         = YAnchored a <$> goV v
    goV (YTagged   a v)         = YTagged   a <$> goV v
    goV (YSeq xs)               = YSeq <$> V.mapM goV xs
    goV (YMap kvs)              = YMap <$> V.mapM
                                     (\(k, x) -> (,) <$> goV k <*> goV x) kvs
    goV v                       = pure v

consumeFlow :: Text -> P Value
consumeFlow = consumeFlowAt (-1)

consumeFlowAt :: Int -> Text -> P Value
consumeFlowAt !openInd = go
  where
    -- Strip end-of-line comments before adding a new chunk to the
    -- buffer. Flow nodes may contain comments between elements,
    -- per the YAML 1.2 grammar.
    stripFlowComment t = T.stripEnd (stripInlineComment t)

    go buf0 = let buf = stripFlowComment buf0 in case scanFlow buf of
      ScanComplete v rest -> do
        recordFlowAnchors v
        v' <- resolveFlowAliases v
        let !s = T.stripStart rest
        case T.uncons s of
          Nothing -> pure v'
          -- Trailing block-context indicators ('- ', ': ', '? ',
          -- bare '-' / ':' / '?') immediately after a flow node
          -- close are malformed (P2EQ /
          -- invalid-block-mapping-key-on-same-line-as-previous-key).
          Just (c, _)
            -- Trailing block indicators or bare text on the same
            -- line after a flow node close are malformed. Only a
            -- ':' / ',' / ']' / '}' terminator (the surrounding
            -- flow context's own punctuation) is acceptable here.
            | c == ',' || c == ':' || c == ']' || c == '}' -> do
                pushLine (PLine 0 0 LContent s s)
                pure v'
            | otherwise ->
                failP $ "trailing content after flow node: " ++ show s
      ScanIncomplete -> do
        ls <- getLines
        case ls of
          []         -> failP "YAML: unterminated flow node"
          (l' : _) | lineKind l' == LDocStart || lineKind l' == LDocEnd ->
            failP $ "document marker inside flow node (line "
                    ++ show (lineNo l') ++ ")"
          (l' : _)
            | openInd > 0
            , lineKind l' == LContent
            , lineIndent l' < openInd ->
                failP $ "wrong-indented flow continuation (line "
                        ++ show (lineNo l') ++ ")"
          (l' : _)
            | openInd > 0
            , lineKind l' == LContent
            , lineIndent l' == 0
            , case T.uncons (lineBody l') of
                Just ('\t', _) -> True
                _              -> False ->
                failP $ "tab as indentation in flow continuation (line "
                        ++ show (lineNo l') ++ ")"
          (l' : rs)  -> do
            setLines rs
            modifyS (setFlowSpanned True)
            -- Join with a sentinel character (\\1) instead of a
            -- plain space so downstream parsers can detect that
            -- a structural element spanned a newline (used for
            -- the implicit-key-followed-by-newline check).
            go (buf <> tSOH <> lineBody l')

data ScanResult
  = ScanComplete !Value !Text
  | ScanIncomplete

scanFlow :: Text -> ScanResult
scanFlow buf = case parseFlowValue 0 buf of
  Just (v, p) -> ScanComplete v (bDrop p buf)
  Nothing     -> ScanIncomplete

parseFlowValue :: Int -> Text -> Maybe (Value, Int)
parseFlowValue !p0 t =
  let !len = bLen t
      p    = skipFlowWS p0 t
  in if p >= len
       then Nothing
       else case bAt t p of
              91  -> parseFlowSeq (p + 1) t   -- '['
              123 -> parseFlowMap (p + 1) t   -- '{'
              34  -> parseDQ p t              -- '"'
              39  -> parseSQ p t              -- '\''
              33  -> parseFlowTagged p t      -- '!'
              38  -> parseFlowAnchored p t    -- '&'
              42  -> parseFlowAlias p t       -- '*'
              _   -> parseFlowPlain p t

-- | Tagged node in flow context. Reads the tag token (everything
-- up to whitespace / flow stopper) and then optionally parses a
-- following node; @!!str@ alone is allowed and means "tagged
-- null".
parseFlowTagged :: Int -> Text -> Maybe (Value, Int)
parseFlowTagged !p t =
  let !len = bLen t
      goT !i
        | i >= len  = i
        | otherwise =
            let !c = bAt t i
            in if c == w8Space || c == w8Tab || c == w8SOH
                  || c == w8Comma || c == w8RBrack || c == w8RBrace
                 then i
                 else goT (i + 1)
      !p1     = goT p
      tagText = bSlice t p p1
      tag     = expandTag tagText
      p2      = skipFlowWS p1 t
  in if p2 >= len
       then Just (YTagged tag YNull, p2)
       else case bAt t p2 of
              44  -> Just (YTagged tag YNull, p1)   -- ','
              93  -> Just (YTagged tag YNull, p1)   -- ']'
              125 -> Just (YTagged tag YNull, p1)   -- '}'
              58  | colonIsSeparator (p2 + 1) t ->  -- ':'
                       Just (YTagged tag YNull, p1)
              _   -> case parseFlowValue p2 t of
                       Just (v, p3) -> Just (YTagged tag v, p3)
                       Nothing      -> Just (YTagged tag YNull, p2)

-- | Anchor in flow context: read the anchor name and parse the
-- labelled value, wrapping it in 'YAnchored' so the post-pass
-- 'recordFlowAnchors' picks it up and registers it with the
-- enclosing parser state.
parseFlowAnchored :: Int -> Text -> Maybe (Value, Int)
parseFlowAnchored !p t =
  let !len = bLen t
      goN !i
        | i >= len  = i
        | otherwise =
            let !c = bAt t i
            in if c == w8Space || c == w8Tab || c == w8SOH
                  || c == w8Comma || c == w8RBrack || c == w8RBrace
                 then i
                 else goN (i + 1)
      !endName = goN (p + 1)
      name     = bSlice t (p + 1) endName
      p2       = skipFlowWS endName t
  in if p2 >= len
       then Just (YAnchored (Anchor name) YNull, p2)
       else case bAt t p2 of
              44  -> Just (YAnchored (Anchor name) YNull, endName)
              93  -> Just (YAnchored (Anchor name) YNull, endName)
              125 -> Just (YAnchored (Anchor name) YNull, endName)
              58  | colonIsSeparator (p2 + 1) t ->
                       Just (YAnchored (Anchor name) YNull, endName)
              _   -> case parseFlowValue p2 t of
                       Just (v, p3) -> Just (YAnchored (Anchor name) v, p3)
                       Nothing      -> Just (YAnchored (Anchor name) YNull, p2)

-- | Alias in flow context: emit a 'YAnchored'-tagged placeholder
-- whose value is a sentinel 'YString' starting with @"\\0alias\\0"@.
-- The post-pass resolves these to the registered anchor value.
parseFlowAlias :: Int -> Text -> Maybe (Value, Int)
parseFlowAlias !p t =
  let !len = bLen t
      goN !i
        | i >= len  = i
        | otherwise =
            let !c = bAt t i
            in if c == w8Space || c == w8Tab || c == w8SOH
                  || c == w8Comma || c == w8RBrack || c == w8RBrace
                 then i
                 else goN (i + 1)
      !p1  = goN (p + 1)
      name = bSlice t (p + 1) p1
  in Just (YString (tAliasSentinel <> name), p1)

parseFlowSeq :: Int -> Text -> Maybe (Value, Int)
parseFlowSeq !p0 t =
  let !len = bLen t
      goV !p acc
        | p >= len            = Nothing
        | bAt t p == w8RBrack = Just (YSeq (V.fromList (reverse acc)), p + 1)
        | bAt t p == w8Hash   = Nothing
        | bAt t p == w8STX    = Nothing
        | otherwise = case parseFlowEntry p t of
            Nothing      -> Nothing
            Just (v, p1) ->
              -- Skip any '\\2' comment-break sentinels before
              -- looking for the next separator.
              let p2 = skipFlowWSAndCB p1 t
              in if p2 >= len
                   then Nothing
                   else case bAt t p2 of
                          44 ->   -- ','
                            let p3 = p2 + 1
                            in if p3 < len && bAt t p3 == w8Hash
                                 then Nothing
                                 else goV (skipFlowWS p3 t) (v : acc)
                          93 ->   -- ']'
                            Just (YSeq (V.fromList (reverse (v : acc))), p2 + 1)
                          _  -> Nothing
  in goV (skipFlowWS p0 t) []

-- | Skip whitespace and comment-break sentinels ('\\2'). Used
-- between flow elements where a comment may sit just before the
-- separator.
skipFlowWSAndCB :: Int -> Text -> Int
skipFlowWSAndCB !p t =
  let !len = bLen t
      go !i
        | i >= len  = i
        | otherwise = case bAt t i of
            32 -> go (i + 1)
            9  -> go (i + 1)
            1  -> go (i + 1)
            2  -> go (i + 1)
            _  -> i
  in go p
{-# INLINE skipFlowWSAndCB #-}

-- | A flow-sequence entry can be a single value or a one-pair
-- mapping (with @key: value@ syntax, or just @: value@ for an empty
-- key).
parseFlowEntry :: Int -> Text -> Maybe (Value, Int)
parseFlowEntry = parseFlowEntry' False

-- | When the @explicit@ flag is set, the entry is being parsed
-- under a leading @?@ marker (explicit key); the
-- implicit-key-spans-newline check is skipped.
parseFlowEntry' :: Bool -> Int -> Text -> Maybe (Value, Int)
parseFlowEntry' !explicit !p0 t =
  let !len = bLen t
      p    = skipFlowWS p0 t
  in if p >= len
       then Nothing
       else
         if bAt t p == w8Quest
            && p + 1 < len
            && (bAt t (p + 1) == w8Space
                || bAt t (p + 1) == w8Tab
                || bAt t (p + 1) == w8SOH)
           then parseFlowEntry' True (skipFlowWS (p + 1) t) t
         else
         if bAt t p == w8Colon && colonIsSeparator (p + 1) t
           then
             let p1 = skipFlowWS (p + 1) t
             in if p1 >= len
                  then Nothing
                  else case bAt t p1 of
                    44 -> Just (YMap (V.singleton (YNull, YNull)), p1)
                    93 -> Just (YMap (V.singleton (YNull, YNull)), p1)
                    _  -> case parseFlowValue p1 t of
                      Nothing -> Just (YMap (V.singleton (YNull, YNull)), p1)
                      Just (v, p2) ->
                        Just (YMap (V.singleton (YNull, v)), p2)
           else
             let !flowOpener = case bAt t p of
                   34  -> True   -- '"'
                   39  -> True   -- '\''
                   91  -> True   -- '['
                   123 -> True   -- '{'
                   _   -> False
             in case parseFlowValue p t of
                  Nothing -> Nothing
                  Just (k, p1) ->
                    let p2 = skipFlowWS p1 t
                    in if p2 < len
                         && bAt t p2 == w8Colon
                         && (flowOpener
                             || colonIsSeparator (p2 + 1) t)
                         then
                           -- For flow /sequences/, an implicit
                           -- key->value pair appearing inline
                           -- must have key and ':' on the same
                           -- line (spec §7.4.1).
                           let span_ = bSlice t p p2
                           in if not explicit
                                 && bAnyByte w8SOH span_
                                then Nothing
                                else case parseFlowValue (skipFlowWS (p2 + 1) t) t of
                                  Nothing -> Just (YMap (V.singleton (k, YNull)), p2 + 1)
                                  Just (v, p3) -> Just (YMap (V.singleton (k, v)), p3)
                         else Just (k, p1)

-- | Whether a colon at position @p@ acts as a key/value separator
-- in flow context: only when the very next character is a flow
-- stopper or whitespace.
colonIsSeparator :: Int -> Text -> Bool
colonIsSeparator !p t
  | p >= bLen t = True
  | otherwise   = flowColonFollower (bAt t p)
{-# INLINE colonIsSeparator #-}

parseFlowMap :: Int -> Text -> Maybe (Value, Int)
parseFlowMap !p0 t =
  let !len = bLen t

      goV !p acc
        | p >= len             = Nothing
        | bAt t p == w8RBrace  = Just (YMap (V.fromList (reverse acc)), p + 1)
        | bAt t p == w8Comma   = goV (skipFlowWS (p + 1) t) acc
        | otherwise =
            let p0'      = p
                (k, p1)  = case bAt t p of
                  58 -> (YNull, p)
                  _  -> case parseFlowValue p t of
                    Just (k', q) -> (k', q)
                    Nothing      -> (YNull, p)
            in if p1 == p0' && bAt t p1 /= w8Colon
                 then Nothing
                 else
                   let p2 = skipFlowWS p1 t
                       skipColon = if p2 < len && bAt t p2 == w8Colon
                                     then Just (skipFlowWS (p2 + 1) t)
                                     else Nothing
                   in case skipColon of
                        Just p2'
                          | p2' < len
                          , let c = bAt t p2'
                          , c == w8Comma || c == w8RBrace ->
                              finish p2' k YNull acc
                          | otherwise -> case parseFlowValue p2' t of
                              Nothing -> finish p2' k YNull acc
                              Just (v, p3) -> finish p3 k v acc
                        Nothing -> finish p2 k YNull acc

      finish !p k v acc =
        let p' = skipFlowWS p t
        in if p' >= len
             then Nothing
             else case bAt t p' of
                    44 -> goV (skipFlowWS (p' + 1) t) ((k, v) : acc)   -- ','
                    125 -> Just                                          -- '}'
                              ( YMap (V.fromList (reverse ((k, v) : acc)))
                              , p' + 1 )
                    _   -> Nothing
  in goV (skipFlowWS p0 t) []

parseDQ :: Int -> Text -> Maybe (Value, Int)
parseDQ !p0 t =
  let !len = bLen t
      -- Fast path: SWAR-scan for the first occurrence of '\"',
      -- '\\\\', or '\\1'. If the closing quote comes first
      -- (i.e. no escape or newline-sentinel before it) we slice
      -- the body out without further work.
      !startSlice = p0 + 1
      !rest       = bSlice t startSlice len
      !idx        = bFindAnyOf3 w8DQuote w8Backslash w8SOH rest
  in if idx < 0
       then Nothing                              -- no terminator at all
       else
         let !absIdx = startSlice + idx
             !c     = bAt t absIdx
         in if c == w8DQuote
              then Just ( YString (bSlice t startSlice absIdx)
                        , absIdx + 1 )
              else slow startSlice []
  where
    !len2 = bLen t
    slow !i acc
      | i >= len2 = Nothing
      | otherwise = case bAt t i of
          34 -> Just (YString (T.pack (reverse acc)), i + 1)
          92 | i + 1 < len2 -> case decodeDQEscape t (i + 1) of
                  Just (c, i') -> slow i' (c : acc)
                  Nothing      -> Nothing
             | otherwise -> Nothing
          1  -> slow (i + 1) (' ' : acc)
          b  | b < 0x80   -> slow (i + 1) (toEnum (fromEnum b) : acc)
             | otherwise  -> -- multi-byte UTF-8 char; fall back
                             -- to char-level read for correctness
                             case T.uncons (bDrop i t) of
                               Just (c, _) ->
                                 slow (i + utf8Width b) (c : acc)
                               Nothing -> Nothing

parseSQ :: Int -> Text -> Maybe (Value, Int)
parseSQ !p0 t =
  let !len        = bLen t
      !startSlice = p0 + 1
      !rest       = bSlice t startSlice len
      -- SWAR-scan for the first '\\'' or '\\1' sentinel. A bare
      -- '\\'' is a closing quote; a doubled '\\'\\'' is the
      -- single-quoted escape and forces the slow path.
      !idx        = bFindAnyOf2 w8SQuote w8SOH rest
  in if idx < 0
       then Nothing
       else
         let !absIdx = startSlice + idx
             !c     = bAt t absIdx
         in if c == w8SQuote
              then if absIdx + 1 < len && bAt t (absIdx + 1) == w8SQuote
                     then slow startSlice []     -- '\\'\\'' escape
                     else Just ( YString (bSlice t startSlice absIdx)
                               , absIdx + 1 )
              else slow startSlice []
  where
    !len2 = bLen t
    slow !i acc
      | i >= len2 = Nothing
      | otherwise = case bAt t i of
          39 | i + 1 < len2 && bAt t (i + 1) == w8SQuote ->
                  slow (i + 2) ('\'' : acc)
             | otherwise ->
                  Just (YString (T.pack (reverse acc)), i + 1)
          1  -> slow (i + 1) (' ' : acc)
          b  | b < 0x80   -> slow (i + 1) (toEnum (fromEnum b) : acc)
             | otherwise  -> case T.uncons (bDrop i t) of
                               Just (c, _) ->
                                 slow (i + utf8Width b) (c : acc)
                               Nothing -> Nothing

-- | Width in bytes of a UTF-8 character given its leading byte.
utf8Width :: Word8 -> Int
utf8Width b
  | b < 0x80  = 1
  | b < 0xC0  = 1   -- continuation; treat as 1 to make progress
  | b < 0xE0  = 2
  | b < 0xF0  = 3
  | otherwise = 4
{-# INLINE utf8Width #-}

parseFlowPlain :: Int -> Text -> Maybe (Value, Int)
parseFlowPlain !p t =
  let !len = bLen t
      -- Walk the body looking for a stopper. Track in 'sawSOH'
      -- whether we crossed any '\\1' newline sentinel, so we
      -- can skip the (allocating) 'T.replace' fold below in
      -- the common case.
      go !i !sawSOH
        | i >= len  = (i, sawSOH)
        | otherwise =
            let !c = bAt t i
            in if isFlowStopByte c
                  || (c == w8Colon && colonStopByte i)
                 then (i, sawSOH)
                 else go (i + 1) (sawSOH || c == w8SOH)
      colonStopByte !i =
        let i1 = i + 1
        in i1 >= len || flowColonFollower (bAt t i1)
      (!p', !sawSOH) = go p False
      raw      = bSlice t p p'
      folded
        | sawSOH    = T.replace tSOH tSpace raw
        | otherwise = raw
      stripped = T.stripEnd folded
  in if T.null stripped
       then Nothing
       else if stripped == tDashStr
              then Nothing
              else Just (resolvePlain stripped, p')

-- ---------------------------------------------------------------------------
-- Constant-string CAFs.
--
-- 'T.pack \"…\"' is /not/ free even for a static literal — it
-- allocates the result 'Text' on every call unless GHC happens
-- to lift it. Hoisting frequently-used literals to top-level
-- saves a re-pack on every hot-loop iteration.
-- ---------------------------------------------------------------------------

tSOH, tSTX, tSpace, tNL :: Text
tSOH    = T.singleton '\1'
tSTX    = T.singleton '\2'
tSpace  = T.singleton ' '
tNL     = T.singleton '\n'

tDashStr, tDashSpace, tDashTab :: Text
tDashStr   = T.pack "-"
tDashSpace = T.pack "- "
tDashTab   = T.pack "-\t"

tQuestStr, tQuestSpace, tQuestTab :: Text
tQuestStr   = T.pack "?"
tQuestSpace = T.pack "? "
tQuestTab   = T.pack "?\t"

tColonStr, tColonSpace, tColonTab :: Text
tColonStr   = T.pack ":"
tColonSpace = T.pack ": "
tColonTab   = T.pack ":\t"

tAliasSentinel :: Text
tAliasSentinel = T.pack "\0alias\0"

tYamlTagPrefix :: Text
tYamlTagPrefix = T.pack "tag:yaml.org,2002:"

-- | Bytes that terminate a flow-context plain scalar
-- (excluding the colon-with-follower case which is handled by
-- the caller).
isFlowStopByte :: Word8 -> Bool
isFlowStopByte c =
  c == w8Comma || c == w8LBrack || c == w8RBrack
    || c == w8LBrace || c == w8RBrace || c == w8STX
{-# INLINE isFlowStopByte #-}

-- | A byte that, when it follows a ':' in flow context, makes
-- that ':' a key/value separator (and therefore a stop point
-- for a flow plain scalar).
flowColonFollower :: Word8 -> Bool
flowColonFollower c =
  c == w8Space || c == w8Tab || c == w8SOH
    || c == w8Comma || c == w8RBrack || c == w8RBrace
{-# INLINE flowColonFollower #-}

skipFlowWS :: Int -> Text -> Int
skipFlowWS !p t =
  let !len = bLen t
      go !i
        | i >= len  = i
        | otherwise = case bAt t i of
            32 -> go (i + 1)   -- ' '
            9  -> go (i + 1)   -- '\t'
            1  -> go (i + 1)   -- '\1'
            _  -> i
  in go p
{-# INLINE skipFlowWS #-}

-- ---------------------------------------------------------------------------
-- Block style: dispatch from a line we haven't consumed yet.
-- ---------------------------------------------------------------------------

-- | A line starts a block-sequence entry if its body is exactly
-- @-@ or starts with @- @ / @-<TAB>@.
isSeqItem :: Text -> Bool
isSeqItem b =
  case bLen b of
    0 -> False
    1 -> bAt b 0 == 45                  -- '-'
    _ -> bAt b 0 == 45
         && let !c1 = bAt b 1
            in c1 == w8Space || c1 == w8Tab
{-# INLINE isSeqItem #-}

-- | Same shape for the explicit-key marker @?@.
isExplicitKey :: Text -> Bool
isExplicitKey b =
  case bLen b of
    0 -> False
    1 -> bAt b 0 == w8Quest
    _ -> bAt b 0 == w8Quest
         && let !c1 = bAt b 1
            in c1 == w8Space || c1 == w8Tab
{-# INLINE isExplicitKey #-}

-- | True when @b@ is exactly @\":\"@ or starts with @\": \"@ /
-- @\":\\t\"@. Used to recognise the value-continuation marker
-- after an explicit @?@ key.
isColonMarker :: Text -> Bool
isColonMarker b =
  case bLen b of
    0 -> False
    1 -> bAt b 0 == w8Colon
    _ -> bAt b 0 == w8Colon
         && let !c1 = bAt b 1
            in c1 == w8Space || c1 == w8Tab
{-# INLINE isColonMarker #-}

parseBlockOrPlain :: PLine -> P Value
parseBlockOrPlain l
  | isSeqItem body     = parseBlockSeq (lineIndent l)
  | isExplicitKey body = parseExplicitMap (lineIndent l)
  | otherwise = case findAliasKeySplit body of
      Just (aliasName, vRest) -> parseBlockMapAliasFirst (lineIndent l)
                                   aliasName vRest
      Nothing -> case findKeyValueSplit body of
        Just (k, vRest) -> parseBlockMap (lineIndent l) k vRest
        Nothing         -> parsePlainScalar (lineIndent l) body
  where
    body = lineBody l

-- | Block mapping whose first key is an alias node (parsed via
-- 'findAliasKeySplit'). Same shape as 'parseBlockMap' otherwise.
parseBlockMapAliasFirst :: Int -> Text -> Text -> P Value
parseBlockMapAliasFirst !ind aliasName firstRest = do
  dropLine
  k0 <- resolveAnchor aliasName
  v0 <- parseImplicitMapValue ind firstRest
  rest <- collect [(k0, v0)]
  pure (YMap (V.fromList (reverse rest)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | isSeqItem (lineBody l) -> pure acc
          | isExplicitKey (lineBody l) -> pure acc
          | startsWithTab (lineBody l) ->
              failP $ "tab character used as indentation (line "
                      ++ show (lineNo l) ++ ")"
          | otherwise -> case findAliasKeySplit (lineBody l) of
              Just (a, vRest) -> do
                dropLine
                k <- resolveAnchor a
                v <- parseImplicitMapValue ind vRest
                collect ((k, v) : acc)
              Nothing -> case findKeyValueSplit (lineBody l) of
                Just (k, vRest) -> do
                  dropLine
                  v <- parseImplicitMapValue ind vRest
                  collect ((YString k, v) : acc)
                Nothing -> pure acc


-- ---------------------------------------------------------------------------
-- Block sequence
-- ---------------------------------------------------------------------------

parseBlockSeq :: Int -> P Value
parseBlockSeq !ind = collect [] >>= \xs -> pure (YSeq (V.fromList (reverse xs)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | not (isSeqItem (lineBody l))
                                -> pure acc
          | otherwise -> do
              v <- parseSeqItem ind
              collect (v : acc)

parseSeqItem :: Int -> P Value
parseSeqItem !ind = do
  Just l <- popLine
  let body = lineBody l
      after | body == "-" = T.empty
            | otherwise   = T.drop 2 body
      after' = T.stripStart after
  -- '-<TAB><INDICATOR>' or '- <TAB><INDICATOR>' (nested block
  -- marker reached via a tab in the indent column) is 'tab as
  -- indentation' per spec §6.1. The plain-scalar form
  -- '-<TAB>x' is fine because no further indent calculation
  -- happens; but '-\\t-' / '- \\t-' / '-\\t?' / etc. would set
  -- the nested block's indent to a tab-containing column.
  let separatorHasTab = T.any (== '\t')
                          (T.takeWhile (\c -> c == ' ' || c == '\t')
                             (T.drop 1 body))
  when (separatorHasTab && startsWithBlockIndicator after') $
    failP $ "tab character used as indentation before nested block marker (line "
            ++ show (lineNo l) ++ ")"
  let isCommentOnly = case T.uncons after' of
        Just ('#', _) -> True
        _             -> False
  if isCommentOnly || T.null after'
    then do
      mNext <- peekLine
      case mNext of
        Just l2 | lineIndent l2 > ind ->
          withParentInd ind (parseNode (lineIndent l2))
        _ -> pure YNull
    else do
      let isCarrier = case T.uncons after' of
            Just ('|', _) -> True
            Just ('>', _) -> True
            Just ('!', _) -> True
            Just ('&', _) -> True
            _             -> False
          isNestedBlock = case T.uncons after' of
            Just ('-', rest) -> startsWithSeparator rest
            Just ('?', rest) -> startsWithSeparator rest
            _                -> False
          extraWS = T.length after - T.length after'
          virtInd | isCarrier      = ind
                  -- For nested block constructs the actual
                  -- column of the next dash / explicit-key marker
                  -- is past the outer dash AND any extra
                  -- separator spaces. Use it so collectors at
                  -- that level match real-world inputs (A2M4).
                  | isNestedBlock  = ind + 2 + extraWS
                  | otherwise      = ind + 2
          virt = PLine (lineNo l) virtInd LContent after' after'
      pushLine virt
      withParentInd ind (parseNode virtInd)

-- ---------------------------------------------------------------------------
-- Block mapping
-- ---------------------------------------------------------------------------

parseBlockMap :: Int -> Text -> Text -> P Value
parseBlockMap !ind firstKey firstRest = do
  dropLine
  v0 <- parseImplicitMapValue ind firstRest
  rest <- collect [(YString firstKey, v0)]
  pure (YMap (V.fromList (reverse rest)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | otherwise ->
              let body = lineBody l in case T.uncons body of
                Nothing       -> pure acc
                Just (h, _)
                  | h == '\t' ->
                      failP $ "tab character used as indentation (line "
                              ++ show (lineNo l) ++ ")"
                  | h == '-'
                  , isSeqItem body -> pure acc
                  | h == '?'
                  , isExplicitKey body -> do
                      k <- readExplicitPart "?"
                      v <- readExplicitValue
                      collect ((k, v) : acc)
                  | h == '*'
                  , Just (aliasName, vRest) <- findAliasKeySplit body -> do
                      dropLine
                      k <- resolveAnchor aliasName
                      v <- parseImplicitMapValue ind vRest
                      collect ((k, v) : acc)
                  | otherwise -> case findKeyValueSplit body of
                      Just (k, vRest) -> do
                        dropLine
                        let (anchors, k') = stripKeyProperties k
                        case T.uncons k' of
                          Just ('*', _) | not (null anchors) ->
                            failP $ "anchor immediately followed by alias key (line "
                                    ++ show (lineNo l) ++ ")"
                          _ -> pure ()
                        v <- parseImplicitMapValue ind vRest
                        let kv = YString k'
                        mapM_ (\an -> recordAnchor an kv) anchors
                        collect ((kv, v) : acc)
                      Nothing -> pure acc

    readExplicitPart marker = do
      Just l <- popLine
      let body = lineBody l
          afterMarker = if body == marker then T.empty
                                          else T.drop 1 body
          rest0 = T.stripStart (T.drop 1 afterMarker)
          rest = case T.uncons rest0 of
            Just ('#', _) -> T.empty
            _             -> rest0
      case T.uncons afterMarker of
        Just ('\t', _) ->
          failP $ "tab character after explicit-key marker (line "
                  ++ show (lineNo l) ++ ")"
        _ -> pure ()
      if T.null rest
        then do
          mNext <- peekLine
          case mNext of
            Just l2 | lineIndent l2 > lineIndent l ->
              parseNode (lineIndent l2)
            Just l2
              | lineIndent l2 == lineIndent l
              , isSeqItem (lineBody l2) ->
                  parseBlockSeq (lineIndent l2)
            _ -> pure YNull
        else do
          let isBlockScalarHead = case T.uncons rest of
                Just ('|', _) -> True
                Just ('>', _) -> True
                _             -> False
              virtInd | isBlockScalarHead = lineIndent l
                      | otherwise         = lineIndent l + 2
          pushLine (PLine (lineNo l) virtInd
                          LContent rest rest)
          parseNode virtInd

    readExplicitValue = do
      mPL <- peekLine
      case mPL of
        Just l | lineIndent l == ind
                 && isColonMarker (lineBody l) ->
            readExplicitPart ":"
        _ -> pure YNull

startsWithTab :: Text -> Bool
startsWithTab t = case T.uncons t of
  Just ('\t', _) -> True
  _              -> False

-- | True when @t@ is empty or starts with a space / tab.
startsWithSeparator :: Text -> Bool
startsWithSeparator t = case T.uncons t of
  Nothing        -> True
  Just (' ', _)  -> True
  Just ('\t', _) -> True
  _              -> False

-- | True when @t@ begins with a block-context structural marker
-- ('-' or '?' followed by space / tab / EOL).
startsWithBlockIndicator :: Text -> Bool
startsWithBlockIndicator t = case T.uncons t of
  Just ('-', rest) -> isBlockSep rest
  Just ('?', rest) -> isBlockSep rest
  _                -> False
  where
    isBlockSep r = case T.uncons r of
      Nothing         -> True
      Just (' ', _)   -> True
      Just ('\t', _)  -> True
      _               -> False

-- | Strip leading anchor / tag tokens (separated by spaces) from
-- a block-mapping key string. Returns the list of anchor names
-- encountered and the remainder text. Tags are dropped silently
-- (they don't change the key projection).
stripKeyProperties :: Text -> ([Text], Text)
stripKeyProperties = go []
  where
    go acc t = case T.uncons (T.stripStart t) of
      Just ('&', rest) ->
        let (name, after) = takeAnchorName rest
        in go (name : acc) after
      Just ('!', rest) ->
        let (_tg, after) = T.span (\c -> not (c == ' ' || c == '\t')) rest
        in go acc after
      _ -> (reverse acc, T.stripStart t)

-- | Recognise @*alias : value@ style mapping entries where the key
-- is an alias node. Returns the alias name (without the @*@) and
-- the value text after the colon, or 'Nothing' when the line
-- doesn't have this shape.
findAliasKeySplit :: Text -> Maybe (Text, Text)
findAliasKeySplit t = case T.uncons t of
  Just ('*', rest) ->
    let (name, after) = takeAnchorName rest
        afterTrim = T.stripStart after
    in case T.uncons afterTrim of
         Just (':', tail_)
           | T.null tail_ || T.head tail_ == ' ' || T.head tail_ == '\t'
               -> Just (name, T.drop 1 afterTrim)
         _   -> Nothing
  _ -> Nothing

parseImplicitMapValue :: Int -> Text -> P Value
parseImplicitMapValue !ind vRest =
  if T.null after
       then do
         mNext <- peekLine
         case mNext of
           Just l2
             | lineIndent l2 > ind -> do
                 -- Strip leading tabs from the body, which YAML
                 -- treats as additional whitespace (not part of
                 -- the scalar text).
                 let body' = T.dropWhile (== '\t') (lineBody l2)
                 modifyS (\s ->
                   s { psLines = case psLines s of
                         (h : rs) -> h { lineBody = body' } : rs
                         []       -> [] })
                 parseNode (lineIndent l2)
             | lineIndent l2 == ind
                 && (isSeqItem (lineBody l2))
                 -> parseBlockSeq ind
           _ -> pure YNull
       else case T.uncons after of
         Just (h, _)
           | h == '|' -> do
               pushLine (PLine 0 ind LContent after after)
               parseBlockScalar Literal
           | h == '>' -> do
               pushLine (PLine 0 ind LContent after after)
               parseBlockScalar Folded
           | h == '[' -> do
               pushLine (PLine 0 (ind + 2) LContent after after)
               Just l <- popLine
               consumeFlowAt (ind + 1) (lineBody l)
           | h == '{' -> do
               pushLine (PLine 0 (ind + 2) LContent after after)
               Just l <- popLine
               consumeFlowAt (ind + 1) (lineBody l)
           | h == '&' -> do
               pushLine (PLine 0 ind LContent after after)
               withInMapValue True parseAnchored
           | h == '*' -> do
               pushLine (PLine 0 (ind + 2) LContent after after)
               parseAlias
           | h == '!' -> do
               pushLine (PLine 0 ind LContent after after)
               parseTagged
           | h == '"'  -> consumeQuotedAt '"'  (ind + 1) after
           | h == '\'' -> consumeQuotedAt '\'' (ind + 1) after
           | h == '#'  -> parseImplicitMapValueEmpty ind
           | otherwise -> goPlain
         Nothing -> goPlain
       where
         goPlain :: P Value
         goPlain = do
           -- Fast path: if the next physical line in the stream
           -- can't extend this plain scalar (lower indent than
           -- the value column, OR no next line at all), we can
           -- resolve directly without 'parsePlainScalar'.
           ls <- getLines
           let canExtend = case ls of
                 (n : _) ->
                   lineKind n == LContent
                   && lineIndent n > ind
                 _ -> False
               sclean = T.stripEnd (stripInlineComment after)
               isUnambiguous = not canExtend
                 && case findKeyValueSplit sclean of
                      Just _  -> False
                      Nothing -> True
           if isUnambiguous
             then pure (resolvePlain sclean)
             else do
               pushLine (PLine 0 (ind + 1) LContent after after)
               withInMapValue True (parsePlainScalar (ind + 1) after)
         after = T.stripStart vRest

parseImplicitMapValueEmpty :: Int -> P Value
parseImplicitMapValueEmpty !ind = do
  mNext <- peekLine
  case mNext of
    Just l2
      | lineIndent l2 > ind -> do
          let body' = T.dropWhile (== '\t') (lineBody l2)
          modifyS (\s ->
            s { psLines = case psLines s of
                  (h : rs) -> h { lineBody = body' } : rs
                  []       -> [] })
          parseNode (lineIndent l2)
      | lineIndent l2 == ind && isSeqItem (lineBody l2) ->
          parseBlockSeq ind
    _ -> pure YNull

-- ---------------------------------------------------------------------------
-- Explicit-key mapping (?-form)
-- ---------------------------------------------------------------------------

parseExplicitMap :: Int -> P Value
parseExplicitMap !ind = collect [] >>= \kvs -> pure (YMap (V.fromList (reverse kvs)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | isExplicitKey (lineBody l) -> do
              k <- readExplicitPart "?"
              v <- readExplicitValue
              collect ((k, v) : acc)
          -- A bare ':' or ': value' / ':<TAB>value' line at the
          -- mapping indent is an entry with an implicit (null)
          -- key. Per spec §8.18, omitting the '?' is permitted.
          | isColonMarker (lineBody l) -> do
              v <- readExplicitPart ":"
              collect ((YNull, v) : acc)
          -- An ordinary 'key: value' implicit pair after a ?-form
          -- entry continues the same mapping (spec §8.18).
          | otherwise -> case findKeyValueSplit (lineBody l) of
              Just (k, vRest) -> do
                dropLine
                let (anchors, k') = stripKeyProperties k
                v <- parseImplicitMapValue ind vRest
                let kv = YString k'
                mapM_ (\an -> recordAnchor an kv) anchors
                collect ((kv, v) : acc)
              Nothing -> pure acc

    readExplicitPart marker = do
      Just l <- popLine
      let body = lineBody l
          afterMarker = if body == marker then T.empty
                                          else T.drop 1 body
          rest0 = T.stripStart (T.drop 1 afterMarker)
          -- '#' immediately after the explicit-key marker
          -- starts a comment; the value comes from the next
          -- continuation line.
          rest = case T.uncons rest0 of
            Just ('#', _) -> T.empty
            _             -> rest0
      case T.uncons afterMarker of
        Just ('\t', _) ->
          failP $ "tab character after explicit-key marker (line "
                  ++ show (lineNo l) ++ ")"
        _ -> pure ()
      if T.null rest
        then do
          mNext <- peekLine
          case mNext of
            Just l2 | lineIndent l2 > lineIndent l -> parseNode (lineIndent l2)
            Just l2
              | lineIndent l2 == lineIndent l
              , isSeqItem (lineBody l2) ->
                  parseBlockSeq (lineIndent l2)
            _ -> pure YNull
        else do
          let isBlockScalarHead = case T.uncons rest of
                Just ('|', _) -> True
                Just ('>', _) -> True
                _             -> False
              virtInd | isBlockScalarHead = lineIndent l
                      | otherwise         = lineIndent l + 2
          pushLine (PLine (lineNo l) virtInd LContent rest rest)
          parseNode virtInd

    readExplicitValue = do
      mPL <- peekLine
      case mPL of
        Just l | lineIndent l == ind
                 && isColonMarker (lineBody l) ->
            readExplicitPart ":"
        _ -> pure YNull

-- ---------------------------------------------------------------------------
-- Plain scalars (multi-line)
-- ---------------------------------------------------------------------------

parsePlainScalar :: Int -> Text -> P Value
parsePlainScalar !ind firstBody = do
  parentInd <- getParentInd
  inMapValue <- getInMapValue
  parsePlainScalarAt parentInd inMapValue ind firstBody

-- | Like 'parsePlainScalar' but with explicit parent indent and
-- "are we in a mapping-value position" flag.
parsePlainScalarAt :: Int -> Bool -> Int -> Text -> P Value
parsePlainScalarAt !parentInd !inMapValue !baseIndArg firstBody = do
  let !ind = baseIndArg
      !_p  = parentInd
      !_m  = inMapValue
  dropLine
  -- Fast path: a body with no '#' at all has no comment, so we
  -- skip the comparison probe entirely.
  let !hasHash    = bAnyByte w8Hash firstBody
      !stripped   = if hasHash then stripInlineComment firstBody
                               else firstBody
      !first      = T.stripEnd stripped
      !hadComment = hasHash
                  && bLen stripped < bLen (T.stripEnd firstBody)
  -- A plain scalar may not contain ': ' (colon-space) in block
  -- context — that would form a nested mapping (spec §7.3.3).
  case findKeyValueSplit first of
    Just _ -> failP $ "nested mapping in plain scalar: " ++ show first
    _      -> pure ()
  -- A trailing comment on the first line of a plain scalar
  -- followed by a continuation line is malformed (the comment
  -- would silently break the scalar).
  when hadComment $ do
    ls' <- getLines
    case ls' of
      (l2 : _)
        | lineIndent l2 >= ind
        , lineKind l2 == LContent
        , not (isSeqItem (lineBody l2))
        , not (isExplicitKey (lineBody l2))
        , case findKeyValueSplit (lineBody l2) of
            Just _  -> False
            Nothing -> True ->
           failP $ "comment between plain-scalar lines (line "
                   ++ show (lineNo l2 - 1) ++ ")"
      _ -> pure ()
  rest <- collectFolds ind 0 []
  let !final = joinPlain (first : rest)
  pure (resolvePlain final)
  where
    -- @blanks@ counts the run of consecutive blank lines we've
    -- absorbed since the last non-blank continuation line. The
    -- collected list interleaves non-blank line bodies with marker
    -- entries representing blank-line runs (encoded as the empty
    -- string preceded by a special sentinel, see joinPlain).
    --
    -- A continuation line is accepted when its indent is /strictly
    -- greater/ than the scalar's base indent and it doesn't look
    -- like a new collection entry (mapping key, seq item, explicit
    -- '?' key).
    collectFolds baseInd blanks acc = do
      ls <- getLines
      case ls of
        []     -> pure (reverse acc)
        (l:_)
          | lineKind l == LBlank ->
              do consumeOne; collectFolds baseInd (blanks + 1) acc
          | lineKind l == LComment
            && lineIndent l > baseInd ->
              do consumeOne; collectFolds baseInd blanks acc
          | (lineKind l == LContent || lineKind l == LDirective)
            -- Standard continuation: indent >= baseInd.
            -- Shallow continuation rules:
            --   * indent in (parentInd .. baseInd) range is OK
            --     for plain scalar content (UV7Q).
            --   * a '- ' shallow line (looks like nested seq) at
            --     the OUTERMOST seq (parentInd == 0, NOT inside
            --     a mapping value) also folds into the plain
            --     scalar (AB8U).
            && (lineIndent l >= baseInd
                || (lineIndent l > parentInd
                    && not (isExplicitKey (lineBody l))
                    && (not (isSeqItem (lineBody l))
                        || (parentInd == 0 && not inMapValue))))
            && not (isSeqItem (lineBody l)
                    && lineIndent l >= baseInd)
            && not (isExplicitKey (lineBody l)
                    && lineIndent l >= baseInd)
            && case findKeyValueSplit (lineBody l) of
                 Just _  -> False
                 Nothing -> True
            -> do
              -- For an LDirective line (begins with '%'), only
              -- accept it as scalar content if there's no later
              -- '---' / '...' marker that would make it a real
              -- directive for a subsequent document.
              accept <- case lineKind l of
                LDirective -> do
                  ls' <- getLines
                  pure (not (any isMarker ls'))
                _ -> pure True
              if not accept then pure (reverse acc) else do
                consumeOne
                let raw = lineBody l
                    s0 = T.stripEnd (stripInlineComment raw)
                    s = T.dropWhile (\c -> c == ' ' || c == '\t') s0
                    hadComment = bLen s0 < bLen (T.stripEnd raw)
                    prefix
                      | blanks == 0 = s
                      | otherwise   = T.replicate blanks tNL <> s
                when hadComment $ do
                  ls' <- getLines
                  case ls' of
                    (l2 : _)
                      | lineIndent l2 >= baseInd
                      , lineKind l2 == LContent
                      , not (isSeqItem (lineBody l2))
                      , not (isExplicitKey (lineBody l2))
                      , case findKeyValueSplit (lineBody l2) of
                          Just _  -> False
                          Nothing -> True ->
                         failP $ "comment between plain-scalar lines (line "
                                 ++ show (lineNo l) ++ ")"
                    _ -> pure ()
                collectFolds baseInd 0 (prefix : acc)
          | otherwise -> pure (reverse acc)
      where
        isMarker l = lineKind l == LDocStart || lineKind l == LDocEnd

    consumeOne = do
      ls <- getLines
      case ls of
        (_:xs) -> setLines xs
        []     -> pure ()

    -- Join the collected pieces; pieces that already start with a
    -- newline marker are joined with no separator.
    -- Most plain scalars are a single line; short-circuit
    -- those without building any intermediate list. For the
    -- multi-line case build the result in one 'T.concat'
    -- allocation rather than chaining '<>' (which is O(N^2) on
    -- the result text).
    joinPlain []     = T.empty
    joinPlain [x]    = x
    joinPlain xs     = T.concat (interleave xs)
      where
        interleave []         = []
        interleave [x]        = [x]
        interleave (x:y:zs)
          | T.isPrefixOf tNL y = x : interleave (y:zs)
          | otherwise          = x : tSpace : interleave (y:zs)

-- ---------------------------------------------------------------------------
-- Block scalars
-- ---------------------------------------------------------------------------

data Chomp = Strip | Clip | Keep deriving (Eq, Show)
data BlockKind = Literal | Folded deriving (Eq, Show)

parseBlockScalar :: BlockKind -> P Value
parseBlockScalar k = do
  Just l <- popLine
  let header = T.drop 1 (lineBody l)   -- drop '|' or '>'
  (chomp, hint) <- case parseHeader header of
    Right h  -> pure h
    Left err -> failP $ "invalid block scalar header: " ++ err
                       ++ " (line " ++ show (lineNo l) ++ ")"
  let explicitBase = (lineIndent l +) <$> hint
  body <- collectScalarLines (lineIndent l) explicitBase
  -- Per spec §8.1.1: a /leading/ blank line whose indent is
  -- greater than the first content line's indent is invalid
  -- (the missing indent indicator can't be recovered from
  -- blank lines alone).
  case explicitBase of
    Nothing ->
      let leadingBlanks = takeWhile (\(_, b) -> T.null b) body
          afterBlanks   = dropWhile (\(_, b) -> T.null b) body
      in case afterBlanks of
           ((firstC, _) : _)
             | firstC >= 0
             , any (\(i, _) -> i > firstC) leadingBlanks ->
                 failP $ "block scalar baseline below earlier blank-line indent (line "
                         ++ show (lineNo l) ++ ")"
           _ -> pure ()
    _ -> pure ()
  let bodyAdj = case nonEmptyContent body of
        True  -> body
        False -> map (\(i, b) -> if i < 0 then (i, b) else (-1, b)) body
      txt = case k of
        Literal -> joinLiteralAt explicitBase chomp bodyAdj
        Folded  -> joinFoldedAt  explicitBase chomp bodyAdj
  pure (YString txt)
  where
    nonEmptyContent = any (\(i, b) -> i >= 0 && not (T.null b))

    parseHeader :: Text -> Either String (Chomp, Maybe Int)
    parseHeader h0 =
      let -- Anything on the header line after a single '#' (with
          -- preceding whitespace) is a comment.
          h = stripInlineComment h0
          rest = T.unpack (T.strip h)
          chompOf '-' = Strip
          chompOf '+' = Keep
          chompOf _   = Clip
          mkHint c
            | c >= '1' && c <= '9' = Right (Just (digitToInt c))
            -- '|0' is invalid (indent indicator must be 1..9).
            | c == '0'             = Left "indent indicator must be 1..9"
            | otherwise            = Left ("unexpected character " ++ [c])
      in case rest of
           []                          -> Right (Clip, Nothing)
           [c] | c == '-' || c == '+'  -> Right (chompOf c, Nothing)
               | isDigit c             -> (\h_ -> (Clip, h_)) <$> mkHint c
               | otherwise             -> Left ("unexpected character " ++ [c])
           [a, b]
             | (a == '-' || a == '+') && isDigit b ->
                 (\h_ -> (chompOf a, h_)) <$> mkHint b
             | isDigit a && (b == '-' || b == '+') ->
                 (\h_ -> (chompOf b, h_)) <$> mkHint a
             | otherwise -> Left ("unexpected header " ++ rest)
           _ -> Left ("unexpected header " ++ rest)

-- | Collect the body lines of a block scalar.
--
-- The semantics: blank / more-indented blank lines belong to the
-- scalar regardless of their column. Once we've seen a content
-- line, that line's indent /is/ the "base indent" of the scalar;
-- the scalar terminates on the first subsequent line whose indent
-- falls /at or below/ that base. Lines whose source classified as
-- @LComment@ but sit at indent > base are treated as scalar
-- content (the '#' is data); comments at base or shallower
-- terminate.
collectScalarLines :: Int -> Maybe Int -> P [(Int, Text)]
collectScalarLines !parent !mExplicit = collect mExplicit []
  where
    -- @mBase@ is the established base indent (after the first
    -- content line, or pre-seeded by an explicit indent
    -- indicator). @acc@ is the reverse-accumulated body.
    collect mBase acc = do
      ls <- getLines
      case ls of
        []     -> pure (reverse acc)
        (l:_)
          | lineKind l == LBlank ->
              do let ind = lineIndent l
                     raw = lineRawBody l
                     hasTabs = not (T.null raw)
                 -- A 'blank' line whose only content is a TAB
                 -- and that sits at or below 'parent' before any
                 -- content line establishes a baseline is using
                 -- a tab as block-scalar indentation: invalid
                 -- per spec §6.1 (Y79Y/000).
                 case mBase of
                   Nothing
                     | hasTabs
                     , ind <= parent
                     , parent >= 0 ->
                       failP $ "tab character used as block-scalar indentation (line "
                               ++ show (lineNo l) ++ ")"
                   _ -> pure ()
                 _ <- consumeOne
                 let isMoreIndented = case mBase of
                       Just b  -> ind > b
                       Nothing -> ind > parent
                 if isMoreIndented
                   then collect mBase ((ind, raw) : acc)
                   else if hasTabs
                          then case mBase of
                                 Just b | ind >= b ->
                                    collect mBase ((ind, raw) : acc)
                                 _ -> collect mBase ((-1, T.empty) : acc)
                          else collect mBase ((-1, T.empty) : acc)
          | lineKind l == LDocStart || lineKind l == LDocEnd
              -> pure (reverse acc)
          | otherwise ->
              let ind = lineIndent l
                  inside = case mBase of
                    Just b  -> ind >= b
                    Nothing -> ind > parent
              in if not inside
                   then pure (reverse acc)
                   else if lineKind l == LComment
                          then case mBase of
                            -- Once a base indent is set, any
                            -- comment at deeper indent is content;
                            -- a comment at /base/ indent is
                            -- content too in the special "compact
                            -- top-level" mode (parent = -1) where
                            -- everything goes into the scalar.
                            Just b | ind > b -> do
                              _ <- consumeOne
                              collect mBase ((ind, lineBody l) : acc)
                            Just b | ind == b && parent < 0 -> do
                              _ <- consumeOne
                              collect mBase ((ind, lineBody l) : acc)
                            Just _ -> pure (reverse acc)
                            Nothing -> do
                              _ <- consumeOne
                              collect (Just ind)
                                ((ind, lineRawBody l) : acc)
                          else do
                            _ <- consumeOne
                            let mBase' = case mBase of
                                  Just _  -> mBase
                                  Nothing -> Just ind
                            collect mBase'
                              ((ind, lineRawBody l) : acc)

    consumeOne = do
      ls <- getLines
      case ls of
        (_:xs) -> setLines xs >> pure ()
        []     -> pure ()

joinLiteral :: Chomp -> [(Int, Text)] -> Text
joinLiteral = joinLiteralAt Nothing

joinLiteralAt :: Maybe Int -> Chomp -> [(Int, Text)] -> Text
joinLiteralAt mExpl chomp xs =
  let baseInd = case mExpl of
                  Just b  -> b
                  Nothing -> minNonNegative xs
      lns     = map (renderLine baseInd) xs
      raw | null xs   = T.empty
          | otherwise = T.intercalate tNL lns <> tNL
  in chompText chomp raw
  where
    renderLine bi (i, b)
      | i < 0     = T.empty
      | otherwise = T.replicate (max 0 (i - bi)) tSpace <> b

joinFolded :: Chomp -> [(Int, Text)] -> Text
joinFolded = joinFoldedAt Nothing

joinFoldedAt :: Maybe Int -> Chomp -> [(Int, Text)] -> Text
joinFoldedAt mExpl chomp xs =
  let baseInd = case mExpl of
                  Just b  -> b
                  Nothing -> minNonNegative xs
      raw | null xs   = T.empty
          | otherwise = T.concat (foldFirst xs baseInd) <> tNL
  in chompText chomp raw
  where
    isBlank (i, b) = i < 0 || T.null b

    -- A line is "more-indented" if its source column is past the
    -- base indent OR its body starts with whitespace (a leading
    -- tab counts).
    isMoreIndented bi (i, b) =
      i > bi
      || case T.uncons b of
           Just (' ',  _) -> True
           Just ('\t', _) -> True
           _              -> False

    foldFirst [] _ = []
    foldFirst ((i, b) : rest) bi
      | isBlank (i, b) = tNL : foldAfterFirstBlank rest bi
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) tSpace <> b
              more = isMoreIndented bi (i, b)
          in txt : foldNext rest bi more

    -- Same as 'foldAfterBlank' but used when no content line has
    -- yet been seen — there's no "previous more-indented marker"
    -- to subtract, so a more-indented first content line gets
    -- /no/ extra preserved newline.
    foldAfterFirstBlank [] _ = []
    foldAfterFirstBlank ((i, b) : rest) bi
      | isBlank (i, b) = tNL : foldAfterFirstBlank rest bi
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) tSpace <> b
              nowMore = isMoreIndented bi (i, b)
          in txt : foldNext rest bi nowMore

    foldNext [] _ _ = []
    foldNext ((i, b) : rest) bi prevMore
      | isBlank (i, b) =
          -- A break right after a more-indented line is preserved
          -- as a literal newline; the upcoming blank emits another
          -- on top of that.
          let pre = if prevMore then [tNL] else []
          in pre ++ tNL : foldAfterBlank rest bi prevMore
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) tSpace <> b
              nowMore = isMoreIndented bi (i, b)
              joinSep
                | prevMore || nowMore = tNL
                | otherwise           = tSpace
          in joinSep : txt : foldNext rest bi nowMore

    -- @prevMore@ here refers to whether the line that opened the
    -- blank run was more-indented; when leaving a blank run we
    -- need to emit one more break if either side is more-indented.
    foldAfterBlank [] _ _ = []
    foldAfterBlank ((i, b) : rest) bi prevMore
      | isBlank (i, b) = tNL : foldAfterBlank rest bi prevMore
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) tSpace <> b
              nowMore = isMoreIndented bi (i, b)
              -- If we're leaving a blank-run into a more-indented
              -- line and the previous content line was /not/
              -- itself more-indented, the spec requires an extra
              -- preserved break.
              extra = if nowMore && not prevMore
                        then [tNL]
                        else []
          in extra ++ txt : foldNext rest bi nowMore

-- | Smallest indent from a non-blank line in the collected list,
-- or 0 if there are no non-blank lines. Blank lines (including
-- "more-indented blanks" we keep around for spacing) do not
-- contribute, since the spec defines the body indent as the indent
-- of the first non-empty line.
minNonNegative :: [(Int, Text)] -> Int
minNonNegative = go Nothing
  where
    go acc [] = case acc of
      Nothing -> 0
      Just !n -> n
    go acc ((i, b) : rest)
      | i < 0 || T.null b = go acc rest
      | otherwise = case acc of
          Nothing             -> go (Just i) rest
          Just !n | i < n     -> go (Just i) rest
                  | otherwise -> go (Just n) rest

chompText :: Chomp -> Text -> Text
chompText Strip = T.dropWhileEnd (== '\n')
chompText Keep  = id
chompText Clip  = \t ->
  let stripped = T.dropWhileEnd (== '\n') t
  in if T.null stripped
       then T.empty               -- "no content" → no trailing newline
       else stripped <> tNL

-- ---------------------------------------------------------------------------
-- Plain-scalar resolution per the YAML 1.2 core schema
-- ---------------------------------------------------------------------------

resolvePlain :: Text -> Value
resolvePlain raw = case T.uncons raw of
  Nothing -> YString T.empty
  Just (h, _) | recognizedFirst h -> resolvePlain' raw
              | otherwise         -> YString raw

-- | First-character check: filter for chars that could possibly
-- begin a YAML core-schema literal (null, bool, inf/nan, signed
-- digit, '+/-/.' or '~'). Skips the costly comparison cascade
-- for the common case of unquoted plain strings.
recognizedFirst :: Char -> Bool
recognizedFirst c =
  c == 'n' || c == 'N'
  || c == 't' || c == 'T'
  || c == 'f' || c == 'F'
  || c == '~'
  || c == '.'
  || c == '+' || c == '-'
  || (c >= '0' && c <= '9')
{-# INLINE recognizedFirst #-}

resolvePlain' :: Text -> Value
resolvePlain' raw
  | raw == "null" || raw == "~" || raw == "Null" || raw == "NULL" = YNull
  | raw == "true" || raw == "True" || raw == "TRUE"               = YBool True
  | raw == "false" || raw == "False" || raw == "FALSE"            = YBool False
  | raw == ".inf" || raw == ".Inf" || raw == ".INF"
      || raw == "+.inf" || raw == "+.Inf" || raw == "+.INF"       = YFloat (1/0)
  | raw == "-.inf" || raw == "-.Inf" || raw == "-.INF"            = YFloat (-1/0)
  | raw == ".nan" || raw == ".NaN" || raw == ".NAN"               = YFloat (0/0)
  | otherwise = case parseIntCore raw of
      Just n  -> YInt n
      Nothing -> case parseFloatCore raw of
        Just d  -> YFloat d
        Nothing -> YString raw

parseIntCore :: Text -> Maybe Int64
parseIntCore raw0 = case T.uncons raw0 of
  Just ('+', rest) -> parseUnsigned rest
  Just ('-', rest) -> negate <$> parseUnsigned rest
  _                -> parseUnsigned raw0
  where
    parseUnsigned r =
      -- Hot path: a body of all decimal digits with no '0o' /
      -- '0x' prefix and no underscores parses with a single
      -- T.foldl'. Also short-circuit on an empty body.
      case T.uncons r of
        Nothing                -> Nothing
        Just ('0', rest)
          | T.null rest        -> Just 0
          | otherwise          -> case T.head rest of
              'x' -> hexBody (T.drop 1 rest)
              'X' -> hexBody (T.drop 1 rest)
              'o' -> octBody (T.drop 1 rest)
              'O' -> octBody (T.drop 1 rest)
              c | c >= '0' && c <= '9' ->
                  -- '0' followed by digits — leading zero is
                  -- only valid for a hex/oct/bin prefix per the
                  -- core schema. Surface as Nothing so the
                  -- value falls through to YString.
                  Nothing
              _   -> decBody r
        Just (h, _)
          | h >= '0' && h <= '9' -> decBody r
          | otherwise            -> hexFallback r

    decBody r
      | T.any (not . isDigit) r = Nothing
      | otherwise =
          Just (T.foldl' (\acc c -> acc * 10 + fromIntegral (digitToInt c)) 0 r)

    hexBody body
      | T.null body || T.any (not . isHexDigit) body = Nothing
      | otherwise =
          Just (T.foldl' (\acc c -> acc * 16 + fromIntegral (digitToInt c)) 0 body)

    octBody body
      | T.null body || T.any (\c -> c < '0' || c > '7') body = Nothing
      | otherwise =
          Just (T.foldl' (\acc c -> acc * 8 + fromIntegral (digitToInt c)) 0 body)

    -- 'hexFallback' fires when the input doesn't start with a
    -- digit / sign / '0x|0o' prefix; it never parses as an int.
    hexFallback _ = Nothing

parseFloatCore :: Text -> Maybe Double
parseFloatCore t = case TR.signed TR.double t of
  Right (d, leftover) | T.null leftover -> Just d
  _                                     -> Nothing

-- ---------------------------------------------------------------------------
-- Tags
-- ---------------------------------------------------------------------------

expandTag :: Text -> Tag
expandTag t
  | T.isPrefixOf "!!" t =
      Tag (tYamlTagPrefix <> T.drop 2 t)
  | T.isPrefixOf "!<" t && T.isSuffixOf ">" t =
      Tag (T.init (T.drop 2 t))
  | otherwise = Tag t

-- | Resolve a tag token against the current %TAG shortcut map.
-- Refuses references to undefined shortcuts (spec §6.8.2).
expandTagP :: Int -> Text -> P Tag
expandTagP lno t
  -- '!!something' uses the implicit secondary handle '!!'.
  | T.isPrefixOf "!!" t = pure (expandTag t)
  -- '!<verbatim>' is a verbatim tag.
  | T.isPrefixOf "!<" t && T.isSuffixOf ">" t = pure (expandTag t)
  -- '!handle!suffix' uses a primary or named shortcut.
  | T.isPrefixOf "!" t
  , let rest = T.drop 1 t
  , (handleBody, sfx) <- T.break (== '!') rest
  , not (T.null sfx)
  , let handle = T.cons '!' (T.snoc handleBody '!')
  = do
      ms <- lookupShortcut handle
      case ms of
        Just prefix -> pure (Tag (prefix <> T.drop 1 sfx))
        Nothing -> failP $ "undefined %TAG shortcut "
                         ++ show handle ++ " (line "
                         ++ show lno ++ ")"
  | otherwise = pure (expandTag t)

-- ---------------------------------------------------------------------------
-- @key: value@ split (top-level, respects quotes / brackets)
-- ---------------------------------------------------------------------------

-- | Walks the line in O(n) by reading the underlying UTF-8
-- bytes directly. Returns @Just (key, rest)@ when a top-level
-- @':'@ separator is found. The key/rest split is byte-position
-- based, which coincides with the character split when the line
-- is ASCII (the overwhelmingly common case for YAML keys).
--
-- For lines that contain non-ASCII multi-byte characters before
-- the separator, we fall back to the slower char-based split so
-- that 'T.take' / 'T.drop' produce correctly aligned slices.
findKeyValueSplit :: Text -> Maybe (Text, Text)
findKeyValueSplit input@(TI.Text arr off blen)
  | blen == 0 = Nothing
  | otherwise = goByte off 0 0 0 32
  where
    !endByte = off + blen
    -- 'goByte' uses byte indices into the underlying TA.Array.
    -- 'p' is the previous byte's code for atTokenStart.
    goByte :: Int -> Int -> Int -> Int -> Int
           -> Maybe (Text, Text)
    goByte !i !d !b !s !p
      | i >= endByte = Nothing
      | otherwise =
          let !w = TA.unsafeIndex arr i
              !c = toEnum (fromIntegral w) :: Char
              wi = fromIntegral w :: Int
          in case s of
               1 -> case c of
                 '"'  -> goByte (i + 1) d b 0 wi
                 '\\' | i + 1 < endByte ->
                        goByte (i + 2) d b 1 wi
                 _    -> goByte (i + 1) d b 1 wi
               2 -> case c of
                 '\'' -> goByte (i + 1) d b 0 wi
                 _    -> goByte (i + 1) d b 2 wi
               _ -> case c of
                 '"'  | ts -> goByte (i + 1) d b 1 wi
                 '\'' | ts -> goByte (i + 1) d b 2 wi
                 '['  | ts || d > 0 || b > 0 ->
                          goByte (i + 1) (d + 1) b s wi
                 ']'  | d > 0 ->
                          goByte (i + 1) (d - 1) b s wi
                 '{'  | ts || d > 0 || b > 0 ->
                          goByte (i + 1) d (b + 1) s wi
                 '}'  | b > 0 ->
                          goByte (i + 1) d (b - 1) s wi
                 '#'  | ts && d == 0 && b == 0 -> Nothing
                 ':'  | d == 0, b == 0 ->
                          if i + 1 >= endByte
                            then Just (sliceKey i, T.empty)
                            else
                              let n = TA.unsafeIndex arr (i + 1)
                              in if n == 32 || n == 9
                                   then Just ( sliceKey i
                                             , sliceTail (i + 1) )
                                   else goByte (i + 1) d b s wi
                 _    -> goByte (i + 1) d b s wi
       where
         ts = p == 32 || p == 9

    -- Slice [off, i) from the underlying array (a zero-copy
    -- Text), strip trailing ASCII whitespace, then unquote.
    sliceKey i = unquoteKey (T.stripEnd (TI.text arr off (i - off)))

    sliceTail i = TI.text arr i (endByte - i)
{-# INLINABLE findKeyValueSplit #-}

unquoteKey :: Text -> Text
unquoteKey t
  | T.length t >= 2 && T.head t == '"' && T.last t == '"'
      = unescapeDQ (T.init (T.tail t))
  | T.length t >= 2 && T.head t == '\'' && T.last t == '\''
      = T.replace "''" "'" (T.init (T.tail t))
  | otherwise = T.strip t

-- | Lightweight unescape for the tiny escape vocabulary we accept in
-- a quoted /key/ position. Full DQ escapes are handled by 'parseDQ'.
unescapeDQ :: Text -> Text
unescapeDQ = T.pack . go . T.unpack
  where
    go [] = []
    go ('\\':'"':rest)  = '"'  : go rest
    go ('\\':'\\':rest) = '\\' : go rest
    go ('\\':'n':rest)  = '\n' : go rest
    go ('\\':'t':rest)  = '\t' : go rest
    go ('\\':'r':rest)  = '\r' : go rest
    go (c:rest)         = c    : go rest

-- ---------------------------------------------------------------------------
-- Inline-comment stripping (respects quotes)
-- ---------------------------------------------------------------------------

stripInlineComment :: Text -> Text
stripInlineComment t
  -- Fast path: if there's no '#' anywhere, return the input
  -- untouched (the common case for typical mapping values).
  | not (bAnyByte w8Hash t) = t
  | otherwise =
      -- Walk the underlying bytes, finding the first
      -- column-stopping '#' (preceded by ' ', '\\t', or '\\1' and
      -- not inside a quoted span). For an outer-context match,
      -- we either truncate at the preceding whitespace or, for
      -- a '\\1' match, splice in a '\\2' sentinel and skip the
      -- comment body.
      let !len = bLen t
          go !i !st
            | i >= len  = bSlice t 0 i
            | otherwise = case st of
                Outer ->
                  let !c = bAt t i
                  in case c of
                       34 -> go (i + 1) InDQ        -- '"'
                       39 -> go (i + 1) InSQ        -- '\''
                       35 ->                         -- '#'
                         if i > 0
                            && let !p = bAt t (i - 1)
                               in p == w8Space || p == w8Tab
                            then bSlice t 0 (i - 1)   -- drop space + #
                            else if i > 0 && bAt t (i - 1) == w8SOH
                                   then -- '\\1#' → '\\2' sentinel +
                                        -- skip up to next '\\1'
                                        spliceAt (i - 1)
                                   else go (i + 1) Outer
                       _  -> go (i + 1) Outer
                InDQ -> case bAt t i of
                  92 | i + 1 < len ->                 -- '\\' escape
                       go (i + 2) InDQ
                  34 -> go (i + 1) Outer
                  _  -> go (i + 1) InDQ
                InSQ ->
                  let !c = bAt t i
                  in if c == w8SQuote
                       then if i + 1 < len && bAt t (i + 1) == w8SQuote
                              then go (i + 2) InSQ
                              else go (i + 1) Outer
                       else go (i + 1) InSQ
          spliceAt !brk =
            -- Replace the '\\1#...' run with '\\2' up to the
            -- next '\\1' (or end of buffer). The slow-path
            -- splice still allocates a new Text via T.pack but
            -- only when this rare case fires.
            let prefix = bSlice t 0 brk
                rest0  = bDrop (brk + 1) t
                afterC = T.dropWhile (/= '\1') rest0
            in prefix <> tSTX <> afterC
      in go 0 Outer
{-# INLINABLE stripInlineComment #-}

data QState = Outer | InDQ | InSQ

-- ---------------------------------------------------------------------------
-- Double-quoted escape decoding
-- ---------------------------------------------------------------------------

decodeDQEscape :: Text -> Int -> Maybe (Char, Int)
decodeDQEscape t !i = case T.index t i of
  '0'  -> Just ('\0', i + 1)
  'a'  -> Just ('\a', i + 1)
  'b'  -> Just ('\b', i + 1)
  't'  -> Just ('\t', i + 1)
  '\t' -> Just ('\t', i + 1)   -- '\<TAB>' as literal tab
  'n'  -> Just ('\n', i + 1)
  'v'  -> Just ('\v', i + 1)
  'f'  -> Just ('\f', i + 1)
  'r'  -> Just ('\r', i + 1)
  'e'  -> Just ('\x1B', i + 1)
  ' '  -> Just (' ', i + 1)
  '"'  -> Just ('"', i + 1)
  '/'  -> Just ('/', i + 1)
  '\\' -> Just ('\\', i + 1)
  'N'  -> Just ('\x85',   i + 1)
  '_'  -> Just ('\xA0',   i + 1)
  'L'  -> Just ('\x2028', i + 1)
  'P'  -> Just ('\x2029', i + 1)
  'x'  -> readHex t (i + 1) 2
  'u'  -> readHex t (i + 1) 4
  'U'  -> readHex t (i + 1) 8
  _    -> Nothing
  where
    readHex tx !j n
      | j + n > T.length tx = Nothing
      | otherwise =
          let chunk = T.take n (T.drop j tx)
          in if T.all isHexDigit chunk
               then Just ( chr (T.foldl' (\acc c -> acc * 16 + digitToInt c) 0 chunk)
                         , j + n)
               else Nothing
