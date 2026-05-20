{- | HTTP protocol versions.

A 'Version' identifies an on-the-wire HTTP protocol dialect.  We pack
@major.minor@ into a single 'Word8' (4 bits each), so the 'Version'
type fits in one machine register; that matches the hermes layout
this module was vendored from.

The pattern synonyms 'HTTP1_0', 'HTTP1_1', 'HTTP2', 'HTTP3' cover
every version the rest of the codebase currently speaks; arbitrary
versions are still representable via 'mkVersion'.
-}
{-# LANGUAGE PatternSynonyms #-}
module Network.HTTP.Types.Version
  ( Version
  , mkVersion
  , versionMajor
  , versionMinor
  , versionToBytes
  , versionFromBytes
    -- * Common versions
  , pattern HTTP0_9
  , pattern HTTP1_0
  , pattern HTTP1_1
  , pattern HTTP2
  , pattern HTTP3
  ) where

import Control.DeepSeq (NFData)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hashable (Hashable)
import Data.Word (Word8)
import GHC.Generics (Generic)

-- | A packed @major.minor@ HTTP version.
--
-- Comparison is lexicographic on (major, minor); i.e.
-- @HTTP1_0 < HTTP1_1 < HTTP2 < HTTP3@.
newtype Version = Version Word8
  deriving stock (Eq, Generic)
  deriving newtype (Hashable, NFData)

instance Show Version where
  showsPrec _ v =
    showString "mkVersion "
      . shows (versionMajor v)
      . showString " "
      . shows (versionMinor v)

instance Ord Version where
  compare a b = case compare (versionMajor a) (versionMajor b) of
    EQ -> compare (versionMinor a) (versionMinor b)
    other -> other

-- | Build a 'Version' from its @major.minor@ digits. Each component
-- must fit in 4 bits (0..15); larger values are silently truncated.
{-# INLINE mkVersion #-}
mkVersion :: Word8 -> Word8 -> Version
mkVersion major minor =
  Version $ ((major .&. 0x0F) `shiftL` 4) .|. (minor .&. 0x0F)

{-# INLINE versionMajor #-}
versionMajor :: Version -> Word8
versionMajor (Version w) = w `shiftR` 4

{-# INLINE versionMinor #-}
versionMinor :: Version -> Word8
versionMinor (Version w) = w .&. 0x0F

-- | Render the canonical on-the-wire spelling of a 'Version'.
versionToBytes :: Version -> ByteString
versionToBytes v = case (versionMajor v, versionMinor v) of
  (0, 9) -> "HTTP/0.9"
  (1, 0) -> "HTTP/1.0"
  (1, 1) -> "HTTP/1.1"
  (2, 0) -> "HTTP/2"
  (3, 0) -> "HTTP/3"
  (mj, mn) ->
    "HTTP/" <> BS.singleton (digit mj) <> "." <> BS.singleton (digit mn)
  where
    digit n
      | n < 10 = 0x30 + n
      | otherwise = 0x3F  -- '?'; only reachable if mkVersion's mask is bypassed

-- | Strict reverse of 'versionToBytes'. Only the canonical spellings
-- are recognised; everything else returns 'Nothing'.
versionFromBytes :: ByteString -> Maybe Version
versionFromBytes bs
  | bs == "HTTP/1.1" = Just HTTP1_1
  | bs == "HTTP/1.0" = Just HTTP1_0
  | bs == "HTTP/2"   = Just HTTP2
  | bs == "HTTP/2.0" = Just HTTP2
  | bs == "HTTP/3"   = Just HTTP3
  | bs == "HTTP/3.0" = Just HTTP3
  | bs == "HTTP/0.9" = Just HTTP0_9
  | otherwise        = Nothing

pattern HTTP0_9 :: Version
pattern HTTP0_9 = Version 0x09

pattern HTTP1_0 :: Version
pattern HTTP1_0 = Version 0x10

pattern HTTP1_1 :: Version
pattern HTTP1_1 = Version 0x11

pattern HTTP2 :: Version
pattern HTTP2 = Version 0x20

pattern HTTP3 :: Version
pattern HTTP3 = Version 0x30
