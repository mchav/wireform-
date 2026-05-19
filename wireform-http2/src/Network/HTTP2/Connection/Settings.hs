module Network.HTTP2.Connection.Settings
  ( Settings (..)
  , defaultSettings
  , applySettingsParams
  , encodeSettings
  , SettingsError (..)
  ) where

import Data.Word
import Network.HTTP2.Types (Settings(..), defaultSettings)

data SettingsError
  = InvalidEnablePush !Word32
  | InvalidInitialWindowSize !Word32
  | InvalidMaxFrameSize !Word32
  deriving stock (Eq, Show)

applySettingsParams :: Settings -> [(Word16, Word32)] -> Either SettingsError Settings
applySettingsParams = go
  where
    go s [] = Right s
    go s ((ident, val):rest) = case ident of
      0x1 -> go s { settingsHeaderTableSize = val } rest
      0x2 -> if val > 1
               then Left (InvalidEnablePush val)
               else go s { settingsEnablePush = val /= 0 } rest
      0x3 -> go s { settingsMaxConcurrentStreams = Just val } rest
      0x4 -> if val > 2147483647
               then Left (InvalidInitialWindowSize val)
               else go s { settingsInitialWindowSize = val } rest
      0x5 -> if val < 16384 || val > 16777215
               then Left (InvalidMaxFrameSize val)
               else go s { settingsMaxFrameSize = val } rest
      0x6 -> go s { settingsMaxHeaderListSize = Just val } rest
      _   -> go s rest  -- Unknown settings MUST be ignored (RFC 9113 Section 6.5.2)

encodeSettings :: Settings -> [(Word16, Word32)]
encodeSettings s =
  [ (0x1, settingsHeaderTableSize s)
  , (0x3, maybe 100 id (settingsMaxConcurrentStreams s))
  , (0x4, settingsInitialWindowSize s)
  , (0x5, settingsMaxFrameSize s)
  ] <> maybe [] (\v -> [(0x6, v)]) (settingsMaxHeaderListSize s)
    <> if settingsEnablePush s then [] else [(0x2, 0)]
