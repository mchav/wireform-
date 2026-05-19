module Network.HTTP2.Types
  ( StreamId
  , FrameType (..)
  , ErrorCode (..)
  , Settings (..)
  , defaultSettings
  , WindowSize
  , HeaderName
  , HeaderValue
  , Header
  , HeaderList
  , Priority (..)
  , defaultPriority
  ) where

import Data.ByteString (ByteString)
import Data.Word

type StreamId = Word32

data FrameType
  = FrameData
  | FrameHeaders
  | FramePriority
  | FrameRSTStream
  | FrameSettings
  | FramePushPromise
  | FramePing
  | FrameGoAway
  | FrameWindowUpdate
  | FrameContinuation
  | FrameUnknown !Word8
  deriving stock (Eq, Show)

frameTypeToWord8 :: FrameType -> Word8
frameTypeToWord8 = \case
  FrameData -> 0x0
  FrameHeaders -> 0x1
  FramePriority -> 0x2
  FrameRSTStream -> 0x3
  FrameSettings -> 0x4
  FramePushPromise -> 0x5
  FramePing -> 0x6
  FrameGoAway -> 0x7
  FrameWindowUpdate -> 0x8
  FrameContinuation -> 0x9
  FrameUnknown w -> w

word8ToFrameType :: Word8 -> FrameType
word8ToFrameType = \case
  0x0 -> FrameData
  0x1 -> FrameHeaders
  0x2 -> FramePriority
  0x3 -> FrameRSTStream
  0x4 -> FrameSettings
  0x5 -> FramePushPromise
  0x6 -> FramePing
  0x7 -> FrameGoAway
  0x8 -> FrameWindowUpdate
  0x9 -> FrameContinuation
  w -> FrameUnknown w

data ErrorCode
  = NoError
  | ProtocolError
  | InternalError
  | FlowControlError
  | SettingsTimeout
  | StreamClosed
  | FrameSizeError
  | RefusedStream
  | Cancel
  | CompressionError
  | ConnectError
  | EnhanceYourCalm
  | InadequateSecurity
  | HTTP11Required
  | UnknownError !Word32
  deriving stock (Eq, Show)

errorCodeToWord32 :: ErrorCode -> Word32
errorCodeToWord32 = \case
  NoError -> 0x0
  ProtocolError -> 0x1
  InternalError -> 0x2
  FlowControlError -> 0x3
  SettingsTimeout -> 0x4
  StreamClosed -> 0x5
  FrameSizeError -> 0x6
  RefusedStream -> 0x7
  Cancel -> 0x8
  CompressionError -> 0x9
  ConnectError -> 0xa
  EnhanceYourCalm -> 0xb
  InadequateSecurity -> 0xc
  HTTP11Required -> 0xd
  UnknownError w -> w

word32ToErrorCode :: Word32 -> ErrorCode
word32ToErrorCode = \case
  0x0 -> NoError
  0x1 -> ProtocolError
  0x2 -> InternalError
  0x3 -> FlowControlError
  0x4 -> SettingsTimeout
  0x5 -> StreamClosed
  0x6 -> FrameSizeError
  0x7 -> RefusedStream
  0x8 -> Cancel
  0x9 -> CompressionError
  0xa -> ConnectError
  0xb -> EnhanceYourCalm
  0xc -> InadequateSecurity
  0xd -> HTTP11Required
  w -> UnknownError w

data Settings = Settings
  { settingsHeaderTableSize :: !Word32
  , settingsEnablePush :: !Bool
  , settingsMaxConcurrentStreams :: !(Maybe Word32)
  , settingsInitialWindowSize :: !Word32
  , settingsMaxFrameSize :: !Word32
  , settingsMaxHeaderListSize :: !(Maybe Word32)
  }
  deriving stock (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings
  { settingsHeaderTableSize = 4096
  , settingsEnablePush = True
  , settingsMaxConcurrentStreams = Nothing
  , settingsInitialWindowSize = 65535
  , settingsMaxFrameSize = 16384
  , settingsMaxHeaderListSize = Nothing
  }

type WindowSize = Int

type HeaderName = ByteString
type HeaderValue = ByteString
type Header = (HeaderName, HeaderValue)
type HeaderList = [Header]

data Priority = Priority
  { priorityExclusive :: !Bool
  , priorityDependency :: !StreamId
  , priorityWeight :: !Word8
  }
  deriving stock (Eq, Show)

defaultPriority :: Priority
defaultPriority = Priority
  { priorityExclusive = False
  , priorityDependency = 0
  , priorityWeight = 16
  }
