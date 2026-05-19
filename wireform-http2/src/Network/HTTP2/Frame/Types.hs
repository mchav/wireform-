module Network.HTTP2.Frame.Types
  ( FrameHeader (..)
  , FramePayload (..)
  , Frame (..)
  , FrameFlags
  , flagEndStream
  , flagEndHeaders
  , flagPadded
  , flagPriority
  , flagAck
  , testFlag
  , setFlag
  , frameHeaderLength
  , connectionPreface
  ) where

import Control.DeepSeq (NFData)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import GHC.Generics (Generic)

import Network.HTTP2.Types

type FrameFlags = Word8

flagEndStream :: FrameFlags
flagEndStream = 0x1

flagAck :: FrameFlags
flagAck = 0x1

flagEndHeaders :: FrameFlags
flagEndHeaders = 0x4

flagPadded :: FrameFlags
flagPadded = 0x8

flagPriority :: FrameFlags
flagPriority = 0x20

{-# INLINE testFlag #-}
testFlag :: FrameFlags -> FrameFlags -> Bool
testFlag flags flag = flags .&. flag /= 0

{-# INLINE setFlag #-}
setFlag :: FrameFlags -> FrameFlags -> FrameFlags
setFlag flags flag = flags .|. flag

frameHeaderLength :: Int
frameHeaderLength = 9

connectionPreface :: ByteString
connectionPreface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

data FrameHeader = FrameHeader
  { fhLength :: !Word32
  , fhType :: !FrameType
  , fhFlags :: !FrameFlags
  , fhStreamId :: !StreamId
  }
  deriving stock (Eq, Show, Generic)

instance NFData FrameHeader

data FramePayload
  = DataFrame !ByteString
  | HeadersFrame !(Maybe Priority) !ByteString
  | PriorityFrame !Priority
  | RSTStreamFrame !ErrorCode
  | SettingsFrame ![(Word16, Word32)]
  | PushPromiseFrame !StreamId !ByteString
  | PingFrame !ByteString
  | GoAwayFrame !StreamId !ErrorCode !ByteString
  | WindowUpdateFrame !Word32
  | ContinuationFrame !ByteString
  | UnknownFrame !Word8 !ByteString
  deriving stock (Eq, Show)

data Frame = Frame
  { frameHeader :: !FrameHeader
  , framePayload :: !FramePayload
  }
  deriving stock (Eq, Show)
