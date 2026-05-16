module Network.GRPC.Util.Stream (
    -- * Streams
    OutputStream(..)
  , writeChunk
  , writeChunkFinal
  , flush
  , InputStream(..)
  , getChunk
  , getTrailers
    -- * Boundary conversion between Wireform and bytestring 'Builder'
  , toBSBuilder
  , fromBSBuilder
    -- * Exceptions
  , ClientDisconnected(..)
  , ServerDisconnected(..)
  , wrapStreamExceptionsWith
  ) where

import Data.ByteString.Builder qualified as BSB
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as Lazy (toStrict)
import Network.HTTP.Types qualified as HTTP
import Wireform.Builder (Builder)
import Wireform.Builder qualified as WB

import Network.GRPC.Util.Backtrace
import Network.GRPC.Util.Imports

{-------------------------------------------------------------------------------
  Streams
-------------------------------------------------------------------------------}

data OutputStream = OutputStream {
      -- | Write a chunk to the stream
      _writeChunk :: HasCallStack => Builder -> IO ()

      -- | Write the final chunk to the stream
    , _writeChunkFinal :: HasCallStack => Builder -> IO ()

      -- | Flush the stream (send frames to the peer)
    , _flush :: HasCallStack => IO ()
    }

data InputStream = InputStream {
      _getChunk    :: HasCallStack => IO (Strict.ByteString, Bool)
    , _getTrailers :: HasCallStack => IO [HTTP.Header]
    }

{-------------------------------------------------------------------------------
  Wrappers to get the proper CallStack
-------------------------------------------------------------------------------}

writeChunk :: HasCallStack => OutputStream -> Builder -> IO ()
writeChunk = _writeChunk

writeChunkFinal :: HasCallStack => OutputStream -> Builder -> IO ()
writeChunkFinal = _writeChunkFinal

flush :: HasCallStack => OutputStream -> IO ()
flush = _flush

-- | Materialise a 'Wireform.Builder.Builder' into a 'BSB.Builder' so it
-- can be handed to @http-semantics@ (which uses bytestring's builder).
-- Streams through 'WB.toLazyByteString' and rewraps as 'BSB.lazyByteString',
-- so the chunked layout is preserved.
toBSBuilder :: Builder -> BSB.Builder
toBSBuilder = BSB.lazyByteString . WB.toLazyByteString
{-# INLINE toBSBuilder #-}

-- | Materialise a 'BSB.Builder' (returned by @grpc-spec@'s @buildInput@ /
-- @buildOutput@) into a 'Wireform.Builder.Builder'. Materialises into
-- a strict bytestring and re-embeds; the underlying bytes are allocated
-- exactly once and then handed through unchanged.
fromBSBuilder :: BSB.Builder -> Builder
fromBSBuilder = WB.byteString . Lazy.toStrict . BSB.toLazyByteString
{-# INLINE fromBSBuilder #-}

getChunk :: HasCallStack => InputStream -> IO (Strict.ByteString, Bool)
getChunk = _getChunk

getTrailers :: HasCallStack => InputStream -> IO [HTTP.Header]
getTrailers = _getTrailers

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Client disconnected unexpectedly
--
-- /If/ you choose to catch this exception, you are advised to match against
-- the type, rather than against the constructor, and then use the record
-- accessors to get access to the fields. Future versions of @grapesy@ may
-- record more information.
data ClientDisconnected = ClientDisconnected {
      clientDisconnectedException :: SomeException
    , clientDisconnectedCallStack :: Backtraces
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Server disconnected unexpectedly
--
-- See comments for 'ClientDisconnected' on how to catch this exception.
data ServerDisconnected = ServerDisconnected {
      serverDisconnectedException :: SomeException
    , serverDisconnectedCallstack :: Backtraces
    }
  deriving stock (Show)
  deriving anyclass (Exception)

wrapStreamExceptionsWith ::
     (HasCallStack, Exception e)
  => (SomeException -> Backtraces -> e)
  -> IO a -> IO a
wrapStreamExceptionsWith f action =
    action `catch` \err -> do
      backtraces <- collectBacktraces
      throwIO $ f err backtraces
