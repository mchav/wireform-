{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Sink.RotatingFile
-- Description : Rotating-file debug sink for 'KStream'
--
-- The JVM @KStream.print(Printed.toFile(...))@ ships an
-- automatically-rotating file sink. The Haskell side
-- historically only had 'Kafka.Streams.KStream.printToHandle'
-- (callers supplied their own pre-opened 'Handle'), which left
-- size-based / time-based rotation as user homework.
--
-- This module fills that gap. 'rotatingPrintStream' wires a
-- 'Kafka.Streams.KStream.KStream' to a 'RotatingHandle' that
-- transparently rolls the underlying log file when:
--
--   * the current file's size exceeds 'rfMaxBytes', or
--   * the wall-clock age of the current file exceeds 'rfMaxAge'.
--
-- Rotated files are renamed with a UTC suffix (@.YYYYMMDDThhmmssZ@)
-- so the active file name stays stable for tail-style readers.
--
-- @
-- import Kafka.Streams.Sink.RotatingFile
--
-- rh <- openRotatingHandle 'defaultRotatingFileConfig'
--   { rfPath     = \"/var/log/myapp/stream.log\"
--   , rfMaxBytes = 'Just' (16 * 1024 * 1024)  -- 16 MiB
--   , rfMaxAge   = 'Just' (60 * 60 * 1000)    -- 1 hour
--   }
-- rotatingPrintStream rh \"[stream]\" src
-- @
--
-- 'rotatingPrintStream' returns @IO ()@ (terminal sink). The
-- caller is responsible for the 'RotatingHandle' lifecycle:
-- 'closeRotatingHandle' flushes the in-flight buffer and closes
-- the underlying file handle. Wrap the whole topology with
-- 'Control.Exception.bracket' in production.
module Kafka.Streams.Sink.RotatingFile
  ( -- * Configuration
    RotatingFileConfig (..)
  , defaultRotatingFileConfig
    -- * Handle
  , RotatingHandle
  , openRotatingHandle
  , closeRotatingHandle
  , writeLine
    -- * Sink
  , rotatingPrintStream
  , rotatingPrintToHandle
  ) where

import Control.Concurrent.MVar
  ( MVar
  , modifyMVar_
  , newMVar
  , withMVar
  )
import Control.Exception (bracketOnError, try, SomeException)
import Control.Monad (when)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (renameFile)
import System.FilePath (takeDirectory, takeExtension, takeFileName)
import qualified System.IO as IO

import qualified Kafka.Streams.KStream as KS
import Kafka.Streams.Types (Record (..))

----------------------------------------------------------------------
-- Config
----------------------------------------------------------------------

-- | Rotation policy. At least one of 'rfMaxBytes' / 'rfMaxAge'
-- must be 'Just', otherwise the file never rolls (which is
-- fine; equivalent to 'KS.printToHandle' with the handle
-- managed for you).
data RotatingFileConfig = RotatingFileConfig
  { rfPath       :: !FilePath
    -- ^ Active file path. Rotated files share this base name
    --   with a UTC suffix inserted before the extension.
  , rfMaxBytes   :: !(Maybe Int64)
    -- ^ Maximum size in bytes before rotation. 'Nothing'
    --   disables size-based rotation.
  , rfMaxAge     :: !(Maybe Int64)
    -- ^ Maximum age (since the file's creation in this process)
    --   in milliseconds. 'Nothing' disables time-based rotation.
  , rfBufferMode :: !IO.BufferMode
    -- ^ Buffering mode passed to 'IO.hSetBuffering' after each
    --   roll. Default is 'IO.LineBuffering' so individual
    --   records flush promptly without manual @hFlush@.
  , rfFlush      :: !Bool
    -- ^ Whether to call 'IO.hFlush' after every write. Set to
    --   'True' for crash-safety at the cost of throughput;
    --   'False' (default) lets the buffer mode batch.
  }

-- | A safe default: no rotation, append to the supplied path,
-- line-buffered, no per-write flush. Override 'rfMaxBytes' and/or
-- 'rfMaxAge' to enable rotation.
defaultRotatingFileConfig :: RotatingFileConfig
defaultRotatingFileConfig = RotatingFileConfig
  { rfPath       = "stream.log"
  , rfMaxBytes   = Nothing
  , rfMaxAge     = Nothing
  , rfBufferMode = IO.LineBuffering
  , rfFlush      = False
  }

----------------------------------------------------------------------
-- Handle
----------------------------------------------------------------------

-- | A rotating-file handle. Internally tracks the current open
-- 'IO.Handle', the bytes written since the last roll, and the
-- 'UTCTime' the current file was opened so 'rfMaxAge' can
-- trigger.
--
-- All public operations acquire the embedded 'MVar', so the
-- handle is safe to share across multiple worker threads.
data RotatingHandle = RotatingHandle
  { rhConfig :: !RotatingFileConfig
  , rhState  :: !(MVar RotatingState)
  }

data RotatingState = RotatingState
  { rsHandle      :: !IO.Handle
  , rsBytes       :: !Int64
  , rsOpenedAt    :: !UTCTime
  }

-- | Open the active file (append mode). Throws if the parent
-- directory doesn't exist; consider 'System.Directory.createDirectoryIfMissing'
-- before calling this.
openRotatingHandle :: RotatingFileConfig -> IO RotatingHandle
openRotatingHandle cfg = do
  h <- openAppending (rfPath cfg)
  IO.hSetBuffering h (rfBufferMode cfg)
  now <- getCurrentTime
  -- Best-effort starting-size discovery so a process restart
  -- doesn't reset the rotation accounting on an existing file.
  sz <- bytesInHandle h
  st <- newMVar RotatingState
    { rsHandle   = h
    , rsBytes    = sz
    , rsOpenedAt = now
    }
  pure RotatingHandle { rhConfig = cfg, rhState = st }

openAppending :: FilePath -> IO IO.Handle
openAppending p = IO.openFile p IO.AppendMode

bytesInHandle :: IO.Handle -> IO Int64
bytesInHandle h = do
  -- 'hFileSize' is supported on every backend we target; if it
  -- fails we just return 0 and let rotation accrue from this
  -- write onwards.
  r <- try (IO.hFileSize h) :: IO (Either SomeException Integer)
  pure $ case r of
    Right n -> fromIntegral n
    Left _  -> 0

-- | Close the active file. Subsequent 'writeLine' calls will
-- raise an exception; this is the user's signal that they're
-- done.
closeRotatingHandle :: RotatingHandle -> IO ()
closeRotatingHandle rh = withMVar (rhState rh) $ \st ->
  IO.hClose (rsHandle st)

-- | Write a single line. Triggers a roll first if either
-- 'rfMaxBytes' or 'rfMaxAge' would be exceeded after this write.
writeLine :: RotatingHandle -> Text -> IO ()
writeLine rh@RotatingHandle{..} line = do
  let !payload = line <> "\n"
      !plen    = fromIntegral (T.length payload) :: Int64
  modifyMVar_ rhState $ \st0 -> do
    st1 <- maybeRotate rh st0 plen
    IO.hPutStr (rsHandle st1) (T.unpack payload)
    when (rfFlush rhConfig) (IO.hFlush (rsHandle st1))
    pure st1
      { rsBytes = rsBytes st1 + plen
      }

-- | Decide whether to roll and, if so, do it. Returns the
-- (possibly-new) 'RotatingState'.
maybeRotate
  :: RotatingHandle -> RotatingState -> Int64 -> IO RotatingState
maybeRotate RotatingHandle{..} st0 incoming = do
  now <- getCurrentTime
  let needRoll =
           bySize ||
           byAge now
      bySize = case rfMaxBytes rhConfig of
        Just lim -> rsBytes st0 + incoming > lim
        Nothing  -> False
      byAge t = case rfMaxAge rhConfig of
        Just msLim ->
          let !elapsedMs = round (diffUTCTime t (rsOpenedAt st0) * 1000) :: Int64
          in elapsedMs >= msLim
        Nothing -> False
  if not needRoll
    then pure st0
    else do
      -- 1) Flush + close the current file.
      IO.hFlush  (rsHandle st0)
      IO.hClose  (rsHandle st0)
      -- 2) Rename to the suffixed archive path. If the
      --    rename fails (e.g. the file disappeared) we
      --    swallow the error and reopen — the user's
      --    next write goes to a fresh active file either
      --    way.
      let !archive = archivePath (rfPath rhConfig) (rsOpenedAt st0)
      _ <- try (renameFile (rfPath rhConfig) archive) :: IO (Either SomeException ())
      -- 3) Open a fresh active file.
      h <- bracketOnError
             (openAppending (rfPath rhConfig))
             IO.hClose
             pure
      IO.hSetBuffering h (rfBufferMode rhConfig)
      pure RotatingState
        { rsHandle   = h
        , rsBytes    = 0
        , rsOpenedAt = now
        }

-- | Compute the archive path for a roll. Mirrors the JVM's
-- @Printed.toFile@ behaviour: insert @.YYYYMMDDThhmmssZ@ before
-- the file's extension (or at the tail if the file has none).
--
-- For an active path of @\/var\/log\/app\/stream.log@ at
-- @2026-05-16T11:09:42Z@ the archive becomes
-- @\/var\/log\/app\/stream.20260516T110942Z.log@. When the file
-- has no extension we append the suffix at the end.
archivePath :: FilePath -> UTCTime -> FilePath
archivePath p t =
  let !dir      = takeDirectory p
      !name     = takeFileName p
      !ext      = takeExtension name
      !stem     = take (length name - length ext) name
      !suffix   = "." <> formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" t
      !archived = case ext of
        ""    -> name <> suffix
        _     -> stem <> suffix <> ext
  in (if null dir || dir == "." then "" else dir <> "/") <> archived

-- | The 'POSIXTime' the active file was opened. Exposed for
-- tests that want to assert the rotation-trigger logic.
--
-- Internal; do not export.
_openedAtPosix :: RotatingState -> Double
_openedAtPosix st =
  realToFrac (utcTimeToPOSIXSeconds (rsOpenedAt st))

----------------------------------------------------------------------
-- Sink
----------------------------------------------------------------------

-- | Print every record using @show@; rotate the underlying file
-- according to the 'RotatingHandle' policy. Terminal sink
-- (returns @IO ()@).
--
-- /JVM equivalent:/ @KStream.print(Printed.toFile(path))@ with
-- caller-controlled rotation thresholds.
rotatingPrintStream
  :: (Show k, Show v)
  => RotatingHandle
  -> Text                                 -- ^ prefix label
  -> KS.KStream k v
  -> IO ()
rotatingPrintStream rh label =
  rotatingPrintToHandle rh (defaultRender label)

-- | Variant of 'rotatingPrintStream' with a caller-supplied
-- record renderer. Equivalent to
-- 'Kafka.Streams.KStream.printToHandle' but writes through a
-- 'RotatingHandle' instead of a raw 'IO.Handle'.
rotatingPrintToHandle
  :: forall k v
   . RotatingHandle
  -> (Record k v -> Text)                 -- ^ per-record renderer
  -> KS.KStream k v
  -> IO ()
rotatingPrintToHandle rh render =
  KS.foreachStream (\r -> writeLine rh (render r))

defaultRender :: (Show k, Show v) => Text -> Record k v -> Text
defaultRender label r =
  label
    <> " key="
    <> T.pack (show (recordKey r))
    <> " value="
    <> T.pack (show (recordValue r))
