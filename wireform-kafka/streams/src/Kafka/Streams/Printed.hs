{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Printed
-- Description : @KStream.print(Printed)@ builder
--
-- Mirrors the JVM
-- @org.apache.kafka.streams.kstream.Printed<K,V>@ builder
-- one-to-one:
--
--   * 'toSysOut' / 'toErr' / 'toHandle' send each record to a
--     well-known 'System.IO.Handle' or a caller-supplied one.
--   * 'toFile' opens a single file in append mode.
--   * 'toRotatingFile' opens a 'RF.RotatingHandle' so the sink
--     auto-rolls by size or age (closes the gap the @README.md@
--     historically called out).
--   * 'withLabel' decorates every line with a caller-supplied
--     prefix (Java's @Printed.withLabel("counts")@).
--   * 'withKeyValueMapper' overrides the default
--     @key=<show k> value=<show v>@ rendering.
--
-- And 'printKStream' applies a fully-built 'Printed' to a
-- 'KStream'. Equivalent to Java's @kstream.print(Printed)@:
--
-- @
-- import qualified Kafka.Streams.Printed as Printed
--
-- Printed.printKStream
--   (Printed.toRotatingFileWith \"\/var\/log\/streams.log\"
--      (Printed.def
--         & Printed.withLabel \"counts\"
--         & Printed.withMaxBytes (16 * 1024 * 1024)
--         & Printed.withMaxAgeMs (60 * 60 * 1000)))
--   countStream
-- @
module Kafka.Streams.Printed
  ( -- * Builder
    Printed
  , toSysOut
  , toErr
  , toHandle
  , toFile
  , toRotatingFile
  , toRotatingFileWith
    -- * Modifiers (Java's @Printed.with*@)
  , withLabel
  , withKeyValueMapper
    -- * Apply
  , printKStream
  , withPrintedFile
  , withPrintedRotatingFile
    -- * Re-exports for the rotating-file shape
  , def
  , withMaxBytes
  , withMaxAgeMs
  , RotationOptions
  ) where

import Control.Exception (bracket)
import Data.Function ((&))
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import System.IO (Handle, hPutStrLn, stderr, stdout)
import qualified System.IO as IO

import qualified Kafka.Streams.KStream as KS
import qualified Kafka.Streams.Sink.RotatingFile as RF
import Kafka.Streams.Types (Record (..))

----------------------------------------------------------------------
-- Printed builder
----------------------------------------------------------------------

-- | A fully-built print sink. Construct one with 'toSysOut' /
-- 'toErr' / 'toHandle' / 'toFile' / 'toRotatingFile', decorate
-- it with 'withLabel' / 'withKeyValueMapper', then hand the
-- result to 'printKStream'.
data Printed k v = Printed
  { pSink     :: !(Sink k v)
  , pLabel    :: !Text
  , pRenderer :: !(Text -> Record k v -> Text)
  }

data Sink k v
  = SinkHandle !Handle
  | SinkFile !FilePath
  | SinkRotating !FilePath !RotationOptions
  | SinkRotatingHandle !RF.RotatingHandle
    -- ^ Used internally by 'withPrintedRotatingFile' so the
    -- bracket-owner controls the rotating-handle lifecycle.

----------------------------------------------------------------------
-- Constructors
----------------------------------------------------------------------

-- | Print to @stdout@.
toSysOut :: (Show k, Show v) => Printed k v
toSysOut = toHandle stdout

-- | Print to @stderr@.
toErr :: (Show k, Show v) => Printed k v
toErr = toHandle stderr

-- | Print to a caller-supplied 'Handle'. The handle's
-- buffering / locking is the caller's responsibility.
toHandle :: (Show k, Show v) => Handle -> Printed k v
toHandle h = Printed
  { pSink     = SinkHandle h
  , pLabel    = "[stream]"
  , pRenderer = defaultRenderer
  }

-- | Append to a single file. /No/ rotation; the file grows
-- without bound. Use 'toRotatingFile' for rotation.
toFile :: (Show k, Show v) => FilePath -> Printed k v
toFile p = Printed
  { pSink     = SinkFile p
  , pLabel    = "[stream]"
  , pRenderer = defaultRenderer
  }

-- | Open a rotating-file sink at the given path with default
-- rotation options (16 MiB or 1 h, whichever comes first). Use
-- 'toRotatingFileWith' to override.
toRotatingFile :: (Show k, Show v) => FilePath -> Printed k v
toRotatingFile p = toRotatingFileWith p def

-- | Open a rotating-file sink at the given path with caller-
-- supplied options.
toRotatingFileWith
  :: (Show k, Show v)
  => FilePath -> RotationOptions -> Printed k v
toRotatingFileWith p opts = Printed
  { pSink     = SinkRotating p opts
  , pLabel    = "[stream]"
  , pRenderer = defaultRenderer
  }

----------------------------------------------------------------------
-- Modifiers
----------------------------------------------------------------------

-- | Prefix every line with the given label. Mirrors
-- @Printed.withLabel(label)@.
withLabel :: Text -> Printed k v -> Printed k v
withLabel l p = p { pLabel = l }

-- | Override the per-record renderer. Mirrors
-- @Printed.withKeyValueMapper(KeyValueMapper)@. The caller's
-- function receives the configured label plus the record and
-- returns the line to write (no trailing newline).
withKeyValueMapper
  :: (Text -> Record k v -> Text) -> Printed k v -> Printed k v
withKeyValueMapper f p = p { pRenderer = f }

----------------------------------------------------------------------
-- Rotation options (mirror of 'RF.RotatingFileConfig' minus the path)
----------------------------------------------------------------------

-- | Tunable rotation policy for 'toRotatingFile'. Identical
-- shape to 'RF.RotatingFileConfig' minus the path / buffer
-- mode (we always use line buffering + best-effort fsync; the
-- rest is rotation policy).
data RotationOptions = RotationOptions
  { roMaxBytes :: !(Maybe Int64)
  , roMaxAge   :: !(Maybe Int64)
  }

-- | Default rotation: 16 MiB or 1 hour, whichever comes first.
def :: RotationOptions
def = RotationOptions
  { roMaxBytes = Just (16 * 1024 * 1024)
  , roMaxAge   = Just (60 * 60 * 1000)
  }

withMaxBytes :: Int64 -> RotationOptions -> RotationOptions
withMaxBytes n r = r { roMaxBytes = Just n }

withMaxAgeMs :: Int64 -> RotationOptions -> RotationOptions
withMaxAgeMs n r = r { roMaxAge = Just n }

----------------------------------------------------------------------
-- Apply
----------------------------------------------------------------------

-- | Attach a 'Printed' sink to a 'KStream'. Returns @IO ()@
-- (terminal sink). Mirrors @KStream.print(Printed)@.
--
-- Lifecycle:
--
--   * 'SinkHandle' — assumes the caller is managing the
--     handle. 'printKStream' never closes it.
--   * 'SinkFile' — opens the file in append mode and leaves it
--     open for the lifetime of the topology process. JVM
--     @Printed.toFile@ has the same lifetime story.
--   * 'SinkRotating' — opens a 'RF.RotatingHandle' and leaves
--     it open. 'rfMaxBytes' / 'rfMaxAge' from
--     'RotationOptions' drive the rolling logic.
--
-- For deterministic shutdown wrap the whole topology with
-- 'Control.Exception.bracket' against your 'Handle' /
-- 'RF.RotatingHandle' acquisition.
printKStream :: Printed k v -> KS.KStream k v -> IO ()
printKStream Printed{..} ks = case pSink of
  SinkHandle h ->
    KS.foreachStream
      (\r -> hPutStrLn h (T.unpack (pRenderer pLabel r))) ks
  SinkFile p -> do
    h <- IO.openFile p IO.AppendMode
    IO.hSetBuffering h IO.LineBuffering
    KS.foreachStream
      (\r -> hPutStrLn h (T.unpack (pRenderer pLabel r))) ks
  SinkRotating p ropts -> do
    rh <- RF.openRotatingHandle RF.defaultRotatingFileConfig
      { RF.rfPath     = p
      , RF.rfMaxBytes = roMaxBytes ropts
      , RF.rfMaxAge   = roMaxAge ropts
      }
    KS.foreachStream (\r -> RF.writeLine rh (pRenderer pLabel r)) ks
  SinkRotatingHandle rh ->
    KS.foreachStream (\r -> RF.writeLine rh (pRenderer pLabel r)) ks

----------------------------------------------------------------------
-- Default renderer
----------------------------------------------------------------------

defaultRenderer :: (Show k, Show v) => Text -> Record k v -> Text
defaultRenderer label r =
  label
    <> " key="
    <> T.pack (show (recordKey r))
    <> " value="
    <> T.pack (show (recordValue r))

----------------------------------------------------------------------
-- Bracketed variants
----------------------------------------------------------------------

-- | Bracket form of 'toFile' + 'printKStream'. Opens the file,
-- runs the supplied body with a 'Printed' built from the
-- file handle, then closes the file even on exception. Use
-- this when you want the file handle to outlive a single
-- topology /run/ but be cleaned up at the end.
--
-- The body is responsible for calling 'printKStream' against
-- the supplied 'Printed' before the topology starts producing
-- records.
withPrintedFile
  :: forall k v r
   . (Show k, Show v)
  => FilePath
  -> (Printed k v -> IO r)
  -> IO r
withPrintedFile p body =
  bracket
    (do
      h <- IO.openFile p IO.AppendMode
      IO.hSetBuffering h IO.LineBuffering
      pure h)
    IO.hClose
    (\h -> body (toHandle h))

-- | Bracket form of 'toRotatingFile' + 'printKStream'. Opens
-- a 'RF.RotatingHandle', runs the supplied body with a
-- 'Printed' wired to that handle, then closes the rotating
-- handle (flushing the in-flight buffer) even on exception.
withPrintedRotatingFile
  :: forall k v r
   . (Show k, Show v)
  => FilePath
  -> RotationOptions
  -> (Printed k v -> IO r)
  -> IO r
withPrintedRotatingFile p opts body =
  bracket
    (RF.openRotatingHandle RF.defaultRotatingFileConfig
      { RF.rfPath     = p
      , RF.rfMaxBytes = roMaxBytes opts
      , RF.rfMaxAge   = roMaxAge opts
      })
    RF.closeRotatingHandle
    $ \rh -> body Printed
        { pSink     = SinkRotatingHandle rh
        , pLabel    = "[stream]"
        , pRenderer = defaultRenderer
        }

-- 'Data.Function.&' is imported for downstream call sites that
-- use the builder-with style. Tiny use-site to silence the
-- unused-import warning.
_useAmp :: Int
_useAmp = 1 & (+ 1)
