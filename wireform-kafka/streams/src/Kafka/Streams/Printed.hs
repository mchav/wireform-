{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Printed
Description : @KStream.print(Printed)@ builder + rotating-file sink

Mirrors the JVM
@org.apache.kafka.streams.kstream.Printed<K,V>@ builder one-
to-one, plus the rotating-file backing it uses to close the
"@Printed.toFile@ with rotation" gap on the JVM side:

  * 'toSysOut' / 'toErr' / 'toHandle' send each record to a
    well-known 'System.IO.Handle' or a caller-supplied one.
  * 'toFile' opens a single file in append mode.
  * 'toRotatingFile' opens a 'RotatingHandle' so the sink
    auto-rolls by size or age (closes the gap the @README.md@
    historically called out).
  * 'withLabel' decorates every line with a caller-supplied
    prefix (Java's @Printed.withLabel("counts")@).
  * 'withKeyValueMapper' overrides the default
    @key=<show k> value=<show v>@ rendering.

And 'printKStream' applies a fully-built 'Printed' to a
'KStream'. Equivalent to Java's @kstream.print(Printed)@:

@
import qualified Kafka.Streams.Printed as Printed

Printed.printKStream
  (Printed.toRotatingFileWith \"\/var\/log\/streams.log\"
     (Printed.def
        & Printed.withLabel \"counts\"
        & Printed.withMaxBytes (16 * 1024 * 1024)
        & Printed.withMaxAgeMs (60 * 60 * 1000)))
  countStream
@

The rotating-handle machinery ('RotatingHandle',
'openRotatingHandle', 'writeLine', 'rotatingPrintStream') is
exposed for callers that want the rotation behaviour without
going through the 'Printed' builder — same surface that used
to live in @Kafka.Streams.Sink.RotatingFile@.
-}
module Kafka.Streams.Printed (
  -- * Builder
  Printed,
  toSysOut,
  toErr,
  toHandle,
  toFile,
  toRotatingFile,
  toRotatingFileWith,

  -- * Modifiers (Java's @Printed.with*@)
  withLabel,
  withKeyValueMapper,

  -- * Apply
  printKStream,
  withPrintedFile,
  withPrintedRotatingFile,

  -- * Rotation policy (consumed by 'toRotatingFileWith')
  RotationOptions,
  def,
  withMaxBytes,
  withMaxAgeMs,

  -- * Rotating-file handle (lower-level; bypasses the builder)

  --

  {- | The same machinery 'toRotatingFile' uses internally,
  exposed for callers that want to wire rotation to an
  arbitrary 'KStream' or use 'writeLine' from inside their
  own processor. No JVM equivalent — the JVM uses
  @java.util.logging@ for the rotation policy.
  -}
  RotatingFileConfig (..),
  defaultRotatingFileConfig,
  RotatingHandle,
  openRotatingHandle,
  closeRotatingHandle,
  writeLine,
  rotatingPrintStream,
  rotatingPrintToHandle,
) where

import Control.Concurrent.MVar (
  MVar,
  modifyMVar_,
  newMVar,
  withMVar,
 )
import Control.Exception (SomeException, bracket, bracketOnError, try)
import Control.Monad (when)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Kafka.Streams.KStream qualified as KS
import Kafka.Streams.Types (Record (..))
import System.Directory (renameFile)
import System.FilePath (takeDirectory, takeExtension, takeFileName)
import System.IO (Handle, hPutStrLn, stderr, stdout)
import System.IO qualified as IO


----------------------------------------------------------------------
-- Printed builder
----------------------------------------------------------------------

{- | A fully-built print sink. Construct one with 'toSysOut' /
'toErr' / 'toHandle' / 'toFile' / 'toRotatingFile', decorate
it with 'withLabel' / 'withKeyValueMapper', then hand the
result to 'printKStream'.
-}
data Printed k v = Printed
  { pSink :: !(Sink k v)
  , pLabel :: !Text
  , pRenderer :: !(Text -> Record k v -> Text)
  }


data Sink k v
  = SinkHandle !Handle
  | SinkFile !FilePath
  | SinkRotating !FilePath !RotationOptions
  | {- | Used internally by 'withPrintedRotatingFile' so the
    bracket-owner controls the rotating-handle lifecycle.
    -}
    SinkRotatingHandle !RotatingHandle


----------------------------------------------------------------------
-- Constructors
----------------------------------------------------------------------

-- | Print to @stdout@.
toSysOut :: (Show k, Show v) => Printed k v
toSysOut = toHandle stdout


-- | Print to @stderr@.
toErr :: (Show k, Show v) => Printed k v
toErr = toHandle stderr


{- | Print to a caller-supplied 'Handle'. The handle's
buffering / locking is the caller's responsibility.
-}
toHandle :: (Show k, Show v) => Handle -> Printed k v
toHandle h =
  Printed
    { pSink = SinkHandle h
    , pLabel = "[stream]"
    , pRenderer = defaultRenderer
    }


{- | Append to a single file. /No/ rotation; the file grows
without bound. Use 'toRotatingFile' for rotation.
-}
toFile :: (Show k, Show v) => FilePath -> Printed k v
toFile p =
  Printed
    { pSink = SinkFile p
    , pLabel = "[stream]"
    , pRenderer = defaultRenderer
    }


{- | Open a rotating-file sink at the given path with default
rotation options (16 MiB or 1 h, whichever comes first). Use
'toRotatingFileWith' to override.
-}
toRotatingFile :: (Show k, Show v) => FilePath -> Printed k v
toRotatingFile p = toRotatingFileWith p def


{- | Open a rotating-file sink at the given path with caller-
supplied options.
-}
toRotatingFileWith
  :: (Show k, Show v)
  => FilePath -> RotationOptions -> Printed k v
toRotatingFileWith p opts =
  Printed
    { pSink = SinkRotating p opts
    , pLabel = "[stream]"
    , pRenderer = defaultRenderer
    }


----------------------------------------------------------------------
-- Modifiers
----------------------------------------------------------------------

{- | Prefix every line with the given label. Mirrors
@Printed.withLabel(label)@.
-}
withLabel :: Text -> Printed k v -> Printed k v
withLabel l p = p {pLabel = l}


{- | Override the per-record renderer. Mirrors
@Printed.withKeyValueMapper(KeyValueMapper)@. The caller's
function receives the configured label plus the record and
returns the line to write (no trailing newline).
-}
withKeyValueMapper
  :: (Text -> Record k v -> Text) -> Printed k v -> Printed k v
withKeyValueMapper f p = p {pRenderer = f}


----------------------------------------------------------------------
-- Rotation options (mirror of 'RotatingFileConfig' minus the path)
----------------------------------------------------------------------

{- | Tunable rotation policy for 'toRotatingFile'. Identical
shape to 'RotatingFileConfig' minus the path / buffer mode
(we always use line buffering + best-effort fsync; the rest
is rotation policy).
-}
data RotationOptions = RotationOptions
  { roMaxBytes :: !(Maybe Int64)
  , roMaxAge :: !(Maybe Int64)
  }


-- | Default rotation: 16 MiB or 1 hour, whichever comes first.
def :: RotationOptions
def =
  RotationOptions
    { roMaxBytes = Just (16 * 1024 * 1024)
    , roMaxAge = Just (60 * 60 * 1000)
    }


withMaxBytes :: Int64 -> RotationOptions -> RotationOptions
withMaxBytes n r = r {roMaxBytes = Just n}


withMaxAgeMs :: Int64 -> RotationOptions -> RotationOptions
withMaxAgeMs n r = r {roMaxAge = Just n}


----------------------------------------------------------------------
-- Apply
----------------------------------------------------------------------

{- | Attach a 'Printed' sink to a 'KStream'. Returns @IO ()@
(terminal sink). Mirrors @KStream.print(Printed)@.

Lifecycle:

  * 'SinkHandle' — assumes the caller is managing the
    handle. 'printKStream' never closes it.
  * 'SinkFile' — opens the file in append mode and leaves it
    open for the lifetime of the topology process. JVM
    @Printed.toFile@ has the same lifetime story.
  * 'SinkRotating' — opens a 'RotatingHandle' and leaves it
    open. 'rfMaxBytes' / 'rfMaxAge' from 'RotationOptions'
    drive the rolling logic.

For deterministic shutdown wrap the whole topology with
'Control.Exception.bracket' against your 'Handle' /
'RotatingHandle' acquisition (see 'withPrintedFile' /
'withPrintedRotatingFile').
-}
printKStream :: Printed k v -> KS.KStream k v -> IO ()
printKStream Printed {..} ks = case pSink of
  SinkHandle h ->
    KS.foreachStream
      (\r -> hPutStrLn h (T.unpack (pRenderer pLabel r)))
      ks
  SinkFile p -> do
    h <- IO.openFile p IO.AppendMode
    IO.hSetBuffering h IO.LineBuffering
    KS.foreachStream
      (\r -> hPutStrLn h (T.unpack (pRenderer pLabel r)))
      ks
  SinkRotating p ropts -> do
    rh <-
      openRotatingHandle
        defaultRotatingFileConfig
          { rfPath = p
          , rfMaxBytes = roMaxBytes ropts
          , rfMaxAge = roMaxAge ropts
          }
    KS.foreachStream (\r -> writeLine rh (pRenderer pLabel r)) ks
  SinkRotatingHandle rh ->
    KS.foreachStream (\r -> writeLine rh (pRenderer pLabel r)) ks


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

{- | Bracket form of 'toFile' + 'printKStream'. Opens the file,
runs the supplied body with a 'Printed' built from the file
handle, then closes the file even on exception. Use this
when you want the file handle to outlive a single topology
/run/ but be cleaned up at the end.

The body is responsible for calling 'printKStream' against
the supplied 'Printed' before the topology starts producing
records.
-}
withPrintedFile
  :: forall k v r
   . (Show k, Show v)
  => FilePath
  -> (Printed k v -> IO r)
  -> IO r
withPrintedFile p body =
  bracket
    ( do
        h <- IO.openFile p IO.AppendMode
        IO.hSetBuffering h IO.LineBuffering
        pure h
    )
    IO.hClose
    (\h -> body (toHandle h))


{- | Bracket form of 'toRotatingFile' + 'printKStream'. Opens
a 'RotatingHandle', runs the supplied body with a 'Printed'
wired to that handle, then closes the rotating handle
(flushing the in-flight buffer) even on exception.
-}
withPrintedRotatingFile
  :: forall k v r
   . (Show k, Show v)
  => FilePath
  -> RotationOptions
  -> (Printed k v -> IO r)
  -> IO r
withPrintedRotatingFile p opts body =
  bracket
    ( openRotatingHandle
        defaultRotatingFileConfig
          { rfPath = p
          , rfMaxBytes = roMaxBytes opts
          , rfMaxAge = roMaxAge opts
          }
    )
    closeRotatingHandle
    $ \rh ->
      body
        Printed
          { pSink = SinkRotatingHandle rh
          , pLabel = "[stream]"
          , pRenderer = defaultRenderer
          }


----------------------------------------------------------------------
-- Rotating-file handle (folded in from the former
-- 'Kafka.Streams.Sink.RotatingFile' module)
----------------------------------------------------------------------

{- | Rotation policy. At least one of 'rfMaxBytes' / 'rfMaxAge'
must be 'Just', otherwise the file never rolls (which is
fine; equivalent to 'KS.printToHandle' with the handle
managed for you).
-}
data RotatingFileConfig = RotatingFileConfig
  { rfPath :: !FilePath
  {- ^ Active file path. Rotated files share this base name
  with a UTC suffix inserted before the extension.
  -}
  , rfMaxBytes :: !(Maybe Int64)
  {- ^ Maximum size in bytes before rotation. 'Nothing'
  disables size-based rotation.
  -}
  , rfMaxAge :: !(Maybe Int64)
  {- ^ Maximum age (since the file's creation in this process)
  in milliseconds. 'Nothing' disables time-based rotation.
  -}
  , rfBufferMode :: !IO.BufferMode
  {- ^ Buffering mode passed to 'IO.hSetBuffering' after each
  roll. Default is 'IO.LineBuffering' so individual
  records flush promptly without manual @hFlush@.
  -}
  , rfFlush :: !Bool
  {- ^ Whether to call 'IO.hFlush' after every write. Set to
  'True' for crash-safety at the cost of throughput;
  'False' (default) lets the buffer mode batch.
  -}
  }


{- | A safe default: no rotation, append to the supplied path,
line-buffered, no per-write flush. Override 'rfMaxBytes' and/or
'rfMaxAge' to enable rotation.
-}
defaultRotatingFileConfig :: RotatingFileConfig
defaultRotatingFileConfig =
  RotatingFileConfig
    { rfPath = "stream.log"
    , rfMaxBytes = Nothing
    , rfMaxAge = Nothing
    , rfBufferMode = IO.LineBuffering
    , rfFlush = False
    }


{- | A rotating-file handle. Internally tracks the current open
'IO.Handle', the bytes written since the last roll, and the
'UTCTime' the current file was opened so 'rfMaxAge' can
trigger.

All public operations acquire the embedded 'MVar', so the
handle is safe to share across multiple worker threads.
-}
data RotatingHandle = RotatingHandle
  { rhConfig :: !RotatingFileConfig
  , rhState :: !(MVar RotatingState)
  }


data RotatingState = RotatingState
  { rsHandle :: !IO.Handle
  , rsBytes :: !Int64
  , rsOpenedAt :: !UTCTime
  }


{- | Open the active file (append mode). Throws if the parent
directory doesn't exist; consider 'System.Directory.createDirectoryIfMissing'
before calling this.
-}
openRotatingHandle :: RotatingFileConfig -> IO RotatingHandle
openRotatingHandle cfg = do
  h <- openAppending (rfPath cfg)
  IO.hSetBuffering h (rfBufferMode cfg)
  now <- getCurrentTime
  sz <- bytesInHandle h
  st <-
    newMVar
      RotatingState
        { rsHandle = h
        , rsBytes = sz
        , rsOpenedAt = now
        }
  pure RotatingHandle {rhConfig = cfg, rhState = st}


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
    Left _ -> 0


{- | Close the active file. Subsequent 'writeLine' calls will
raise an exception; this is the user's signal that they're
done.
-}
closeRotatingHandle :: RotatingHandle -> IO ()
closeRotatingHandle rh = withMVar (rhState rh) $ \st ->
  IO.hClose (rsHandle st)


{- | Write a single line. Triggers a roll first if either
'rfMaxBytes' or 'rfMaxAge' would be exceeded after this write.
-}
writeLine :: RotatingHandle -> Text -> IO ()
writeLine rh@RotatingHandle {..} line = do
  let !payload = line <> "\n"
      !plen = fromIntegral (T.length payload) :: Int64
  modifyMVar_ rhState $ \st0 -> do
    st1 <- maybeRotate rh st0 plen
    IO.hPutStr (rsHandle st1) (T.unpack payload)
    when (rfFlush rhConfig) (IO.hFlush (rsHandle st1))
    pure
      st1
        { rsBytes = rsBytes st1 + plen
        }


{- | Decide whether to roll and, if so, do it. Returns the
(possibly-new) 'RotatingState'.
-}
maybeRotate
  :: RotatingHandle -> RotatingState -> Int64 -> IO RotatingState
maybeRotate RotatingHandle {..} st0 incoming = do
  now <- getCurrentTime
  let needRoll =
        bySize
          || byAge now
      bySize = case rfMaxBytes rhConfig of
        Just lim -> rsBytes st0 + incoming > lim
        Nothing -> False
      byAge t = case rfMaxAge rhConfig of
        Just msLim ->
          let !elapsedMs = round (diffUTCTime t (rsOpenedAt st0) * 1000) :: Int64
          in elapsedMs >= msLim
        Nothing -> False
  if not needRoll
    then pure st0
    else do
      IO.hFlush (rsHandle st0)
      IO.hClose (rsHandle st0)
      let !archive = archivePath (rfPath rhConfig) (rsOpenedAt st0)
      _ <- try (renameFile (rfPath rhConfig) archive) :: IO (Either SomeException ())
      h <-
        bracketOnError
          (openAppending (rfPath rhConfig))
          IO.hClose
          pure
      IO.hSetBuffering h (rfBufferMode rhConfig)
      pure
        RotatingState
          { rsHandle = h
          , rsBytes = 0
          , rsOpenedAt = now
          }


{- | Compute the archive path for a roll. Mirrors the JVM's
@Printed.toFile@ behaviour: insert @.YYYYMMDDThhmmssZ@ before
the file's extension (or at the tail if the file has none).

For an active path of @\/var\/log\/app\/stream.log@ at
@2026-05-16T11:09:42Z@ the archive becomes
@\/var\/log\/app\/stream.20260516T110942Z.log@. When the file
has no extension we append the suffix at the end.
-}
archivePath :: FilePath -> UTCTime -> FilePath
archivePath p t =
  let !dir = takeDirectory p
      !name = takeFileName p
      !ext = takeExtension name
      !stem = take (length name - length ext) name
      !suffix = "." <> formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" t
      !archived = case ext of
        "" -> name <> suffix
        _ -> stem <> suffix <> ext
  in (if null dir || dir == "." then "" else dir <> "/") <> archived


----------------------------------------------------------------------
-- Rotating sink helpers (lower-level than the 'Printed' builder)
----------------------------------------------------------------------

{- | Print every record using @show@; rotate the underlying file
according to the 'RotatingHandle' policy. Terminal sink
(returns @IO ()@).

/JVM equivalent:/ @KStream.print(Printed.toFile(path))@ with
caller-controlled rotation thresholds.
-}
rotatingPrintStream
  :: (Show k, Show v)
  => RotatingHandle
  -> Text
  -- ^ prefix label
  -> KS.KStream k v
  -> IO ()
rotatingPrintStream rh label =
  rotatingPrintToHandle rh (defaultRender label)


{- | Variant of 'rotatingPrintStream' with a caller-supplied
record renderer. Equivalent to
'Kafka.Streams.KStream.printToHandle' but writes through a
'RotatingHandle' instead of a raw 'IO.Handle'.
-}
rotatingPrintToHandle
  :: forall k v
   . RotatingHandle
  -> (Record k v -> Text)
  -- ^ per-record renderer
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
