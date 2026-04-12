{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
-- | Incremental and concurrent SAX parser.
--
-- The incremental parser ('feedChunk' \/ 'feedEnd') allows pushing chunks of
-- XML as they arrive (e.g. from a network socket).
--
-- The concurrent parser ('withConcurrentParse') runs the SAX parser in a
-- background thread, emitting events into a bounded 'TBQueue'.  The consumer
-- processes events as they arrive, overlapping parsing and processing – critical
-- for large (multi-gigabyte) documents.
module XML.Incremental
  ( -- * Incremental SAX parser (single-threaded, chunk-at-a-time)
    IncrementalParser
  , newParser
  , feedChunk
  , feedEnd
  , ParserResult(..)

    -- * Concurrent SAX parser (parser thread + consumer thread)
  , withConcurrentParse
  , withConcurrentParseBS

    -- * Streaming fold (concurrent, constant memory)
  , streamFold
  , streamFoldIO

    -- * Channel-based (low-level)
  , parseToChan
  , SAXChan
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (SomeException, bracket, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef
import Data.Vector (Vector)
import qualified Data.Vector as V

import XML.SAX (SAXEvent(..), parseSAX, parseSAXStream)

------------------------------------------------------------------------
-- Part 1: Incremental (chunk-at-a-time) parser
------------------------------------------------------------------------

data ScanMode
  = SText
  | SMarkup
  | SAttrDQ
  | SAttrSQ
  | SComment
  | SCDATA
  | SPI
  deriving (Eq, Show)

-- | Opaque incremental parser state.
data IncrementalParser = IncrementalParser
  { ipBuffer    :: !(IORef ByteString)
  , ipPrevCount :: !(IORef Int)
  , ipScanMode  :: !(IORef ScanMode)
  , ipScanPos   :: !(IORef Int)
  , ipSafeBound :: !(IORef Int)
  }

data ParserResult
  = MoreEvents !(Vector SAXEvent)
  | ParseDone
  | ParseError !String
  deriving (Show, Eq)

-- | Create a new incremental parser.
newParser :: IO IncrementalParser
newParser = IncrementalParser
  <$> newIORef BS.empty
  <*> newIORef 0
  <*> newIORef SText
  <*> newIORef 0
  <*> newIORef 0

-- | Scan bytes to find the last position where a complete XML token ended.
--
-- Returns @(safeBound, finalMode, resumePos)@ where @safeBound@ is the byte
-- offset just past the last @>@ that closed a tag\/comment\/CDATA\/PI, and
-- @resumePos@ is where scanning should resume on the next call.
scanForBoundary :: ByteString -> Int -> ScanMode -> Int -> (Int, ScanMode, Int)
scanForBoundary !bs !startOff !startMode !lastSafe = go startOff startMode lastSafe
  where
    !len = BS.length bs
    go :: Int -> ScanMode -> Int -> (Int, ScanMode, Int)
    go !i !mode !safe
      | i >= len = (safe, mode, i)
      | otherwise =
          let !b = BSU.unsafeIndex bs i
          in case mode of
            SText
              | b == 0x3C ->
                  if i + 1 >= len
                    then (safe, SText, i)
                    else let !b2 = BSU.unsafeIndex bs (i + 1) in
                      case b2 of
                        0x21 ->
                          if i + 3 >= len
                            then (safe, SText, i)
                            else if BSU.unsafeIndex bs (i + 2) == 0x2D
                                 && BSU.unsafeIndex bs (i + 3) == 0x2D
                              then go (i + 4) SComment safe
                              else if BSU.unsafeIndex bs (i + 2) == 0x5B
                                then go (i + 3) SCDATA safe
                                else go (i + 2) SMarkup safe
                        0x3F -> go (i + 2) SPI safe
                        _    -> go (i + 1) SMarkup safe
              | otherwise -> go (i + 1) SText safe
            SMarkup
              | b == 0x3E -> go (i + 1) SText (i + 1)
              | b == 0x22 -> go (i + 1) SAttrDQ safe
              | b == 0x27 -> go (i + 1) SAttrSQ safe
              | otherwise -> go (i + 1) SMarkup safe
            SAttrDQ
              | b == 0x22 -> go (i + 1) SMarkup safe
              | otherwise -> go (i + 1) SAttrDQ safe
            SAttrSQ
              | b == 0x27 -> go (i + 1) SMarkup safe
              | otherwise -> go (i + 1) SAttrSQ safe
            SComment
              | b == 0x2D ->
                  if i + 2 >= len
                    then (safe, SComment, i)
                    else if BSU.unsafeIndex bs (i + 1) == 0x2D
                         && BSU.unsafeIndex bs (i + 2) == 0x3E
                      then go (i + 3) SText (i + 3)
                      else go (i + 1) SComment safe
              | otherwise -> go (i + 1) SComment safe
            SCDATA
              | b == 0x5D ->
                  if i + 2 >= len
                    then (safe, SCDATA, i)
                    else if BSU.unsafeIndex bs (i + 1) == 0x5D
                         && BSU.unsafeIndex bs (i + 2) == 0x3E
                      then go (i + 3) SText (i + 3)
                      else go (i + 1) SCDATA safe
              | otherwise -> go (i + 1) SCDATA safe
            SPI
              | b == 0x3F ->
                  if i + 1 >= len
                    then (safe, SPI, i)
                    else if BSU.unsafeIndex bs (i + 1) == 0x3E
                      then go (i + 2) SText (i + 2)
                      else go (i + 1) SPI safe
              | otherwise -> go (i + 1) SPI safe

-- | Feed a chunk of bytes.  Returns any complete SAX events parsed so far.
--
-- Internally, the parser maintains a byte-level scanner that identifies safe
-- parse boundaries (positions where a complete XML token just ended).  Only
-- the prefix up to the last safe boundary is handed to the SAX engine, so
-- text content that might be split across chunks is never emitted prematurely.
feedChunk :: IncrementalParser -> ByteString -> IO (Vector SAXEvent)
feedChunk ip chunk
  | BS.null chunk = pure V.empty
  | otherwise = do
      oldBuf <- readIORef (ipBuffer ip)
      let !newBuf = oldBuf <> chunk
      writeIORef (ipBuffer ip) newBuf

      oldMode    <- readIORef (ipScanMode ip)
      oldScanPos <- readIORef (ipScanPos ip)
      oldSafe    <- readIORef (ipSafeBound ip)

      let (!newSafe, !newMode, !newScanPos) =
            scanForBoundary newBuf oldScanPos oldMode oldSafe
      writeIORef (ipScanMode ip) newMode
      writeIORef (ipScanPos ip) newScanPos
      writeIORef (ipSafeBound ip) newSafe

      if newSafe <= 0 || newSafe == oldSafe
        then pure V.empty
        else do
          prevCount <- readIORef (ipPrevCount ip)
          let !parseBytes = BS.take newSafe newBuf
          eventsRef <- newIORef []
          _ <- parseSAXStream parseBytes (\ev -> modifyIORef' eventsRef (ev :))
          allEvents <- V.fromList . reverse <$> readIORef eventsRef

          let !totalEvents = V.length allEvents
          if totalEvents > prevCount
            then do
              writeIORef (ipPrevCount ip) totalEvents
              pure (V.drop prevCount allEvents)
            else pure V.empty

-- | Signal end of input.  Returns final events and any error.
--
-- If the accumulated bytes form a well-formed XML document, returns
-- @Right remainingEvents@.  Otherwise returns @Left errorMessage@.
feedEnd :: IncrementalParser -> IO (Either String (Vector SAXEvent))
feedEnd ip = do
  buf <- readIORef (ipBuffer ip)
  prevCount <- readIORef (ipPrevCount ip)

  if BS.null buf
    then pure (Right V.empty)
    else case parseSAX buf of
      Left err -> pure (Left err)
      Right allEvents ->
        pure (Right (V.drop prevCount allEvents))

------------------------------------------------------------------------
-- Part 2: Concurrent SAX parser
------------------------------------------------------------------------

-- | Bounded channel carrying SAX events.
-- 'Nothing' signals end-of-stream.  @Just (Left err)@ signals a parse error.
type SAXChan = TBQueue (Maybe (Either String SAXEvent))

-- | Parse a 'ByteString' concurrently: the parser runs in a background thread
-- emitting events to a bounded queue.  The callback processes events as they
-- arrive.  Parser and processor run concurrently.
--
-- The bounded queue provides backpressure: if the consumer is slow, the parser
-- blocks until the consumer catches up.
withConcurrentParse
  :: ByteString
  -> Int                  -- ^ Queue size (e.g. 256)
  -> (SAXEvent -> IO ())  -- ^ Event handler (runs in caller's thread)
  -> IO (Either String ())
withConcurrentParse bs queueSize handler = do
  chan <- newTBQueueIO (fromIntegral queueSize)
  bracket
    (forkIO (producerThread chan bs))
    killThread
    (\_ -> consumerLoop chan handler)

-- | Same as 'withConcurrentParse' but accepts a list of strict 'ByteString'
-- chunks (e.g. from 'Data.ByteString.Lazy.toChunks').
withConcurrentParseBS
  :: [ByteString]
  -> Int
  -> (SAXEvent -> IO ())
  -> IO (Either String ())
withConcurrentParseBS chunks queueSize handler =
  withConcurrentParse (BS.concat chunks) queueSize handler

producerThread :: SAXChan -> ByteString -> IO ()
producerThread chan bs = do
  result <- try $ parseSAXStream bs $ \event ->
    atomically $ writeTBQueue chan (Just (Right event))
  case result of
    Left (e :: SomeException) ->
      atomically $ writeTBQueue chan (Just (Left (show e)))
    Right (Left err) ->
      atomically $ writeTBQueue chan (Just (Left err))
    Right (Right ()) -> pure ()
  atomically $ writeTBQueue chan Nothing

consumerLoop :: SAXChan -> (SAXEvent -> IO ()) -> IO (Either String ())
consumerLoop chan handler = go
  where
    go = do
      item <- atomically $ readTBQueue chan
      case item of
        Nothing            -> pure (Right ())
        Just (Left err)    -> pure (Left err)
        Just (Right event) -> handler event >> go

------------------------------------------------------------------------
-- Part 3: Streaming fold (concurrent, constant memory)
------------------------------------------------------------------------

-- | Fold over SAX events concurrently.  The parser runs in a background
-- thread while the fold runs in the main thread.  Memory usage is constant
-- (bounded queue + accumulator).
streamFold
  :: ByteString
  -> Int                    -- ^ Queue size
  -> a                      -- ^ Initial accumulator
  -> (a -> SAXEvent -> a)   -- ^ Pure fold step
  -> IO (Either String a)
streamFold bs queueSize z f = do
  ref <- newIORef z
  result <- withConcurrentParse bs queueSize $ \event ->
    modifyIORef' ref (`f` event)
  case result of
    Left err -> pure (Left err)
    Right () -> Right <$> readIORef ref

-- | IO variant where the fold step can perform IO.
streamFoldIO
  :: ByteString
  -> Int
  -> a
  -> (a -> SAXEvent -> IO a)
  -> IO (Either String a)
streamFoldIO bs queueSize z f = do
  ref <- newIORef z
  result <- withConcurrentParse bs queueSize $ \event -> do
    acc <- readIORef ref
    acc' <- f acc event
    writeIORef ref acc'
  case result of
    Left err -> pure (Left err)
    Right () -> Right <$> readIORef ref

------------------------------------------------------------------------
-- Part 4: Low-level channel API
------------------------------------------------------------------------

-- | Start a parser that writes to a 'TBQueue'.  Returns the channel and
-- the thread ID.  The caller is responsible for reading the queue and killing
-- the thread.
parseToChan :: ByteString -> Int -> IO (SAXChan, ThreadId)
parseToChan bs queueSize = do
  chan <- newTBQueueIO (fromIntegral queueSize)
  tid <- forkIO (producerThread chan bs)
  pure (chan, tid)
