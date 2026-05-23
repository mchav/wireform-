{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module Wireform.Parser.Driver
  ( runParser
  , runParserLoop
  , LoopControl (..)
  , parseByteString
  , runParserInternal
  , InternalResult (..)
  ) where

import Control.Exception (SomeException, bracket, mask)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr (..), plusPtr, minusPtr, castPtr)
import Foreign.Storable (poke)
import GHC.Exts
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (..))
import GHC.IO (IO (..))
import System.IO.Unsafe (unsafeDupablePerformIO)

import Wireform.Parser.Error
import Wireform.Parser.Internal
import Wireform.Ring.Internal (ringBase, ringSize, ringMask)
import Wireform.Transport

data LoopControl = Continue | Stop
  deriving stock (Eq, Show)

data InternalResult e a
  = IRDone {-# UNPACK #-} !Word64 !a
  | IRFail !Word64
  | IRErr !Word64 !e
  | IRUnexpectedEof !Word64 !Int
  | IRTransportError !SomeException
  | IRCleanEof
  | IRRingOverflow {-# UNPACK #-} !Word64 {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    -- ^ position, requested bytes, ring size — see 'ParseRingOverflow'.

data TransportState
  = TSOpen
  | TSClosedEof
  | TSClosedErr !SomeException

------------------------------------------------------------------------
-- runParser
------------------------------------------------------------------------

runParser :: forall e a. Transport -> Parser Stream e a -> IO (Either (ParseError e) a)
runParser t p = do
  startPos <- transportLoadHead t
  ir <- runParserInternal t p startPos
  pure $ case ir of
    IRDone _ a              -> Right a
    IRFail pos              -> Left (ParseFail pos)
    IRErr pos e             -> Left (ParseErr pos e)
    IRUnexpectedEof pos n   -> Left (ParseUnexpectedEof pos n)
    IRTransportError exc    -> Left (ParseTransportError exc)
    IRCleanEof              -> Left (ParseUnexpectedEof 0 0)
    IRRingOverflow pos n sz -> Left (ParseRingOverflow pos n sz)

runParserInternal :: forall e a. Transport -> Parser Stream e a -> Word64 -> IO (InternalResult e a)
runParserInternal t p startPos = mask \restore -> do
  let ring  = transportRing t
      !base = ringBase ring
      !msk  = ringMask ring
      !sz   = ringSize ring

  currentHead <- transportLoadHead t
  let !curOffset = fromIntegral startPos .&. msk
      !(Ptr initCur#) = base `plusPtr` curOffset
      -- See StepCheckpoint / StepSuspend for the rationale on
      -- computing eob from @cur + avail@ rather than
      -- @base + (head .&. msk)@: when @head - startPos == ringSize@,
      -- the masked offsets coincide and eob collapses onto cur.
      !avail     = fromIntegral (currentHead - startPos) :: Int
      !(Ptr initEnd#) = (Ptr initCur#) `plusPtr` avail

  -- One bracketed buffer holds the three mutable cells the parser
  -- shares with the driver: end pointer, anchor pos, anchor cur.
  bracket (mallocBytes 24) free \cells -> do
    let !endPtr     = cells
        !anchorPos  = cells `plusPtr` 8
        !anchorCur  = cells `plusPtr` 16
    poke (castPtr endPtr    :: Ptr (Ptr Word8)) (Ptr initEnd#)
    poke (castPtr anchorPos :: Ptr Word64)      startPos
    poke (castPtr anchorCur :: Ptr (Ptr Word8)) (Ptr initCur#)

    highWaterRef <- newIORef startPos
    tsRef <- newIORef TSOpen

    -- Allocate tag and construct env inside IO so the tag
    -- is available for peTag, then return both.
    (env, step0) <- IO \s0 -> case newPromptTag# s0 of
      (# s1, (tag :: PromptTag# (Step e a)) #) ->
        let !env' = ParserEnv
              { peEndPtr    = castPtr endPtr
              , peBaseAddr  = base
              , peMask      = msk
              , peAnchorPos = castPtr anchorPos
              , peAnchorCur = castPtr anchorCur
              , peBackingFp = FinalPtr
              , peTag       = tagToAny tag
              }
            body :: State# RealWorld -> (# State# RealWorld, Step e a #)
            body s = case runParser# p env' initEnd# initCur# s of
              (# s', OK# a cur' #) ->
                case curToPos env' cur' s' of
                  (# s'', newPos #) -> (# s'', StepDone newPos a #)
              (# s', Fail# #) ->
                (# s', StepFail startPos #)
              (# s', Err# e #) ->
                (# s', StepErr startPos e #)
        in case prompt# tag body s1 of
             (# s2, step #) -> (# s2, (env', step) #)

    driverLoop restore t env base msk sz startPos highWaterRef tsRef step0

driverLoop :: forall e a.
              (forall x. IO x -> IO x) -> Transport -> ParserEnv
           -> Ptr Word8 -> Int -> Int
           -> Word64 -> IORef Word64 -> IORef TransportState
           -> Step e a -> IO (InternalResult e a)
driverLoop restore t env base msk sz startPos hwRef tsRef = go
  where
    go :: Step e a -> IO (InternalResult e a)
    go step = case step of
      StepDone newPos a -> do
        transportAdvanceTail t newPos
        pure (IRDone newPos a)

      StepErr pos e -> pure (IRErr pos e)

      StepFail pos -> do
        ts <- readIORef tsRef
        hw <- readIORef hwRef
        pure $ case ts of
          TSOpen        -> IRFail pos
          TSClosedErr e -> IRTransportError e
          TSClosedEof
            | hw == startPos -> IRCleanEof
            | otherwise      -> IRUnexpectedEof pos 0

      StepCheckpoint pos resume -> do
        transportAdvanceTail t pos
        h <- transportLoadHead t
        -- Compute eob relative to the new cur so that a wrap (where
        -- @pos .&. msk == h .&. msk@ because exactly a ring's worth
        -- of bytes are in flight) does not collapse eob onto cur and
        -- make the parser see zero bytes when there are actually
        -- ringSize bytes available.  The double mapping guarantees
        -- @newCur + avail@ is addressable for any @avail <= ringSize@.
        -- 'resumeContinue' re-anchors the env so 'curToPos' stays
        -- correct after the cur wrap.
        let !curOff = fromIntegral pos .&. msk
            !newCur = base `plusPtr` curOff
            !avail  = fromIntegral (h - pos) :: Int
            !newEnd = newCur `plusPtr` avail
        nextStep <- resumeContinue resume newCur newEnd
        go nextStep

      StepSuspend pausedAt needed resume
        | needed > sz ->
            -- The parser asked for more bytes than the entire ring can
            -- ever hold.  No amount of refilling will satisfy this;
            -- waiting would deadlock (producer can't make room because
            -- the consumer is suspended waiting for it).  Fail loudly
            -- instead.
            pure (IRRingOverflow pausedAt needed sz)
        | otherwise -> do
            modifyIORef' hwRef (max pausedAt)
            result <- restore (waitUntilAvailable t tsRef pausedAt needed sz)
            case result of
              WAMoreData newHead -> do
                modifyIORef' hwRef (max newHead)
                -- See the comment on StepCheckpoint above: compute eob
                -- as @newCur + (newHead - pausedAt)@ rather than as
                -- @base + (newHead .&. msk)@.  The latter collapses to
                -- @newCur@ when the producer has filled exactly one
                -- ring-worth of bytes since @pausedAt@, hiding all the
                -- newly-available bytes from the parser.
                -- 'resumeContinue' re-anchors the env so 'curToPos'
                -- stays correct after the cur wrap.
                let !newCurOff = fromIntegral pausedAt .&. msk
                    !newCur    = base `plusPtr` newCurOff
                    !avail     = fromIntegral (newHead - pausedAt) :: Int
                    !newEnd    = newCur `plusPtr` avail
                nextStep <- resumeContinue resume newCur newEnd
                go nextStep
              WAEndOfInput -> do
                nextStep <- resumeEof resume
                go nextStep
              WATransportError exc ->
                pure (IRTransportError exc)

data WaitAvail
  = WAMoreData {-# UNPACK #-} !Word64
  | WAEndOfInput
  | WATransportError !SomeException

waitUntilAvailable :: Transport -> IORef TransportState
                   -> Word64 -> Int -> Int -> IO WaitAvail
waitUntilAvailable t tsRef pos needed _ringSize = do
  h0 <- transportLoadHead t
  loop (max pos h0)
  where
    -- 'waitFrom' is the position we ask the transport to advance past.
    -- It advances on each iteration so a transport that delivers data
    -- in dribs (a chunked feeder, a TLS layer that yields partial
    -- records, an io_uring SQ that hands us one CQE at a time)
    -- doesn't spin: each 'transportWaitData' is asked to wait /past/
    -- the head we already observed, so it must either pull fresh
    -- bytes or report EndOfInput.
    loop !waitFrom = do
      h <- transportLoadHead t
      if h - pos >= fromIntegral needed
        then pure (WAMoreData h)
        else do
          r <- transportWaitData t waitFrom
          case r of
            MoreData h'    -> loop (max waitFrom h')
            EndOfInput     -> do { writeIORef tsRef TSClosedEof; pure WAEndOfInput }
            TransportError exc -> do { writeIORef tsRef (TSClosedErr exc); pure (WATransportError exc) }

------------------------------------------------------------------------
-- runParserLoop
------------------------------------------------------------------------

runParserLoop :: forall e a. Transport -> Parser Stream e a -> (a -> IO LoopControl)
              -> IO (Either (ParseError e) ())
runParserLoop t p k = do
  startPos <- transportLoadHead t
  loop startPos
  where
    loop pos = do
      r <- runParserInternal t p pos
      case r of
        IRDone newPos a -> do
          ctl <- k a
          case ctl of { Continue -> loop newPos; Stop -> pure (Right ()) }
        IRCleanEof               -> pure (Right ())
        IRFail fpos              -> pure (Left (ParseFail fpos))
        IRErr fpos e             -> pure (Left (ParseErr fpos e))
        IRUnexpectedEof fpos n   -> pure (Left (ParseUnexpectedEof fpos n))
        IRTransportError exc     -> pure (Left (ParseTransportError exc))
        IRRingOverflow fpos n sz -> pure (Left (ParseRingOverflow fpos n sz))

------------------------------------------------------------------------
-- parseByteString (non-streaming, flatparse-equivalent)
------------------------------------------------------------------------

-- | Run a parser against a whole 'ByteString'.
-- The hot path is bit-identical to flatparse — no suspension overhead.
parseByteString :: forall e a. Parser Pure e a -> ByteString -> Either (ParseError e) a
parseByteString p b = unsafeDupablePerformIO $ do
  -- withForeignPtr keeps the ByteString's backing memory alive
  let !(BSI.BS (ForeignPtr buf# fp) (I# len#)) = b
      !end# = plusAddr# buf# len#

  withForeignPtr (ForeignPtr buf# fp) \_ ->
    bracket (mallocBytes 24) free \cells -> do
      let !endPtr    = cells
          !anchorPos = cells `plusPtr` 8
          !anchorCur = cells `plusPtr` 16
      poke (castPtr endPtr    :: Ptr (Ptr Word8)) (Ptr end#)
      poke (castPtr anchorPos :: Ptr Word64)      0
      poke (castPtr anchorCur :: Ptr (Ptr Word8)) (Ptr buf#)

      IO \s0 -> case newPromptTag# s0 of
        (# s1, (tag :: PromptTag# (Step e a)) #) ->
          let env = ParserEnv
                { peEndPtr    = castPtr endPtr
                , peBaseAddr  = Ptr buf#
                , peMask      = maxBound
                , peAnchorPos = castPtr anchorPos
                , peAnchorCur = castPtr anchorCur
                , peBackingFp = fp
                , peTag       = tagToAny tag
                }
              body :: State# RealWorld -> (# State# RealWorld, Step e a #)
              body s = case runParser# p env end# buf# s of
                (# s', OK# a cur' #) ->
                  let !pos = fromIntegral (I# (minusAddr# cur' buf#))
                  in (# s', StepDone pos a #)
                (# s', Fail# #) -> (# s', StepFail 0 #)
                (# s', Err# e #) -> (# s', StepErr 0 e #)
          in case prompt# tag body s1 of
               (# s2, step #) -> unIO (classifyStep step) s2
  where
    classifyStep (StepDone _ a)       = pure (Right a)
    classifyStep (StepFail pos)       = pure (Left (ParseFail pos))
    classifyStep (StepErr pos e)      = pure (Left (ParseErr pos e))
    classifyStep (StepSuspend _ _ r)  = resumeEof r >>= classifyStep
    classifyStep (StepCheckpoint _ r) = resumeEof r >>= classifyStep
    unIO (IO f) = f
