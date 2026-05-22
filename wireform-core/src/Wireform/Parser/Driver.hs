{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wireform.Parser.Driver
  ( runParser
  , runParserLoop
  , LoopControl (..)
  , parseByteString
  , runParserInternal
  , InternalResult (..)
  ) where

import Control.Exception (SomeException, mask)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import GHC.Exts (PromptTag#, newPromptTag#, prompt#, State#, RealWorld)
import GHC.IO (IO (..))

import Wireform.Parser.Error
import Wireform.Parser.Internal
import Wireform.Ring.Internal (MagicRing, ringBase, ringSize, ringMask)
import Wireform.Transport

data LoopControl = Continue | Stop
  deriving stock (Eq, Show)

------------------------------------------------------------------------
-- Internal result
------------------------------------------------------------------------

data InternalResult e a
  = IRDone {-# UNPACK #-} !Word64 !a
    -- ^ Succeeded; carries the position AFTER the consumed input.
  | IRFail !Word64
  | IRErr !Word64 !e
  | IRUnexpectedEof !Word64 !Int
  | IRTransportError !SomeException
  | IRCleanEof

data TransportState
  = TSOpen
  | TSClosedEof
  | TSClosedErr !SomeException

------------------------------------------------------------------------
-- runParser
------------------------------------------------------------------------

runParser :: forall e a. Transport -> Parser e a -> IO (Either (ParseError e) a)
runParser t p = do
  startPos <- transportLoadHead t
  ir <- runParserInternal t p startPos
  pure $ case ir of
    IRDone _ a            -> Right a
    IRFail pos            -> Left (ParseFail pos)
    IRErr pos e           -> Left (ParseErr pos e)
    IRUnexpectedEof pos n -> Left (ParseUnexpectedEof pos n)
    IRTransportError exc  -> Left (ParseTransportError exc)
    IRCleanEof            -> Left (ParseUnexpectedEof 0 0)

runParserInternal :: forall e a. Transport -> Parser e a -> Word64 -> IO (InternalResult e a)
runParserInternal t p startPos = mask \restore -> do
  let ring  = transportRing t
      !base = ringBase ring
      !msk  = ringMask ring
      !sz   = ringSize ring

  currentHead <- transportLoadHead t
  let !curOffset = fromIntegral startPos .&. msk
      !initCur   = base `plusPtr` curOffset
      -- Set initEnd to reflect data already available in the ring.
      -- If currentHead > startPos, data from a previous recv is present.
      !endOffset = fromIntegral currentHead .&. msk
      !initEnd   = base `plusPtr` endOffset

  endRef <- newIORef initEnd
  highWaterRef <- newIORef startPos
  tsRef <- newIORef TSOpen

  let env = ParserEnv
        { peEndRef   = endRef
        , peBaseAddr = base
        , peMask     = msk
        , peStartPos = startPos
        , peInitCur  = initCur
        }

  -- Allocate prompt tag and run parser inside a prompt frame
  step0 <- IO \s0 -> case newPromptTag# s0 of
    (# s1, (tag :: PromptTag# (Step e a)) #) ->
      let body :: State# RealWorld -> (# State# RealWorld, Step e a #)
          body s = case unIO (unParser p tag env initCur) s of
            (# s', res #) -> case res of
              OK a newCur ->
                let !newPos = startPos + fromIntegral (newCur `minusPtr` initCur)
                in (# s', StepDone newPos a #)
              Fail    -> (# s', StepFail startPos #)
              Err e   -> (# s', StepErr startPos e #)
      in prompt# tag body s1

  driverLoop restore t env base msk sz startPos highWaterRef tsRef step0

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f
{-# INLINE unIO #-}

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

      StepErr pos e ->
        pure (IRErr pos e)

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
        let !curOffset = fromIntegral pos .&. msk
            !newCur    = base `plusPtr` curOffset
        end <- readIORef (peEndRef env)
        nextStep <- resumeContinue resume newCur end
        go nextStep

      StepSuspend pausedAt needed resume -> do
        modifyIORef' hwRef (max pausedAt)
        result <- restore (waitUntilAvailable t tsRef pausedAt needed sz)
        case result of
          WAMoreData newHead -> do
            modifyIORef' hwRef (max newHead)
            let !newEndOff = fromIntegral newHead .&. msk
                !newEnd    = base `plusPtr` newEndOff
                !newCurOff = fromIntegral pausedAt .&. msk
                !newCur    = base `plusPtr` newCurOff
            writeIORef (peEndRef env) newEnd
            nextStep <- resumeContinue resume newCur newEnd
            go nextStep

          WAEndOfInput -> do
            nextStep <- resumeEof resume
            go nextStep

          WATransportError exc ->
            pure (IRTransportError exc)

------------------------------------------------------------------------
-- waitUntilAvailable
------------------------------------------------------------------------

data WaitAvail
  = WAMoreData {-# UNPACK #-} !Word64
  | WAEndOfInput
  | WATransportError !SomeException

waitUntilAvailable :: Transport -> IORef TransportState
                   -> Word64 -> Int -> Int -> IO WaitAvail
waitUntilAvailable t tsRef pos needed _ringSize = loop
  where
    loop = do
      h <- transportLoadHead t
      if h - pos >= fromIntegral needed
        then pure (WAMoreData h)
        else do
          r <- transportWaitData t pos
          case r of
            MoreData _ -> loop
            EndOfInput -> do
              writeIORef tsRef TSClosedEof
              pure WAEndOfInput
            TransportError exc -> do
              writeIORef tsRef (TSClosedErr exc)
              pure (WATransportError exc)

------------------------------------------------------------------------
-- runParserLoop
------------------------------------------------------------------------

runParserLoop :: forall e a. Transport -> Parser e a -> (a -> IO LoopControl)
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
          case ctl of
            Continue -> loop newPos
            Stop     -> pure (Right ())
        IRCleanEof     -> pure (Right ())
        IRFail fpos    -> pure (Left (ParseFail fpos))
        IRErr fpos e   -> pure (Left (ParseErr fpos e))
        IRUnexpectedEof fpos n -> pure (Left (ParseUnexpectedEof fpos n))
        IRTransportError exc   -> pure (Left (ParseTransportError exc))

------------------------------------------------------------------------
-- parseByteString
------------------------------------------------------------------------

parseByteString :: forall e a. Parser e a -> ByteString -> Either (ParseError e) a
parseByteString p bs = BSI.accursedUnutterablePerformIO $ do
  let (fptr, off, len) = BSI.toForeignPtr bs
  withForeignPtr fptr \basePtr -> do
    let !start = basePtr `plusPtr` off
        !end   = start `plusPtr` len

    endRef <- newIORef end

    let env = ParserEnv
          { peEndRef   = endRef
          , peBaseAddr = start
          , peMask     = maxBound
          , peStartPos = 0
          , peInitCur  = start
          }

    step0 <- IO \s0 -> case newPromptTag# s0 of
      (# s1, (tag :: PromptTag# (Step e a)) #) ->
        let body :: State# RealWorld -> (# State# RealWorld, Step e a #)
            body s = case unIO (unParser p tag env start) s of
              (# s', res #) -> case res of
                OK a newCur ->
                  let !pos = fromIntegral (newCur `minusPtr` start)
                  in (# s', StepDone pos a #)
                Fail    -> (# s', StepFail 0 #)
                Err e   -> (# s', StepErr 0 e #)

        in prompt# tag body s1

    -- For ByteString parsing, any suspend is answered with EOF
    -- (there is no more data). Loop until we get a terminal step.
    let go step = case step of
          StepDone _ a    -> pure (Right a)
          StepFail pos    -> pure (Left (ParseFail pos))
          StepErr pos e   -> pure (Left (ParseErr pos e))
          StepSuspend _ _ resume -> do
            nextStep <- resumeEof resume
            go nextStep
          StepCheckpoint _ resume -> do
            nextStep <- resumeContinue resume start end
            go nextStep

    go step0
