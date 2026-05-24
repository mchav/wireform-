{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the wireform-parser-based Kafka response frame
-- reader ('Kafka.Network.FrameParser').
module Network.FrameParserSpec (tests) where

import qualified Data.Binary.Put as BP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Int (Int32)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)

import Wireform.Network (chunkedReceiveFn, withReceiveBufTransport)
import Wireform.Parser.Driver (LoopControl (..))
import Wireform.Transport.Config (defaultTransportConfig)

import qualified Kafka.Network.FrameParser as FP

-- | Build a single Kafka response frame: [4-byte BE length]
-- [4-byte BE correlation id] [body].  Length covers id+body.
frameBytes :: Int32 -> BS.ByteString -> BS.ByteString
frameBytes cid body =
  let !payload = BL.toStrict $ BP.runPut $ do
        BP.putInt32be cid
        BP.putByteString body
      !lenHdr = BL.toStrict $ BP.runPut $
        BP.putInt32be (fromIntegral (BS.length payload))
  in lenHdr <> payload

tests :: TestTree
tests = testGroup "Kafka.Network.FrameParser"
  [ testCase "reads a single frame off the magic ring" $ do
      let bs = frameBytes 7 "hello"
      recvFn <- chunkedReceiveFn [bs]
      got <- newIORef Nothing
      r <- withReceiveBufTransport defaultTransportConfig recvFn $ \t ->
        FP.runKafkaFrameLoop t $ \(cid, body) -> do
          -- 'takeBs' hands back a slice of the magic-ring memory
          -- which becomes invalid as soon as 'withReceiveBufTransport'
          -- tears the ring down.  Force the copy now via a bang so
          -- the IORef holds a heap-allocated bytestring that
          -- outlives the transport scope.
          let !bodyCopy = BS.copy body
          writeIORef got (Just (cid, bodyCopy))
          pure Stop
      case r of
        Right () -> pure ()
        Left e   -> assertFailure ("loop error: " <> show e)
      Just (cid, body) <- readIORef got
      cid  @?= 7
      body @?= "hello"

  , testCase "reads two frames split across recv chunks" $ do
      let f1 = frameBytes 11 "abc"
          f2 = frameBytes 22 "defg"
          combined = f1 <> f2
          (l, r)   = BS.splitAt (BS.length f1 + 4) combined
      recvFn <- chunkedReceiveFn [l, r]
      acc <- newIORef ([] :: [(Int32, BS.ByteString)])
      _ <- withReceiveBufTransport defaultTransportConfig recvFn $ \t ->
        FP.runKafkaFrameLoop t $ \(cid, body) -> do
          let !bodyCopy = BS.copy body
          modifyIORef acc ((cid, bodyCopy) :)
          xs <- readIORef acc
          pure (if length xs >= 2 then Stop else Continue)
      observed <- reverse <$> readIORef acc
      observed @?= [(11, "abc"), (22, "defg")]

  , testCase "raises FrameTooShort on undersized length prefix" $ do
      let !payload = BL.toStrict $ BP.runPut $ BP.putInt32be 2  -- < 4
          !len     = BS.length payload
          _ = len
      recvFn <- chunkedReceiveFn [payload]
      r <- withReceiveBufTransport defaultTransportConfig recvFn $ \t ->
        FP.runKafkaFrameLoop t $ \_ -> pure Continue
      case r of
        Left _ -> pure ()
        Right _ -> assertFailure "expected FrameTooShort"
  ]
