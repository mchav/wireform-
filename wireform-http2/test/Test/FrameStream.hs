{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Test.FrameStream (tests) where

import qualified Data.ByteString as BS
import Data.IORef
import Test.Tasty
import Test.Tasty.HUnit

import Wireform.Network (chunkedRecvFn, withRecvBufTransport)
import Wireform.Parser.Driver (LoopControl (..))
import Wireform.Transport.Config (defaultTransportConfig)

import Network.HTTP2.Frame
  ( Frame (..)
  , FrameHeader (..)
  , FramePayload (..)
  , encodeFrame
  , encodeFrameHeader
  , flagEndStream
  )
import Network.HTTP2.Frame.Stream (FrameStreamError (..), runFrameLoop)
import Network.HTTP2.Types (FrameType (..))

tests :: TestTree
tests = testGroup "FrameStream"
  [ testCase "decodes a single DATA frame" $ do
      let payload = "hello"
          hdr     = FrameHeader (fromIntegral (BS.length payload)) FrameData 0 1
          bs      = encodeFrameHeader hdr <> payload
      ref <- newIORef Nothing
      recvFn <- chunkedRecvFn [bs]
      r <- withRecvBufTransport defaultTransportConfig recvFn $ \t ->
        runFrameLoop t $ \fr -> do
          writeIORef ref (Just fr)
          pure Stop
      case r of
        Right () -> pure ()
        Left e   -> assertFailure ("loop error: " <> show e)
      Just fr <- readIORef ref
      fhStreamId (frameHeader fr) @?= 1
      fhType     (frameHeader fr) @?= FrameData
      fhLength   (frameHeader fr) @?= fromIntegral (BS.length payload)

  , testCase "decodes back-to-back frames split mid-header" $ do
      let dataPayload = "world"
          dataHdr     = FrameHeader (fromIntegral (BS.length dataPayload))
                                    FrameData flagEndStream 3
          dataBs      = encodeFrameHeader dataHdr <> dataPayload
          pingBs      = encodeFrame
                          (Frame (FrameHeader 8 FramePing 0 0)
                                 (PingFrame (BS.pack [1,2,3,4,5,6,7,8])))
          combined    = dataBs <> pingBs
          -- Split mid-way through the second frame's header so the
          -- streaming parser has to suspend twice.
          (l, r)      = BS.splitAt (BS.length dataBs + 4) combined
      seen <- newIORef ([] :: [(FrameType, Int)])
      recvFn <- chunkedRecvFn [l, r]
      _ <- withRecvBufTransport defaultTransportConfig recvFn $ \t ->
        runFrameLoop t $ \fr -> do
          modifyIORef seen
            (((fhType (frameHeader fr), fromIntegral (fhLength (frameHeader fr)))) :)
          xs <- readIORef seen
          pure (if length xs >= 2 then Stop else Continue)
      observed <- reverse <$> readIORef seen
      observed @?= [(FrameData, BS.length dataPayload), (FramePing, 8)]

  , testCase "raises an FrameStreamDecode on bad WINDOW_UPDATE" $ do
      let payload = BS.pack [0,0,0,0]  -- increment = 0 → InvalidWindowUpdateIncrement
          hdr     = FrameHeader 4 FrameWindowUpdate 0 1
          bs      = encodeFrameHeader hdr <> payload
      recvFn <- chunkedRecvFn [bs]
      r <- withRecvBufTransport defaultTransportConfig recvFn $ \t ->
        runFrameLoop t $ \_fr -> pure Continue
      case r of
        Left _  -> pure ()
        Right _ -> assertFailure "expected a decode error"
  ]
