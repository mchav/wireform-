{-# LANGUAGE OverloadedStrings #-}
module Test.HPACKConcurrency (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP2.HPACK

tests :: TestTree
tests = testGroup "HPACK concurrency"
  [ testCase "sequential encode/decode consistency across 100 header blocks" $ do
      encDt <- newDynamicTable 4096
      decDt <- newDynamicTable 4096
      mapM_ (\i -> do
        let headers =
              [ (":method", "GET")
              , (":path", BS8.pack ("/" <> show i))
              , (":scheme", "https")
              , (":authority", "example.com")
              , ("x-request-id", BS8.pack (show i))
              ]
        encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
        result <- decodeHeaderBlock decDt encoded
        case result of
          Right decoded -> decoded @?= headers
          Left err -> assertFailure ("decode failed at iteration " <> show i <> ": " <> show err)
        ) [1 :: Int .. 100]

  , testCase "serialized MVar access preserves table consistency" $ do
      encMVar <- newDynamicTable 4096 >>= newMVar
      decMVar <- newDynamicTable 4096 >>= newMVar

      errRef <- newIORef (Nothing :: Maybe String)
      doneVar <- newEmptyMVar

      let worker i = do
            result <- try @SomeException $ do
              let headers =
                    [ (":method", if even i then "GET" else "POST")
                    , (":path", BS8.pack ("/worker/" <> show i))
                    , (":scheme", "https")
                    , (":authority", BS8.pack ("host-" <> show (i `mod` 3) <> ".example.com"))
                    , ("x-worker-id", BS8.pack (show i))
                    , ("x-timestamp", BS8.pack (show (i * 1000)))
                    ]
              encoded <- withMVar encMVar $ \enc ->
                encodeHeaderBlock defaultEncodeStrategy enc headers
              decoded <- withMVar decMVar $ \dec ->
                decodeHeaderBlock dec encoded
              case decoded of
                Right hdrs -> do
                  when (hdrs /= headers) $
                    atomicModifyIORef' errRef (\_ ->
                      (Just ("worker " <> show i <> ": headers mismatch"), ()))
                Left err ->
                  atomicModifyIORef' errRef (\_ ->
                    (Just ("worker " <> show i <> ": decode error: " <> show err), ()))
            case result of
              Left e -> atomicModifyIORef' errRef (\_ ->
                (Just ("worker " <> show i <> ": exception: " <> show e), ()))
              Right () -> pure ()
            putMVar doneVar ()

      mapM_ (\i -> forkIO (worker i)) [1 :: Int .. 20]
      mapM_ (\_ -> takeMVar doneVar) [1 :: Int .. 20]
      merr <- readIORef errRef
      case merr of
        Just e  -> assertFailure e
        Nothing -> pure ()

  , testCase "table remains consistent after many distinct header values" $ do
      encDt <- newDynamicTable 4096
      decDt <- newDynamicTable 4096
      mapM_ (\i -> do
        let headers = [(BS8.pack ("x-key-" <> show (i `mod` 10)), BS8.pack ("val-" <> show i))]
        encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
        result <- decodeHeaderBlock decDt encoded
        result @?= Right headers
        ) [1 :: Int .. 200]

  , testCase "concurrent encodes with MVar produce valid header blocks" $ do
      encMVar <- newDynamicTable 4096 >>= newMVar
      errRef <- newIORef (Nothing :: Maybe String)
      doneVar <- newEmptyMVar

      let worker i = do
            encoded <- withMVar encMVar $ \enc -> do
              let headers =
                    [ (":status", "200")
                    , ("content-type", "application/json")
                    , ("x-stream-id", BS8.pack (show i))
                    ]
              encodeHeaderBlock defaultEncodeStrategy enc headers
            when (BS.null encoded) $
              atomicModifyIORef' errRef (\_ ->
                (Just ("worker " <> show i <> ": empty encoded block"), ()))
            putMVar doneVar ()

      mapM_ (\i -> forkIO (worker i)) [1 :: Int .. 30]
      mapM_ (\_ -> takeMVar doneVar) [1 :: Int .. 30]
      merr <- readIORef errRef
      case merr of
        Just e  -> assertFailure e
        Nothing -> pure ()
  ]

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = pure ()
