{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end tests for the rotating-file machinery folded
-- into "Kafka.Streams.Printed".
module Streams.RotatingFileSinkSpec (tests) where

import Control.Exception (bracket)
import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , listDirectory
  , removeFile
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams
  ( Timestamp (..)
  , closeDriver
  , newDriver
  , newStreamsBuilder
  , pipeInput
  , streamFromTopic
  , consumed
  , textSerde
  , topicName
  , buildTopology
  )
import qualified Kafka.Streams.Printed as Printed
-- The rotating-handle machinery used to live in
-- 'Kafka.Streams.Sink.RotatingFile'. It now hangs off
-- 'Kafka.Streams.Printed' alongside the JVM-parity
-- @Printed.toRotatingFile@ builder. The @RF@ alias is kept so
-- the test bodies still read as low-level handle operations.
import qualified Kafka.Streams.Printed as RF

tests :: TestTree
tests = testGroup "Kafka.Streams.Printed (rotating-file sink)"
  [ rotating_basic_write
  , rotating_size_based_rollover
  , printed_to_rotating_file
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

ts :: Int -> Timestamp
ts = Timestamp . fromIntegral

-- | A simple no-rotation run: every record ends up in the
-- active file.
rotating_basic_write :: TestTree
rotating_basic_write =
  testCase "no-rotation: every record lands in the active file" $
    withSystemTempDirectory "wireform-rot-sink" $ \dir -> do
      let logPath = dir </> "stream.log"
      bracket
        (RF.openRotatingHandle RF.defaultRotatingFileConfig { RF.rfPath = logPath })
        RF.closeRotatingHandle
        $ \rh -> do
            b   <- newStreamsBuilder
            src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
            RF.rotatingPrintStream rh "stream" src
            topo <- buildTopology b

            driver <- newDriver topo "rotating-sink-basic"
            pipeInput driver (topicName "in") (Just "k1") (bytes "alpha") (ts 0) 0
            pipeInput driver (topicName "in") (Just "k2") (bytes "beta")  (ts 1) 0
            closeDriver driver

      exists <- doesFileExist logPath
      assertBool "active log exists" exists
      txt <- readFile logPath
      assertBool ("contains alpha: " <> txt) ("alpha" `T.isInfixOf` T.pack txt)
      assertBool ("contains beta: "  <> txt) ("beta"  `T.isInfixOf` T.pack txt)

-- | A size-based rollover triggers exactly when the next write
-- would exceed 'rfMaxBytes'. We feed enough records to clear
-- the size cap once and assert that an archive file appeared.
rotating_size_based_rollover :: TestTree
rotating_size_based_rollover =
  testCase "size-based rollover produces an archive file" $
    withSystemTempDirectory "wireform-rot-sink-roll" $ \dir -> do
      createDirectoryIfMissing True dir
      let logPath = dir </> "stream.log"
          cfg = RF.defaultRotatingFileConfig
            { RF.rfPath     = logPath
            , RF.rfMaxBytes = Just 32    -- ~32 bytes triggers fast
            }
      bracket (RF.openRotatingHandle cfg) RF.closeRotatingHandle $ \rh -> do
        b   <- newStreamsBuilder
        src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
        RF.rotatingPrintStream rh "stream" src
        topo <- buildTopology b

        driver <- newDriver topo "rotating-sink-roll"
        replicateM_ 10 $
          pipeInput driver (topicName "in")
                    (Just "kkkkkkkkkkkkkkkk") (bytes "vvvvvvvvvvvvvvvv")
                    (ts 0) 0
        closeDriver driver

      entries <- listDirectory dir
      -- We expect the active file (stream.log) plus at least one
      -- archive (stream.YYYYMMDDThhmmssZ.log).
      let archives = filter (\e -> e /= "stream.log") entries
      assertBool
        ("expected at least one archived file, got: " <> show entries)
        (not (null archives))
      length entries @?= length entries -- silence unused
      mapM_ (removeFile . (dir </>)) entries

-- | End-to-end: 'Printed.withPrintedRotatingFile' wires
-- 'Kafka.Streams.Printed' to the rotating file sink with a
-- proper lifecycle bracket. Mirrors the JVM
-- @KStream.print(Printed.toFile(path))@ with auto rotation.
printed_to_rotating_file :: TestTree
printed_to_rotating_file =
  testCase "Printed.withPrintedRotatingFile end-to-end with label override" $
    withSystemTempDirectory "wireform-printed" $ \dir -> do
      let logPath = dir </> "p.log"
      Printed.withPrintedRotatingFile
        logPath
        (Printed.withMaxBytes 1024 Printed.def)
        $ \printed -> do
            b   <- newStreamsBuilder
            src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
            Printed.printKStream
              (Printed.withLabel "[from-printed]" printed) src
            topo <- buildTopology b
            driver <- newDriver topo "printed-test"
            pipeInput driver (topicName "in") (Just "k1") (bytes "hello") (ts 0) 0
            closeDriver driver
      txt <- readFile logPath
      assertBool ("contains label: " <> txt)
        ("[from-printed]" `T.isInfixOf` T.pack txt)
      assertBool ("contains value: " <> txt)
        ("hello" `T.isInfixOf` T.pack txt)
