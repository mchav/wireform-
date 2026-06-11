{-# LANGUAGE OverloadedStrings #-}

{- | End-to-end test for the one Haskell-native DSL façade that
sits next to the imperative IO-based core: 'Pipeline'.

'Pipeline' is a 'Control.Category.Category' over
'KStream'-shaped IO functions. It lets users build reusable
topology fragments as values and compose them with @(>>>)@
like ordinary functions, without introducing a parallel
monad/DSL surface.
-}
module Streams.IdiomaticDSLSpec (tests) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Kafka.Streams.Pipeline
import Test.Syd


tests :: Spec
tests =
  describe "Pipeline (Haskell-native composable fragments)" $
    sequence_
      [ pipeline_arrow_composition
      ]


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


ts :: Int -> Timestamp
ts = Timestamp . fromIntegral


-- A Pipeline value composed with 'Category' @(>>>)@. The same
-- pipeline value can be applied to many streams across many
-- topologies.
pipeline_arrow_composition :: Spec
pipeline_arrow_composition =
  it "Category-composed pipeline applies end-to-end" $ do
    let normalise :: Pipeline (KStream Text Text) (KStream Text Text)
        normalise =
          pmapValues T.toUpper
            >>> pfilter (\r -> recordValue r /= "")
            >>> pmapValues (T.take 4)

    b <- newStreamsBuilder
    src <-
      streamFromTopic
        b
        (topicName "in")
        (consumed textSerde textSerde)
    out <- applyPipeline normalise src
    toTopic (topicName "out") (produced textSerde textSerde) out
    topo <- buildTopology b

    driver <- newDriver topo "pipeline-arrow"
    pipeInput driver (topicName "in") (Just "k1") (bytes "hello") (ts 0) 0
    pipeInput driver (topicName "in") (Just "k2") (bytes "") (ts 1) 0
    pipeInput driver (topicName "in") (Just "k3") (bytes "haskell") (ts 2) 0
    let outT = createOutputTopic driver (topicName "out") textSerde textSerde
    rs <- readKeyValuesToList outT
    [v | Right (_, v) <- rs] `shouldBe` ["HELL", "HASK"]
    closeDriver driver
