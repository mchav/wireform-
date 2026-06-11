{-# LANGUAGE OverloadedStrings #-}

{- | End-to-end tests for "Kafka.Streams.DSL" — the Haskell-native
builder-implicit façade with the @|>@ pipe operator.
-}
module Streams.DSLFacadeSpec (tests) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.DSL qualified as S
import Kafka.Streams.Imperative (
  Timestamp (..),
  closeDriver,
  createOutputTopic,
  newDriver,
  pipeInput,
  readKeyValuesToList,
  textSerde,
  topicName,
 )
import Kafka.Streams.Pipeline qualified as P
import Test.Syd


tests :: Spec
tests =
  describe "Kafka.Streams.DSL (builder-implicit façade)" $
    sequence_
      [ dsl_pipe_chain
      , dsl_multiple_branches
      , dsl_table_join
      , dsl_pipeline_fragment
      ]


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


ts :: Int -> Timestamp
ts = Timestamp . fromIntegral


{- | A straight `|>` chain produces the same observable output as
the imperative API, with the builder implicit and no
per-operation IO threading.
-}
dsl_pipe_chain :: Spec
dsl_pipe_chain =
  it "source |> map |> filter |> sink" $ do
    topo <- S.build $ do
      src <- S.source "in" textSerde textSerde
      src
        S.|> S.map T.toUpper
        S.|> S.filter (\r -> S.recordValue r /= "")
        S.|> S.map (T.take 4)
        S.|> S.sink "out" textSerde textSerde

    driver <- newDriver topo "dsl-pipe-chain"
    pipeInput driver (topicName "in") (Just "k1") (bytes "hello") (ts 0) 0
    pipeInput driver (topicName "in") (Just "k2") (bytes "") (ts 1) 0
    pipeInput driver (topicName "in") (Just "k3") (bytes "haskell") (ts 2) 0
    let outT = createOutputTopic driver (topicName "out") textSerde textSerde
    rs <- readKeyValuesToList outT
    let vs = [v | Right (_, v) <- rs]
    vs `shouldBe` ["HELL", "HASK"]
    closeDriver driver


{- | The same intermediate stream can be sent to multiple sinks
without rebuilding it. Exercises the side-effecting nature of
'S.sink' and the topology builder underneath.
-}
dsl_multiple_branches :: Spec
dsl_multiple_branches =
  it "single source feeds two sinks" $ do
    topo <- S.build $ do
      src <- S.source "in" textSerde textSerde
      upper <- src S.|> S.map T.toUpper
      upper S.|> S.sink "upper" textSerde textSerde
      src
        S.|> S.filter (\r -> T.length (S.recordValue r) > 4)
        S.|> S.sink "long" textSerde textSerde

    driver <- newDriver topo "dsl-branches"
    pipeInput driver (topicName "in") (Just "k1") (bytes "hi") (ts 0) 0
    pipeInput driver (topicName "in") (Just "k2") (bytes "hello") (ts 1) 0
    pipeInput driver (topicName "in") (Just "k3") (bytes "world!") (ts 2) 0

    let upT = createOutputTopic driver (topicName "upper") textSerde textSerde
        lgT = createOutputTopic driver (topicName "long") textSerde textSerde
    upR <- readKeyValuesToList upT
    lgR <- readKeyValuesToList lgT
    [v | Right (_, v) <- upR] `shouldBe` ["HI", "HELLO", "WORLD!"]
    [v | Right (_, v) <- lgR] `shouldBe` ["hello", "world!"]
    closeDriver driver


{- | A first-class 'P.Pipeline' fragment (built with the
'Control.Category.>>>' arrow vocabulary) can be spliced
straight into a @|>@ chain via the 'S.PipeInto' instance for
'P.Pipeline', interleaved with ordinary @b -> Streams c@
steps. Confirms the two composition styles compose.
-}
dsl_pipeline_fragment :: Spec
dsl_pipeline_fragment =
  it "a reusable Pipeline fragment splices into a |> chain" $ do
    let normalise :: P.Pipeline (S.KStream Text Text) (S.KStream Text Text)
        normalise =
          P.pmapValues T.toUpper
            >>> P.pfilter (\r -> S.recordValue r /= "")
    topo <- S.build $ do
      src <- S.source "in" textSerde textSerde
      src
        S.|> normalise
        S.|> S.sink "out" textSerde textSerde

    driver <- newDriver topo "dsl-pipeline-fragment"
    pipeInput driver (topicName "in") (Just "k1") (bytes "hello") (ts 0) 0
    pipeInput driver (topicName "in") (Just "k2") (bytes "") (ts 1) 0
    pipeInput driver (topicName "in") (Just "k3") (bytes "world") (ts 2) 0
    let outT = createOutputTopic driver (topicName "out") textSerde textSerde
    rs <- readKeyValuesToList outT
    [v | Right (_, v) <- rs] `shouldBe` ["HELLO", "WORLD"]
    closeDriver driver


{- | Stream-table inner join inside the DSL. Exercises 'S.table',
'S.join', and the 'S.|>' pipe over a 'KTable' argument.
-}
dsl_table_join :: Spec
dsl_table_join =
  it "stream-table inner join via the DSL" $ do
    let joinedConf = (error "unused Joined" :: S.Joined Text Text Text)
    topo <- S.build $ do
      stream <- S.source "events" textSerde textSerde
      lookup' <- S.table "people" textSerde textSerde
      joined <- S.join (\v vt -> v <> "@" <> vt) joinedConf stream lookup'
      joined S.|> S.sink "out" textSerde textSerde

    driver <- newDriver topo "dsl-table-join"
    -- 1) load the lookup table
    pipeInput driver (topicName "people") (Just "alice") (bytes "Engineering") (ts 0) 0
    pipeInput driver (topicName "people") (Just "bob") (bytes "Sales") (ts 1) 0
    -- 2) drive a few stream records through the join
    pipeInput driver (topicName "events") (Just "alice") (bytes "login") (ts 10) 0
    pipeInput driver (topicName "events") (Just "carol") (bytes "login") (ts 11) 0
    pipeInput driver (topicName "events") (Just "bob") (bytes "logout") (ts 12) 0

    let outT = createOutputTopic driver (topicName "out") textSerde textSerde
    rs <- readKeyValuesToList outT
    [v | Right (_, v) <- rs] `shouldBe` ["login@Engineering", "logout@Sales"]
    closeDriver driver
