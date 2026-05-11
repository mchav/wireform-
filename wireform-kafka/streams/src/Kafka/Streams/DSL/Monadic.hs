{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.DSL.Monadic
-- Description : Monadic wrappers over the IO-based DSL
--
-- A thin shim that lifts the existing
-- @'Kafka.Streams.DSL.KStream'.* :: ... -> IO (KStream k v)@
-- combinators into 'TopologyM'. Users who want the
-- Haskell-native style import this module and write the
-- topology as a single @do@ block:
--
-- @
-- topology :: TopologyM ()
-- topology = do
--   src <- streamFrom (topicName \"in\")
--                     (consumed textSerde textSerde)
--   out <- src
--           |> mapValuesM T.toUpper
--           |> filterStreamM (\\r -> recordValue r \/= \"skip\")
--   sinkTo (topicName \"out\")
--          (produced textSerde textSerde) out
-- @
--
-- The names end in @M@ to avoid clashing with the existing
-- @IO@-returning versions in
-- 'Kafka.Streams.DSL.KStream'. Pick one style per module —
-- the two are interoperable via 'liftBuilder' but mixing them
-- in the same @do@ block is confusing.
module Kafka.Streams.DSL.Monadic
  ( -- * Sources
    streamFrom
  , tableFrom
    -- * Sinks
  , sinkTo
    -- * Stateless transforms (lifted into 'TopologyM')
  , mapValuesM
  , mapKeyValueM
  , filterStreamM
  , flatMapValuesM
  , flatMapKeyValueM
  , peekStreamM
  , selectKeyM
    -- * Re-exports
  , module Kafka.Streams.DSL.Topology
  ) where

import qualified Kafka.Streams.DSL.Consumed as Consumed
import qualified Kafka.Streams.DSL.KStream as KS
import qualified Kafka.Streams.DSL.KTable as KT
import qualified Kafka.Streams.DSL.Materialized as Materialized
import qualified Kafka.Streams.DSL.Produced as Produced
import Kafka.Streams.DSL.Topology
import Kafka.Streams.Types (Record, TopicName)

----------------------------------------------------------------------
-- Sources
----------------------------------------------------------------------

-- | Monadic equivalent of 'KS.streamFromTopic'.
streamFrom
  :: TopicName
  -> Consumed.Consumed k v
  -> TopologyM (KS.KStream k v)
streamFrom t c = liftBuilder $ \b -> KS.streamFromTopic b t c

-- | Monadic equivalent of 'KT.tableFromTopic'.
tableFrom
  :: Ord k
  => TopicName
  -> Consumed.Consumed k v
  -> Materialized.Materialized k v
  -> TopologyM (KT.KTable k v)
tableFrom t c m = liftBuilder $ \b -> KT.tableFromTopic b t c m

----------------------------------------------------------------------
-- Sinks
----------------------------------------------------------------------

-- | Monadic equivalent of 'KS.toTopic'. Returns @()@ because
-- a sink is terminal.
sinkTo
  :: TopicName
  -> Produced.Produced k v
  -> KS.KStream k v
  -> TopologyM ()
sinkTo t p s = liftBuilder $ \_ -> KS.toTopic t p s

----------------------------------------------------------------------
-- Stateless transforms
----------------------------------------------------------------------

-- | Monadic 'KS.mapValues'.
mapValuesM
  :: (v -> v')
  -> KS.KStream k v
  -> TopologyM (KS.KStream k v')
mapValuesM f s = liftBuilder $ \_ -> KS.mapValues f s

-- | Monadic 'KS.mapKeyValue'.
mapKeyValueM
  :: (k -> v -> (k', v'))
  -> KS.KStream k v
  -> TopologyM (KS.KStream k' v')
mapKeyValueM f s = liftBuilder $ \_ -> KS.mapKeyValue f s

-- | Monadic 'KS.filterStream'.
filterStreamM
  :: (Record k v -> Bool)
  -> KS.KStream k v
  -> TopologyM (KS.KStream k v)
filterStreamM p s = liftBuilder $ \_ -> KS.filterStream p s

-- | Monadic 'KS.flatMapValues'.
flatMapValuesM
  :: (v -> [v'])
  -> KS.KStream k v
  -> TopologyM (KS.KStream k v')
flatMapValuesM f s = liftBuilder $ \_ -> KS.flatMapValues f s

-- | Monadic 'KS.flatMapKeyValue'.
flatMapKeyValueM
  :: (k -> v -> [(k', v')])
  -> KS.KStream k v
  -> TopologyM (KS.KStream k' v')
flatMapKeyValueM f s = liftBuilder $ \_ -> KS.flatMapKeyValue f s

-- | Monadic 'KS.peekStream'.
peekStreamM
  :: (Record k v -> IO ())
  -> KS.KStream k v
  -> TopologyM (KS.KStream k v)
peekStreamM f s = liftBuilder $ \_ -> KS.peekStream f s

-- | Monadic 'KS.selectKey'.
selectKeyM
  :: (Record k v -> k')
  -> KS.KStream k v
  -> TopologyM (KS.KStream k' v)
selectKeyM f s = liftBuilder $ \_ -> KS.selectKey f s
