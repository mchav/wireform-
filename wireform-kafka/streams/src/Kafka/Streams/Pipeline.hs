{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Pipeline
-- Description : First-class composable topology fragments
--
-- A 'Pipeline a b' is a /value/ that describes a topology
-- transformation from a stream of type @a@ to a stream of
-- type @b@. Under the hood it's a 'Control.Arrow.Kleisli'
-- arrow over 'IO': @Pipeline a b ≃ a -> IO b@.
--
-- Pipelines compose with the usual 'Control.Category' /
-- 'Control.Arrow' vocabulary:
--
-- @
-- import Control.Category ((>>>))
--
-- normalise :: 'Pipeline' ('KStream' Text Text) ('KStream' Text Text)
-- normalise = 'pmapValues' T.toUpper
--         >>> 'pfilter'    (\\r -> recordValue r \/= \"\")
-- @
--
-- And you /apply/ a pipeline to a stream with 'applyPipeline'
-- (or just call the function directly):
--
-- @
-- src <- streamFromTopic b \"in\" (consumed textSerde textSerde)
-- out <- applyPipeline normalise src
-- toTopic \"out\" (produced textSerde textSerde) out
-- @
--
-- This module sits alongside (not on top of) the existing
-- imperative DSL — that API stays for users who prefer the
-- Java-style fluent shape.
module Kafka.Streams.Pipeline
  ( -- * Pipeline
    Pipeline (..)
  , applyPipeline
    -- * Smart constructors over 'KStream'
  , pmapValues
  , pmapKeyValue
  , pfilter
  , pflatMapValues
  , pflatMapKeyValue
  , ppeek
  , pselectKey
    -- * Lifting / lowering
  , liftIOAction
  ) where

import Control.Category (Category (..))
import Control.Monad ((>=>))
import Prelude hiding (id, (.))

import qualified Kafka.Streams.KStream as KS
import Kafka.Streams.Types (Record)

----------------------------------------------------------------------
-- Pipeline
----------------------------------------------------------------------

-- | A composable topology fragment. Isomorphic to
-- 'Control.Arrow.Kleisli' @IO a b@; we use a dedicated
-- newtype so error messages and Haddock both refer to
-- @Pipeline@ instead of @Kleisli IO@.
newtype Pipeline a b = Pipeline
  { runPipeline :: a -> IO b
  }

-- | Category-style composition: 'id' is the no-op pipeline,
-- @q '.' p@ runs @p@ first then @q@. The same shape as
-- function composition; '(.)' is right-to-left, '(>>>)' is
-- left-to-right.
instance Category Pipeline where
  id :: Pipeline a a
  id = Pipeline pure

  (.) :: Pipeline b c -> Pipeline a b -> Pipeline a c
  Pipeline g . Pipeline f = Pipeline (f >=> g)

-- | 'applyPipeline' is the canonical way to run a pipeline
-- against a stream. It's a synonym for 'runPipeline' chosen
-- for readability at call sites:
--
-- @
-- result <- applyPipeline pipeline source
-- @
applyPipeline :: Pipeline a b -> a -> IO b
applyPipeline = runPipeline

----------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------

-- | Pure value-only transform.
pmapValues
  :: (v -> v')
  -> Pipeline (KS.KStream k v) (KS.KStream k v')
pmapValues f = Pipeline (KS.mapValues f)

-- | Pure key+value transform.
pmapKeyValue
  :: (k -> v -> (k', v'))
  -> Pipeline (KS.KStream k v) (KS.KStream k' v')
pmapKeyValue f = Pipeline (KS.mapKeyValue f)

-- | Predicate filter. Records the predicate rejects are
-- dropped from the downstream stream.
pfilter
  :: (Record k v -> Bool)
  -> Pipeline (KS.KStream k v) (KS.KStream k v)
pfilter p = Pipeline (KS.filterStream p)

-- | One-to-many value transform.
pflatMapValues
  :: (v -> [v'])
  -> Pipeline (KS.KStream k v) (KS.KStream k v')
pflatMapValues f = Pipeline (KS.flatMapValues f)

-- | One-to-many key+value transform.
pflatMapKeyValue
  :: (k -> v -> [(k', v')])
  -> Pipeline (KS.KStream k v) (KS.KStream k' v')
pflatMapKeyValue f = Pipeline (KS.flatMapKeyValue f)

-- | Side-effecting observer; doesn't change the stream.
ppeek
  :: (Record k v -> IO ())
  -> Pipeline (KS.KStream k v) (KS.KStream k v)
ppeek f = Pipeline (KS.peekStream f)

-- | Re-key the stream from the full record.
pselectKey
  :: (Record k v -> k')
  -> Pipeline (KS.KStream k v) (KS.KStream k' v)
pselectKey f = Pipeline (KS.selectKey f)

----------------------------------------------------------------------
-- Lifting / lowering
----------------------------------------------------------------------

-- | Lift an arbitrary @a -> IO b@ action into a 'Pipeline'.
-- Useful when you've already written the topology-mutating
-- function in the imperative style and want to splice it
-- into a Category-style composition without re-writing.
liftIOAction :: (a -> IO b) -> Pipeline a b
liftIOAction = Pipeline
