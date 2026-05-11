{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Kafka.Streams.DSL.Topology
-- Description : Monadic façade for building topologies
--
-- The existing DSL ('Kafka.Streams.DSL.KStream', etc.) is built
-- around an explicit @'StreamsBuilder'@ value and combinators
-- that return @'IO' ('KStream' k v)@. That maps the Java fluent
-- API one-for-one, but in Haskell it's awkward — every step
-- needs an @\<-@ in a @do@ block, and there's no real way to
-- @fmap@ or @>>>@ over a chain of transformations.
--
-- 'TopologyM' is a thin monadic wrapper that hides the
-- builder. A topology becomes a @do@ block whose return value
-- is whatever the user wants to expose downstream (often
-- @()@ — the topology is the side-effect):
--
-- @
-- helloWorld :: TopologyM ()
-- helloWorld = do
--   src <- streamFrom (topicName \"in\")
--                     (consumed textSerde textSerde)
--   out <- src
--           |> mapValues T.toUpper
--           |> filterStream (\\r -> recordValue r \/= \"skip\")
--   sinkTo (topicName \"out\")
--          (produced textSerde textSerde) out
-- @
--
-- 'runTopologyM' is the bridge to the existing API: given a
-- 'TopologyM' action it allocates a fresh 'StreamsBuilder',
-- runs the action, and returns the resulting 'Topology'.
module Kafka.Streams.DSL.Topology
  ( TopologyM
  , runTopologyM
  , liftBuilder
  , askBuilder
    -- * Pipe operator
  , (|>)
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader
  ( ReaderT
  , ask
  , runReaderT
  )

import qualified Kafka.Streams.DSL.StreamsBuilder as SB
import qualified Kafka.Streams.Topology as Topo

----------------------------------------------------------------------
-- TopologyM
----------------------------------------------------------------------

-- | Monad for building topologies. Carries the underlying
-- 'StreamsBuilder' in a 'ReaderT' so DSL ops can mutate the
-- topology without the user threading it through. All the
-- existing @IO ('KStream' k v)@ combinators lift naturally
-- via 'liftBuilder'.
--
-- 'TopologyM' is a 'Functor' / 'Applicative' / 'Monad' /
-- 'MonadIO' so all the usual idioms (@do@-notation, @fmap@,
-- @\<$\>@, @\<*\>@, @when@, @forM_@) compose cleanly.
newtype TopologyM a = TopologyM
  { unTopologyM :: ReaderT SB.StreamsBuilder IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

-- | Lift an @IO@-returning DSL combinator (the existing
-- shape: @StreamsBuilder -> IO a@-style) into 'TopologyM'.
-- Most user code never calls this directly — the
-- @Kafka.Streams.DSL.*@ modules expose typed wrappers.
liftBuilder :: (SB.StreamsBuilder -> IO a) -> TopologyM a
liftBuilder f = TopologyM $ do
  b <- ask
  liftIO (f b)

-- | Access the underlying 'StreamsBuilder'. Provided for the
-- subset of DSL operations that take the builder as a
-- positional first argument (e.g. 'streamFromTopic') — those
-- wrap as 'streamFrom' / 'tableFrom' helpers that call
-- @askBuilder >>= flip op args@ internally.
askBuilder :: TopologyM SB.StreamsBuilder
askBuilder = TopologyM ask

-- | Build a 'Topology' by running a 'TopologyM' action
-- against a fresh 'StreamsBuilder'. Returns the topology plus
-- whatever value the action produced (often @()@).
runTopologyM :: TopologyM a -> IO (Topo.Topology, a)
runTopologyM (TopologyM action) = do
  b <- SB.newStreamsBuilder
  !a <- runReaderT action b
  t <- SB.buildTopology b
  pure (t, a)

----------------------------------------------------------------------
-- Pipe operator
----------------------------------------------------------------------

-- | Left-to-right function-application — the same idea as
-- F#'s @|>@ or Elixir's @|>@.
--
-- @x |> f@ is exactly @f x@; the only purpose is to let users
-- chain a sequence of DSL transformations reading left-to-right:
--
-- @
-- src \<- streamFrom ...
-- out \<- src
--           |> mapValues T.toUpper
--           |> filterStream (\\r -> ...)
-- @
--
-- The operator is right-associative at precedence 1 (looser
-- than @\<$>@ / @>>=@) so it composes naturally with monadic
-- binds when one of the transformations is already in
-- 'TopologyM'.
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 1 |>
