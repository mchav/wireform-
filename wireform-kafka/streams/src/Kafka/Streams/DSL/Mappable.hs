-- |
-- Module      : Kafka.Streams.DSL.Mappable
-- Description : Functor-flavoured wrapper over 'KStream'
--
-- 'KStream' has two type parameters @k@ and @v@, so it can't
-- be a 'Functor' directly. 'OfStream' is a one-parameter
-- newtype that fixes the key type and exposes a 'Functor'
-- instance over the value:
--
-- @
-- s :: 'KS.KStream' Text Int
-- s2 \<- 'withStream' (fmap (+ 1) ('OfStream' (pure s)))
-- -- equivalent to:  s2 \<- mapValues (+ 1) s
-- @
--
-- The wrapper defers the topology-mutation: every 'fmap'
-- chains an 'IO' action; 'withStream' runs the chain to get
-- the materialised stream. Chaining is the point — you can
-- write
--
-- @
-- s' \<- 'withStream' $
--   ('OfStream' (pure s))
--     '&' fmap (+ 1)
--     '&' fmap (* 2)
--     '&' fmap show
-- @
--
-- and the runtime registers three @mapValues@ nodes in order.
module Kafka.Streams.DSL.Mappable
  ( OfStream (..)
  , withStream
  , liftStream
  ) where

import qualified Kafka.Streams.DSL.KStream as KS

-- | Functor wrapper over 'KStream'. The wrapped 'IO' is a
-- deferred topology mutation; 'fmap' chains another
-- 'mapValues' on top.
newtype OfStream k v = OfStream
  { unwrapOfStream :: IO (KS.KStream k v)
  }

instance Functor (OfStream k) where
  fmap f (OfStream m) = OfStream (m >>= KS.mapValues f)

-- | Promote a 'KStream' into the wrapper so it can be 'fmap'-ped.
liftStream :: KS.KStream k v -> OfStream k v
liftStream s = OfStream (pure s)

-- | Run a deferred-chain 'OfStream' to get the materialised
-- 'KStream' (registering every queued transformation in the
-- topology). Use this just before sinking the stream.
withStream :: OfStream k v -> IO (KS.KStream k v)
withStream = unwrapOfStream
