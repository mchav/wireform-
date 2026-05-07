-- | Convenience re-exports for the Kafka client surface.
--
-- @
-- import qualified Wireform.Kafka as Kafka
-- @
--
-- See "Kafka" for the package-level umbrella, and the individual
-- @Kafka.*@ namespaces for finer-grained imports (the wire protocol
-- primitives, generated request\/response modules, network and
-- compression layers).
module Wireform.Kafka
  ( module Kafka
  ) where

import Kafka
