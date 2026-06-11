{- |
Module      : Kafka.Dump
Description : Legacy configuration dump helpers.

@hw-kafka-client@ exposed librdkafka dump functions. This transitional
facade can return the compatibility property maps it stores, but native
wireform handles do not have librdkafka's complete runtime dump.
-}
module Kafka.Dump (
  hPrintSupportedKafkaConf,
  hPrintKafka,
  dumpKafkaConf,
  dumpTopicConf,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Kafka.Internal.Compat (
  HasKafka,
  HasKafkaConf (..),
  HasTopicConf (..),
  kcfgKafkaProps,
  topicConfProps,
 )
import System.IO (Handle)


-- | Print a note about supported configuration.
hPrintSupportedKafkaConf :: MonadIO m => Handle -> m ()
hPrintSupportedKafkaConf h =
  liftIO (TIO.hPutStrLn h "wireform-hw-kafka-client: librdkafka conf dump is not available")


-- | Print a note for a specific Kafka compatibility handle.
hPrintKafka :: (MonadIO m, HasKafka k) => Handle -> k -> m ()
hPrintKafka h _ =
  liftIO (TIO.hPutStrLn h "wireform-hw-kafka-client: kafka handle dump is not available")


-- | Return the topic properties stored in a compatibility handle.
dumpTopicConf :: (MonadIO m, HasTopicConf t) => t -> m (Map Text Text)
dumpTopicConf t = pure (topicConfProps (getTopicConf t))


-- | Return the Kafka properties stored in a compatibility handle.
dumpKafkaConf :: (MonadIO m, HasKafkaConf k) => k -> m (Map Text Text)
dumpKafkaConf k = pure (kcfgKafkaProps (getKafkaConf k))
