module Kafka.Dump
  ( hPrintSupportedKafkaConf
  , hPrintKafka
  , dumpKafkaConf
  , dumpTopicConf
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Kafka.Internal.Compat
  ( HasKafka
  , HasKafkaConf (..)
  , HasTopicConf (..)
  , kcfgKafkaProps
  , topicConfProps
  )
import System.IO (Handle)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO

hPrintSupportedKafkaConf :: MonadIO m => Handle -> m ()
hPrintSupportedKafkaConf h =
  liftIO (TIO.hPutStrLn h "wireform-hw-kafka-client: librdkafka conf dump is not available")

hPrintKafka :: (MonadIO m, HasKafka k) => Handle -> k -> m ()
hPrintKafka h _ =
  liftIO (TIO.hPutStrLn h "wireform-hw-kafka-client: kafka handle dump is not available")

dumpTopicConf :: (MonadIO m, HasTopicConf t) => t -> m (Map Text Text)
dumpTopicConf t = pure (topicConfProps (getTopicConf t))

dumpKafkaConf :: (MonadIO m, HasKafkaConf k) => k -> m (Map Text Text)
dumpKafkaConf k = pure (kcfgKafkaProps (getKafkaConf k))
