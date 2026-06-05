module Kafka.Consumer
  ( KafkaConsumer
  , module X
  , runConsumer
  , newConsumer
  , assign
  , assignment
  , subscription
  , pausePartitions
  , resumePartitions
  , committed
  , position
  , seek
  , seekPartitions
  , pollMessage
  , pollConsumerEvents
  , pollMessageBatch
  , commitOffsetMessage
  , commitAllOffsets
  , commitPartitionsOffsets
  , storeOffsets
  , storeOffsetMessage
  , rewindConsumer
  , closeConsumer
  , RdKafkaRespErrT (..)
  ) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Set (Set)
import Kafka.Consumer.ConsumerProperties as X
import Kafka.Consumer.Subscription as X
import Kafka.Consumer.Types as X hiding (KafkaConsumer)
import Kafka.Consumer.Types (KafkaConsumer (..))
import Kafka.Internal.Compat
  ( Kafka (..)
  , KafkaConf (..)
  , RdKafkaRespErrT (..)
  , kafkaConf
  , textDecimal
  )
import Kafka.Types as X
import qualified Data.IORef as IORef
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Kafka.Client.Consumer as WF

{-# DEPRECATED runConsumer "Use 'newConsumer'/'closeConsumer' instead" #-}
runConsumer
  :: ConsumerProperties
  -> Subscription
  -> (KafkaConsumer -> IO (Either KafkaError a))
  -> IO (Either KafkaError a)
runConsumer props sub f =
  bracket (newConsumer props sub) closeResult runResult
  where
    closeResult (Left err) = pure (Left err)
    closeResult (Right consumer) = maybeToLeft <$> closeConsumer consumer

    runResult (Left err) = pure (Left err)
    runResult (Right consumer) = f consumer

newConsumer
  :: MonadIO m
  => ConsumerProperties
  -> Subscription
  -> m (Either KafkaError KafkaConsumer)
newConsumer props sub@(Subscription topicsSet subProps) = liftIO $ do
  kc <- kafkaConf (cpProps props <> subProps)
  let brokers = bootstrapServers (cpProps props)
      group = M.findWithDefault "default-group" "group.id" (cpProps props)
      cfg = consumerConfig props sub
      topicTexts = map unTopicName (Set.toList topicsSet)
  result <- WF.createConsumer brokers group cfg
  case result of
    Left err -> pure (Left (KafkaError (T.pack err)))
    Right consumer -> do
      subscribeResult <- case topicTexts of
        [] -> pure (Right ())
        ts -> WF.subscribe consumer ts
      pure $ case subscribeResult of
        Left err -> Left (KafkaError (T.pack err))
        Right () -> Right KafkaConsumer
          { kcKafkaPtr = KafkaConsumerHandle consumer
          , kcKafkaConf = kc
          }

pollMessage
  :: MonadIO m
  => KafkaConsumer
  -> Timeout
  -> m (Either KafkaError (ConsumerRecord (Maybe ByteString) (Maybe ByteString)))
pollMessage kc@KafkaConsumer{kcKafkaConf = conf} timeout = liftIO $ do
  buffered <- IORef.readIORef (kcfgBufferedRecords conf)
  case buffered of
    record:rest -> do
      IORef.writeIORef (kcfgBufferedRecords conf) rest
      pure (Right (fromWireformRecord record))
    [] -> do
      batch <- pollWireform kc timeout
      case batch of
        Left err -> pure (Left err)
        Right [] -> pure (Left (KafkaResponseError RdKafkaRespErrTimedOut))
        Right (record:rest) -> do
          IORef.writeIORef (kcfgBufferedRecords conf) rest
          pure (Right (fromWireformRecord record))

pollMessageBatch
  :: MonadIO m
  => KafkaConsumer
  -> Timeout
  -> BatchSize
  -> m [Either KafkaError (ConsumerRecord (Maybe ByteString) (Maybe ByteString))]
pollMessageBatch kc (Timeout timeoutMs) (BatchSize batchSize) = liftIO $ do
  batch <- pollWireform kc (Timeout timeoutMs)
  pure $ case batch of
    Left err -> [Left err]
    Right records -> map (Right . fromWireformRecord) (take batchSize records)

commitOffsetMessage
  :: MonadIO m
  => OffsetCommit
  -> KafkaConsumer
  -> ConsumerRecord k v
  -> m (Maybe KafkaError)
commitOffsetMessage mode kc record =
  commitPartitionsOffsets mode kc [topicPartitionFromRecord record]

storeOffsetMessage :: MonadIO m => KafkaConsumer -> ConsumerRecord k v -> m (Maybe KafkaError)
storeOffsetMessage _ _ = pure Nothing

storeOffsets :: MonadIO m => KafkaConsumer -> [TopicPartition] -> m (Maybe KafkaError)
storeOffsets _ _ = pure Nothing

commitAllOffsets :: MonadIO m => OffsetCommit -> KafkaConsumer -> m (Maybe KafkaError)
commitAllOffsets mode kc =
  commitResult mode kc

commitPartitionsOffsets
  :: MonadIO m
  => OffsetCommit
  -> KafkaConsumer
  -> [TopicPartition]
  -> m (Maybe KafkaError)
commitPartitionsOffsets mode kc partitions = do
  seekResult <- seekPartitions kc partitions (Timeout 0)
  case seekResult of
    Just err -> pure (Just err)
    Nothing -> commitResult mode kc

assign :: MonadIO m => KafkaConsumer -> [TopicPartition] -> m (Maybe KafkaError)
assign KafkaConsumer{kcKafkaPtr = KafkaConsumerHandle consumer} partitions = liftIO $ do
  result <- WF.assign consumer (map toWireformTopicPartition partitions)
  case result of
    Left err -> pure (Just (KafkaError (T.pack err)))
    Right () -> do
      seekErrs <- traverse (seekOne consumer) partitions
      pure (firstJust seekErrs)
assign _ _ =
  pure (Just (KafkaBadSpecification "KafkaConsumer does not contain a consumer handle"))

assignment :: MonadIO m => KafkaConsumer -> m (Either KafkaError (Map TopicName [PartitionId]))
assignment KafkaConsumer{kcKafkaPtr = KafkaConsumerHandle consumer} = liftIO $ do
  partitions <- WF.assignment consumer
  pure (Right (foldr insertPartition M.empty partitions))
assignment _ =
  pure (Left (KafkaBadSpecification "KafkaConsumer does not contain a consumer handle"))

subscription :: MonadIO m => KafkaConsumer -> m (Either KafkaError [(TopicName, SubscribedPartitions)])
subscription _ = pure (Right [])

pausePartitions :: MonadIO m => KafkaConsumer -> [(TopicName, PartitionId)] -> m KafkaError
pausePartitions KafkaConsumer{kcKafkaPtr = KafkaConsumerHandle consumer} partitions = liftIO $ do
  WF.pause consumer (map pairToWireformTopicPartition partitions)
  pure (KafkaResponseError RdKafkaRespErrNoError)
pausePartitions _ _ =
  pure (KafkaBadSpecification "KafkaConsumer does not contain a consumer handle")

resumePartitions :: MonadIO m => KafkaConsumer -> [(TopicName, PartitionId)] -> m KafkaError
resumePartitions KafkaConsumer{kcKafkaPtr = KafkaConsumerHandle consumer} partitions = liftIO $ do
  WF.resume consumer (map pairToWireformTopicPartition partitions)
  pure (KafkaResponseError RdKafkaRespErrNoError)
resumePartitions _ _ =
  pure (KafkaBadSpecification "KafkaConsumer does not contain a consumer handle")

seek :: MonadIO m => KafkaConsumer -> Timeout -> [TopicPartition] -> m (Maybe KafkaError)
seek kc _ partitions = seekPartitions kc partitions (Timeout 0)

seekPartitions :: MonadIO m => KafkaConsumer -> [TopicPartition] -> Timeout -> m (Maybe KafkaError)
seekPartitions KafkaConsumer{kcKafkaPtr = KafkaConsumerHandle consumer} partitions _ =
  liftIO (firstJust <$> traverse (seekOne consumer) partitions)
seekPartitions _ _ _ =
  pure (Just (KafkaBadSpecification "KafkaConsumer does not contain a consumer handle"))

committed
  :: MonadIO m
  => KafkaConsumer
  -> Timeout
  -> [(TopicName, PartitionId)]
  -> m (Either KafkaError [TopicPartition])
committed KafkaConsumer{kcKafkaPtr = KafkaConsumerHandle consumer} _ partitions = liftIO $ do
  results <- traverse (committedOne consumer) partitions
  pure (sequence results)
committed _ _ _ =
  pure (Left (KafkaBadSpecification "KafkaConsumer does not contain a consumer handle"))

position
  :: MonadIO m
  => KafkaConsumer
  -> [(TopicName, PartitionId)]
  -> m (Either KafkaError [TopicPartition])
position KafkaConsumer{kcKafkaPtr = KafkaConsumerHandle consumer} partitions = liftIO $ do
  results <- traverse (positionOne consumer) partitions
  pure (sequence results)
position _ _ =
  pure (Left (KafkaBadSpecification "KafkaConsumer does not contain a consumer handle"))

pollConsumerEvents :: KafkaConsumer -> Maybe Timeout -> IO ()
pollConsumerEvents _ _ = pure ()

closeConsumer :: MonadIO m => KafkaConsumer -> m (Maybe KafkaError)
closeConsumer KafkaConsumer{kcKafkaPtr = KafkaConsumerHandle consumer} =
  WF.closeConsumer consumer >> pure Nothing
closeConsumer _ = pure Nothing

rewindConsumer :: MonadIO m => KafkaConsumer -> Timeout -> m (Maybe KafkaError)
rewindConsumer kc timeout = do
  assigned <- assignment kc
  case assigned of
    Left err -> pure (Just err)
    Right partsByTopic -> do
      let pairs = concatMap expand (M.toList partsByTopic)
      committedOffsets <- committed kc timeout pairs
      case committedOffsets of
        Left err -> pure (Just err)
        Right offsets -> seekPartitions kc offsets timeout
  where
    expand (topic, partitions) = map (\partition -> (topic, partition)) partitions

pollWireform :: KafkaConsumer -> Timeout -> IO (Either KafkaError [WF.ConsumerRecord])
pollWireform KafkaConsumer{kcKafkaPtr = KafkaConsumerHandle consumer} (Timeout timeoutMs) = do
  result <- WF.poll consumer timeoutMs
  pure $ case result of
    Left err -> Left (KafkaError (T.pack err))
    Right records -> Right records
pollWireform _ _ =
  pure (Left (KafkaBadSpecification "KafkaConsumer does not contain a consumer handle"))

commitResult :: MonadIO m => OffsetCommit -> KafkaConsumer -> m (Maybe KafkaError)
commitResult mode KafkaConsumer{kcKafkaPtr = KafkaConsumerHandle consumer} = liftIO $ do
  result <- case mode of
    OffsetCommit -> WF.commitSync consumer
    OffsetCommitAsync -> WF.commitAsync consumer
  pure $ case result of
    Left err -> Just (KafkaError (T.pack err))
    Right () -> Nothing
commitResult _ _ =
  pure (Just (KafkaBadSpecification "KafkaConsumer does not contain a consumer handle"))

seekOne :: WF.Consumer -> TopicPartition -> IO (Maybe KafkaError)
seekOne consumer tp =
  case tpOffset tp of
    PartitionOffsetBeginning ->
      resultToMaybe <$> WF.seekToBeginning consumer [toWireformTopicPartition tp]
    PartitionOffsetEnd ->
      resultToMaybe <$> WF.seekToEnd consumer [toWireformTopicPartition tp]
    PartitionOffset offset ->
      resultToMaybe <$> WF.seek consumer (toWireformTopicPartition tp) offset
    PartitionOffsetStored ->
      pure Nothing
    PartitionOffsetInvalid ->
      pure (Just (KafkaBadSpecification "Cannot seek to PartitionOffsetInvalid"))

committedOne :: WF.Consumer -> (TopicName, PartitionId) -> IO (Either KafkaError TopicPartition)
committedOne consumer pair = do
  let wfTp = pairToWireformTopicPartition pair
  result <- WF.committed consumer wfTp
  pure $ case result of
    Left err -> Left (KafkaError (T.pack err))
    Right offset -> Right (fromWireformTopicPartition wfTp (PartitionOffset offset))

positionOne :: WF.Consumer -> (TopicName, PartitionId) -> IO (Either KafkaError TopicPartition)
positionOne consumer pair = do
  let wfTp = pairToWireformTopicPartition pair
  result <- WF.position consumer wfTp
  pure $ case result of
    Left err -> Left (KafkaError (T.pack err))
    Right offset -> Right (fromWireformTopicPartition wfTp (PartitionOffset offset))

consumerConfig :: ConsumerProperties -> Subscription -> WF.ConsumerConfig
consumerConfig ConsumerProperties{..} (Subscription _ subProps) =
  WF.defaultConsumerConfig
    { WF.consumerClientId = M.findWithDefault "hw-kafka-client" "client.id" cpProps
    , WF.consumerGroupId = M.findWithDefault "default-group" "group.id" cpProps
    , WF.consumerAutoCommit = lookupBool "enable.auto.commit" cpProps True
    , WF.consumerEnableAutoOffsetStore = lookupBool "enable.auto.offset.store" cpProps True
    , WF.consumerAutoCommitIntervalMs =
        lookupInt "auto.commit.interval.ms" cpProps (WF.consumerAutoCommitIntervalMs WF.defaultConsumerConfig)
    , WF.consumerAutoOffsetReset =
        case M.lookup "auto.offset.reset" subProps <|> M.lookup "auto.offset.reset" cpProps of
          Just "earliest" -> WF.Earliest
          Just "latest" -> WF.Latest
          _ -> WF.consumerAutoOffsetReset WF.defaultConsumerConfig
    , WF.consumerQueuedMaxMessagesKbytes =
        lookupInt "queued.max.messages.kbytes" cpProps
          (WF.consumerQueuedMaxMessagesKbytes WF.defaultConsumerConfig)
    }

bootstrapServers :: M.Map T.Text T.Text -> [T.Text]
bootstrapServers props =
  case M.lookup "bootstrap.servers" props of
    Nothing -> []
    Just brokers -> filter (not . T.null) (T.splitOn "," brokers)

lookupInt :: T.Text -> M.Map T.Text T.Text -> Int -> Int
lookupInt key props fallback =
  maybe fallback fromIntegral (textDecimal =<< M.lookup key props)

lookupBool :: T.Text -> M.Map T.Text T.Text -> Bool -> Bool
lookupBool key props fallback =
  case M.lookup key props of
    Just "true" -> True
    Just "false" -> False
    _ -> fallback

fromWireformRecord :: WF.ConsumerRecord -> ConsumerRecord (Maybe ByteString) (Maybe ByteString)
fromWireformRecord WF.ConsumerRecord{..} =
  ConsumerRecord
    { crTopic = TopicName topic
    , crPartition = PartitionId (fromIntegral partition)
    , crOffset = Offset offset
    , crTimestamp = CreateTime (Millis timestamp)
    , crHeaders = headersFromList (map convertHeader headers)
    , crKey = key
    , crValue = Just value
    }

toWireformTopicPartition :: TopicPartition -> WF.TopicPartition
toWireformTopicPartition TopicPartition{..} =
  WF.TopicPartition (unTopicName tpTopicName) (fromIntegral (unPartitionId tpPartition))

pairToWireformTopicPartition :: (TopicName, PartitionId) -> WF.TopicPartition
pairToWireformTopicPartition (TopicName topic, PartitionId partition) =
  WF.TopicPartition topic (fromIntegral partition)

fromWireformTopicPartition :: WF.TopicPartition -> PartitionOffset -> TopicPartition
fromWireformTopicPartition WF.TopicPartition{..} offset =
  TopicPartition
    { tpTopicName = TopicName topic
    , tpPartition = PartitionId (fromIntegral partition)
    , tpOffset = offset
    }

topicPartitionFromRecord :: ConsumerRecord k v -> TopicPartition
topicPartitionFromRecord record =
  TopicPartition
    { tpTopicName = crTopic record
    , tpPartition = crPartition record
    , tpOffset = PartitionOffset (unOffset (crOffset record) + 1)
    }

insertPartition :: WF.TopicPartition -> Map TopicName [PartitionId] -> Map TopicName [PartitionId]
insertPartition WF.TopicPartition{..} =
  M.insertWith (<>) (TopicName topic) [PartitionId (fromIntegral partition)]

convertHeader :: (T.Text, ByteString) -> (ByteString, ByteString)
convertHeader (name, value) = (TE.encodeUtf8 name, value)

resultToMaybe :: Either String () -> Maybe KafkaError
resultToMaybe = either (Just . KafkaError . T.pack) (const Nothing)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (Just x:_) = Just x

maybeToLeft :: Maybe a -> Either a ()
maybeToLeft Nothing = Right ()
maybeToLeft (Just x) = Left x

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _ = Just x
Nothing <|> y = y
