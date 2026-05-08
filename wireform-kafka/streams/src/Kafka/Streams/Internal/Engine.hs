{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Internal.Engine
-- Description : Topology engine — instantiates stores, processors and
--               forwarders for one task
--
-- The engine is the implementation of the topology shape. It is shared
-- by 'Kafka.Streams.Driver' (the synchronous test driver) and
-- 'Kafka.Streams.Runtime' (the broker-backed runtime).
--
-- A single engine corresponds to a single Kafka Streams /task/: a single
-- set of state stores, a single ProcessorContext, a single record
-- collector. The real 'Kafka.Streams.Runtime' instantiates one engine
-- per partition assignment.
--
-- == Type erasure
--
-- The Java engine has erasure for free; we don't. We thread records
-- through the engine as the existential type 'Erased' — backed by
-- @unsafeCoerce@ at every cross-node forwarder. The DSL builders
-- ensure that every parent of a node forwards records of the same
-- /actual/ type, so the coercion is sound. State-store accessors use
-- their own typed serdes and never rely on this trick.
module Kafka.Streams.Internal.Engine
  ( Engine
  , engineTopology
  , engineTaskId
  , engineAppId
  , engineCollector
  , engineStreamTime
  , engineWallClock
  , buildEngine
  , feedSource
  , advanceWallClock
  , advanceStreamTimeTo
  , triggerStreamTimePunctuators
  , triggerWallClockPunctuators
  , commitEngine
  , closeEngine
  , storeByName
  , streamTimeOfEngine
  , wallClockTimeOfEngine
    -- * Internals
  , StoreEntry (..)
  , Erased
  , erase
  , unsafeUnerase
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_)
import Data.ByteString (ByteString)
import Data.IORef
import GHC.Exts (Any)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time.Clock as Clock
import Data.Int (Int64)
import Unsafe.Coerce (unsafeCoerce)

import Kafka.Streams.Errors
  ( DeserializationException (..)
  , DeserializationHandler (..)
  , DeserializationResponse (..)
  )
import Kafka.Streams.Internal.RecordCollector
  ( CollectedRecord (..)
  , RecordCollector (..)
  )
import Kafka.Streams.Processor
  ( Cancellable (..)
  , ProcessorContext (..)
  , Processor (..)
  , ProcessorName (..)
  , PunctuationType (..)
  , Punctuator (..)
  , TaskId (..)
  )
import Kafka.Streams.Serde (Serde (..))
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , KeyValueStore (..)
  , SessionStore (..)
  , StateStore (..)
  , StoreBuilder (..)
  , StoreBuilderKV (..)
  , StoreBuilderS (..)
  , StoreBuilderW (..)
  , StoreName
  , WindowStore (..)
  , unStoreName
  )
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Time
  ( StreamTime (..)
  , Timestamp (..)
  , advanceStreamTime
  , initialStreamTime
  , runTimestampExtractor
  , utcTimeToTimestamp
  )
import Kafka.Streams.Types
  ( Record (..)
  , RecordMetadata (..)
  , TopicName
  , emptyHeaders
  , unTopicName
  )

----------------------------------------------------------------------
-- Type erasure
----------------------------------------------------------------------

-- | An /erased/ value. We use 'GHC.Exts.Any' as the existential
-- carrier when threading records and serdes across the engine. The
-- DSL guarantees that the runtime types match before the engine
-- ever sees them.
type Erased = Any

erase :: a -> Erased
erase = unsafeCoerce
{-# INLINE erase #-}

-- | The DSL guarantees the witness; the engine just does the cast.
unsafeUnerase :: Erased -> a
unsafeUnerase = unsafeCoerce
{-# INLINE unsafeUnerase #-}

-- | The universal forwarder shape used inside the engine.
type NodeForwarder = Record Erased Erased -> IO ()

----------------------------------------------------------------------
-- Engine state
----------------------------------------------------------------------

data StoreEntry = StoreEntry
  { storeEntryName  :: !StoreName
  , storeEntryAny   :: !AnyStateStore
  , storeEntryClose :: !(IO ())
  , storeEntryFlush :: !(IO ())
  }

data ProcessorRecord = ProcessorRecord
  { prClose :: !(IO ())
  , prName  :: !ProcessorName
  }

data PunctuatorEntry = PunctuatorEntry
  { peType        :: !PunctuationType
  , peIntervalMs  :: !Int
  , peNextFireMs  :: !Int64
  , pePunctuator  :: !Punctuator
  , peCancelled   :: !(IORef Bool)
  , peOwner       :: !Topo.NodeName
  }

data SourceHandler = SourceHandler
  { shSourceName :: !Topo.NodeName
  , shTopics     :: ![TopicName]
  , shHandler    :: !(SourceInput -> IO ())
  }

data SourceInput = SourceInput
  { siTopic     :: !TopicName
  , siPartition :: !Int
  , siOffset    :: !Int64
  , siKey       :: !(Maybe ByteString)
  , siValue     :: !ByteString
  , siTimestamp :: !Timestamp
  }

data Engine = Engine
  { engineTopology     :: !Topo.Topology
  , engineTaskId       :: !TaskId
  , engineAppId        :: !Text
  , engineCollector    :: !RecordCollector
  , engineForwarders   :: !(IORef (Map Topo.NodeName NodeForwarder))
  , engineSources      :: !(IORef (Map Topo.NodeName SourceHandler))
  , engineStores       :: !(IORef (Map StoreName StoreEntry))
  , engineProcessors   :: !(IORef (Map Topo.NodeName ProcessorRecord))
  , engineStreamTime   :: !(IORef StreamTime)
  , engineWallClock    :: !(IORef Timestamp)
  , engineCurrentMd    :: !(IORef (Maybe RecordMetadata))
  , enginePunctuators  :: !(IORef [PunctuatorEntry])
  , engineDeserHandler :: !DeserializationHandler
  }

----------------------------------------------------------------------
-- Build
----------------------------------------------------------------------

-- | Realise a topology into a runnable engine. Builds every store,
-- instantiates every processor (calling 'procInit' with the typed
-- 'ProcessorContext'), wires forwarders, and registers every source
-- handler.
buildEngine
  :: Topo.TopologyValid
  -> TaskId
  -> Text
  -> RecordCollector
  -> DeserializationHandler
  -> IO Engine
buildEngine validated tid appId collector deserHandler = do
  let topo = Topo.topologyValidGraph validated

  storeEntries <- forM (Map.toList (Topo.topoStores topo)) $ \(_nm, ab) ->
    realiseStore ab
  storesRef <- newIORef
    (Map.fromList [(storeEntryName se, se) | se <- storeEntries])

  forwardersRef <- newIORef Map.empty
  procsRef      <- newIORef Map.empty
  sourcesRef    <- newIORef Map.empty
  streamRef     <- newIORef initialStreamTime
  wallRef       <- nowAsTimestamp >>= newIORef
  currentMdRef  <- newIORef Nothing
  pesRef        <- newIORef []

  let engine = Engine
        { engineTopology     = topo
        , engineTaskId       = tid
        , engineAppId        = appId
        , engineCollector    = collector
        , engineForwarders   = forwardersRef
        , engineSources      = sourcesRef
        , engineStores       = storesRef
        , engineProcessors   = procsRef
        , engineStreamTime   = streamRef
        , engineWallClock    = wallRef
        , engineCurrentMd    = currentMdRef
        , enginePunctuators  = pesRef
        , engineDeserHandler = deserHandler
        }

  -- Wire processors first so children-of references resolve.
  forM_ (Map.toList (Topo.topoProcessors topo)) $ \(nm, spec) ->
    instantiateProcessor engine nm spec
  forM_ (Map.toList (Topo.topoSinks topo)) $ \(nm, spec) ->
    instantiateSink engine nm spec
  forM_ (Map.toList (Topo.topoSources topo)) $ \(nm, spec) ->
    instantiateSource engine nm spec
  pure engine

----------------------------------------------------------------------
-- Stores
----------------------------------------------------------------------

realiseStore :: Topo.AnyStoreBuilder -> IO StoreEntry
realiseStore = \case
  Topo.AsKeyValueBuilder b -> do
    s <- sbKvBuild b
    pure StoreEntry
      { storeEntryName  = sbKvName b
      , storeEntryAny   = AnyKeyValueStore s
      , storeEntryClose = storeClose (kvsBase s)
      , storeEntryFlush = storeFlush (kvsBase s)
      }
  Topo.AsWindowBuilder b -> do
    s <- sbWBuild b
    pure StoreEntry
      { storeEntryName  = sbWName b
      , storeEntryAny   = AnyWindowStore s
      , storeEntryClose = storeClose (wsBase s)
      , storeEntryFlush = storeFlush (wsBase s)
      }
  Topo.AsSessionBuilder b -> do
    s <- sbSBuild b
    pure StoreEntry
      { storeEntryName  = sbSName b
      , storeEntryAny   = AnySessionStore s
      , storeEntryClose = storeClose (ssBase s)
      , storeEntryFlush = storeFlush (ssBase s)
      }
  Topo.AsRawBuilder b -> do
    s <- sbBuild b
    pure StoreEntry
      { storeEntryName  = sbName b
      , storeEntryAny   = AnyKeyValueStore (rawAsKeyValuePlaceholder s)
      , storeEntryClose = storeClose s
      , storeEntryFlush = storeFlush s
      }

rawAsKeyValuePlaceholder :: StateStore -> KeyValueStore () ()
rawAsKeyValuePlaceholder ss = KeyValueStore
  { kvsBase          = ss
  , kvsGet           = \_   -> opaque
  , kvsPut           = \_ _ -> opaque
  , kvsPutIfAbsent   = \_ _ -> opaque
  , kvsDelete        = \_   -> opaque
  , kvsRange         = \_ _ -> opaque
  , kvsAll           = opaque
  , kvsApproxEntries = pure 0
  }
  where
    opaque :: IO a
    opaque = error
      "Kafka.Streams.Internal.Engine: raw store has no typed key/value accessors"

----------------------------------------------------------------------
-- Processor instantiation
----------------------------------------------------------------------

instantiateProcessor :: Engine -> Topo.NodeName -> Topo.ProcessorSpec -> IO ()
instantiateProcessor engine nm spec =
  case Topo.processorSpecSupplier spec of
    Topo.AnyProcessor mkProc -> do
      proc_ <- mkProc
      let ctx = makeContext engine nm
      procInit proc_ ctx
      atomicModifyIORef' (engineForwarders engine) $ \m ->
        let !m' = Map.insert nm (forwardForProc proc_) m in (m', ())
      atomicModifyIORef' (engineProcessors engine) $ \m ->
        let !m' = Map.insert nm
                    ProcessorRecord
                      { prClose = procClose proc_
                      , prName  = procName proc_
                      } m
         in (m', ())

forwardForProc :: forall k v. Processor k v -> NodeForwarder
forwardForProc p = \rec ->
  -- Cross the erasure boundary: the DSL guarantees that incoming
  -- records have actual type @Record k v@ matching this processor.
  let !typed = Record
        { recordKey       = unsafeUnerase <$> recordKey rec :: Maybe k
        , recordValue     = unsafeUnerase (recordValue rec) :: v
        , recordTimestamp = recordTimestamp rec
        , recordHeaders   = recordHeaders rec
        }
   in procProcess p typed
{-# INLINE forwardForProc #-}

----------------------------------------------------------------------
-- Sinks
----------------------------------------------------------------------

instantiateSink :: Engine -> Topo.NodeName -> Topo.SinkSpec -> IO ()
instantiateSink engine nm spec = do
  let !forwarder = sinkForwarder engine spec
  atomicModifyIORef' (engineForwarders engine) $ \m ->
    let !m' = Map.insert nm forwarder m in (m', ())

sinkForwarder :: Engine -> Topo.SinkSpec -> NodeForwarder
sinkForwarder engine spec = \rec ->
  case (Topo.sinkKeySerde spec, Topo.sinkValueSerde spec) of
    (Topo.AnySerde ks, Topo.AnySerde vs) -> do
      let !serK = serialize (unsafeCoerce ks :: Serde Erased)
          !serV = serialize (unsafeCoerce vs :: Serde Erased)
          !keyB = serK <$> recordKey rec
          !valB = serV (recordValue rec)
          out = CollectedRecord
            { crTopic     = Topo.sinkTopic spec
            , crKey       = keyB
            , crValue     = valB
            , crTimestamp = recordTimestamp rec
            , crHeaders   = recordHeaders rec
            , crPartition = Nothing
            }
      collectorSend (engineCollector engine) out

----------------------------------------------------------------------
-- Sources
----------------------------------------------------------------------

instantiateSource :: Engine -> Topo.NodeName -> Topo.SourceSpec -> IO ()
instantiateSource engine nm spec = do
  -- Resolve children once so we don't pay for the lookup per record.
  fwds <- readIORef (engineForwarders engine)
  let kids = Topo.childrenOf (engineTopology engine) nm
  let childForwarders =
        [ fw | k <- kids, Just fw <- [Map.lookup k fwds] ]
  let handler = handleSource engine spec childForwarders
  atomicModifyIORef' (engineSources engine) $ \m ->
    let !m' = Map.insert nm
                SourceHandler
                  { shSourceName = nm
                  , shTopics     = Topo.sourceTopics spec
                  , shHandler    = handler
                  } m
     in (m', ())

handleSource
  :: Engine
  -> Topo.SourceSpec
  -> [NodeForwarder]
  -> SourceInput
  -> IO ()
handleSource engine spec children si = do
  let mErasedKey = decodeKeyErased (Topo.sourceKeySerde spec) (siKey si)
      eErasedVal = decodeValErased (Topo.sourceValueSerde spec) (siValue si)
  case (mErasedKey, eErasedVal) of
    (Right mk, Right v) -> do
      st <- readIORef (engineStreamTime engine)
      ts <- runErasedExtractor (Topo.sourceExtractor spec)
              mk v (siTimestamp si) st
      atomicModifyIORef' (engineStreamTime engine) $ \cur ->
        (advanceStreamTime ts cur, ())
      writeIORef (engineCurrentMd engine) (Just RecordMetadata
        { rmTopic     = siTopic si
        , rmPartition = fromIntegral (siPartition si)
        , rmOffset    = siOffset si
        })
      let rec = Record
            { recordKey       = mk
            , recordValue     = v
            , recordTimestamp = ts
            , recordHeaders   = emptyHeaders
            }
      mapM_ (\fw -> fw rec) children
      writeIORef (engineCurrentMd engine) Nothing
    _ -> handleDeserError engine si mErasedKey eErasedVal

-- | Decode the source's key bytes via its 'AnySerde' into a fully
-- erased 'Maybe Erased'. The unsafeCoerce is sound because the
-- topology builder paired this 'Serde' with the same 'k' that
-- downstream processors will receive.
decodeKeyErased
  :: Topo.AnySerde -> Maybe ByteString -> Either String (Maybe Erased)
decodeKeyErased _ Nothing = Right Nothing
decodeKeyErased (Topo.AnySerde s) (Just kb) =
  case deserialize s kb of
    Left e  -> Left e
    Right k -> Right (Just (erase k))

decodeValErased
  :: Topo.AnySerde -> ByteString -> Either String Erased
decodeValErased (Topo.AnySerde s) bs =
  fmap erase (deserialize s bs)

-- | Run a type-erased 'TimestampExtractor'. Both the @Maybe k@ and
-- @v@ have already been erased; we coerce back to the extractor's
-- declared type at the boundary.
runErasedExtractor
  :: Topo.AnyTimestampExtractor
  -> Maybe Erased
  -> Erased
  -> Timestamp
  -> StreamTime
  -> IO Timestamp
runErasedExtractor (Topo.AnyTimestampExtractor ex) mk v ts st =
  runTimestampExtractor
    ex
    (unsafeUnerase <$> mk)
    (unsafeUnerase v)
    ts
    st

handleDeserError
  :: forall a b
   . Engine
  -> SourceInput
  -> Either String a
  -> Either String b
  -> IO ()
handleDeserError engine si keyR valR = do
  let reason = case (keyR, valR) of
        (Left e, _) -> "key: " <> T.pack e
        (_, Left e) -> "value: " <> T.pack e
        _           -> "unknown"
      ex = DeserializationException
        { deserTopic     = unTopicName (siTopic si)
        , deserPartition = fromIntegral (siPartition si)
        , deserOffset    = fromIntegral (siOffset si)
        , deserKey       = siKey si
        , deserValue     = siValue si
        , deserReason    = reason
        }
  resp <- runDeserializationHandler (engineDeserHandler engine) ex
  case resp of
    DeserContinueProcessing -> pure ()
    DeserFailFast           ->
      error $ "Kafka.Streams: deserialization failure: " <> T.unpack reason

----------------------------------------------------------------------
-- ProcessorContext
----------------------------------------------------------------------

makeContext :: Engine -> Topo.NodeName -> ProcessorContext
makeContext engine selfNm = ProcessorContext
  { ctxApplicationId  = engineAppId engine
  , ctxTaskId         = engineTaskId engine
  , ctxRecordMetadata = readIORef (engineCurrentMd engine)
  , ctxStreamTime     = streamTimeOfEngine engine
  , ctxWallClockTime  = wallClockTimeOfEngine engine
  , ctxForward = \rec -> do
      let !erased = eraseRecord rec
      fws <- childForwardersFor engine selfNm
      mapM_ ($ erased) fws
  , ctxForwardTo = \child rec -> do
      let !erased = eraseRecord rec
      fwds <- readIORef (engineForwarders engine)
      case Map.lookup child fwds of
        Just fw -> fw erased
        Nothing ->
          error $
            "Kafka.Streams: forward to unknown node "
              <> T.unpack (Topo.unNodeName child)
  , ctxSchedule = \intervalMs ptype pun -> do
      cancelRef <- newIORef False
      now <- case ptype of
        StreamTimePunctuation    -> unTimestamp <$> streamTimeOfEngine engine
        WallClockTimePunctuation -> unTimestamp <$> wallClockTimeOfEngine engine
      let entry = PunctuatorEntry
            { peType        = ptype
            , peIntervalMs  = intervalMs
            , peNextFireMs  = now + fromIntegral intervalMs
            , pePunctuator  = pun
            , peCancelled   = cancelRef
            , peOwner       = selfNm
            }
      atomicModifyIORef' (enginePunctuators engine) $ \xs -> (entry : xs, ())
      pure Cancellable { cancel = writeIORef cancelRef True }
  , ctxGetStore = \sn -> do
      m <- readIORef (engineStores engine)
      pure (storeEntryAny <$> Map.lookup sn m)
  }

eraseRecord :: Record k v -> Record Erased Erased
eraseRecord r = Record
  { recordKey       = erase <$> recordKey r
  , recordValue     = erase (recordValue r)
  , recordTimestamp = recordTimestamp r
  , recordHeaders   = recordHeaders r
  }

childForwardersFor :: Engine -> Topo.NodeName -> IO [NodeForwarder]
childForwardersFor engine nm = do
  m <- readIORef (engineForwarders engine)
  let kids = Topo.childrenOf (engineTopology engine) nm
  pure [fw | k <- kids, Just fw <- [Map.lookup k m]]

----------------------------------------------------------------------
-- Driving
----------------------------------------------------------------------

feedSource
  :: Engine
  -> TopicName
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> Int
  -> Int64
  -> IO ()
feedSource engine topic key value ts part off = do
  shs <- readIORef (engineSources engine)
  let matching =
        [ sh
        | sh <- Map.elems shs
        , topic `elem` shTopics sh
        ]
  case matching of
    [] -> pure ()
    _  -> do
      let si = SourceInput
            { siTopic     = topic
            , siPartition = part
            , siOffset    = off
            , siKey       = key
            , siValue     = value
            , siTimestamp = ts
            }
      mapM_ (\sh -> shHandler sh si) matching
      triggerStreamTimePunctuators engine

advanceWallClock :: Engine -> Int64 -> IO ()
advanceWallClock engine deltaMs = do
  atomicModifyIORef' (engineWallClock engine) $ \(Timestamp t) ->
    (Timestamp (t + deltaMs), ())
  triggerWallClockPunctuators engine

advanceStreamTimeTo :: Engine -> Timestamp -> IO ()
advanceStreamTimeTo engine ts = do
  atomicModifyIORef' (engineStreamTime engine) $ \cur ->
    (advanceStreamTime ts cur, ())
  triggerStreamTimePunctuators engine

triggerStreamTimePunctuators :: Engine -> IO ()
triggerStreamTimePunctuators engine = do
  StreamTime (Timestamp now) <- readIORef (engineStreamTime engine)
  fireDue engine StreamTimePunctuation now

triggerWallClockPunctuators :: Engine -> IO ()
triggerWallClockPunctuators engine = do
  Timestamp now <- readIORef (engineWallClock engine)
  fireDue engine WallClockTimePunctuation now

fireDue :: Engine -> PunctuationType -> Int64 -> IO ()
fireDue engine pt now = do
  pes <- readIORef (enginePunctuators engine)
  newPes <- forM pes $ \pe ->
    if peType pe /= pt
      then pure pe
      else do
        wasCancelled <- readIORef (peCancelled pe)
        if wasCancelled
          then pure pe
          else
            if peNextFireMs pe <= now
              then do
                runPunctuator (pePunctuator pe) (Timestamp now)
                pure pe
                  { peNextFireMs =
                      peNextFireMs pe + fromIntegral (peIntervalMs pe)
                  }
              else pure pe
  writeIORef (enginePunctuators engine) newPes

----------------------------------------------------------------------
-- Lifecycle
----------------------------------------------------------------------

commitEngine :: Engine -> IO ()
commitEngine engine = do
  m <- readIORef (engineStores engine)
  forM_ (Map.elems m) $ \se -> do
    eflush <- try (storeEntryFlush se) :: IO (Either SomeException ())
    case eflush of
      Left e -> putStrLn ("[streams] store flush failed: " <> show e)
      _      -> pure ()
  collectorFlush (engineCollector engine)

closeEngine :: Engine -> IO ()
closeEngine engine = do
  ps <- readIORef (engineProcessors engine)
  forM_ (Map.toList ps) $ \(_nm, pr) -> do
    eclose <- try (prClose pr) :: IO (Either SomeException ())
    case eclose of
      Left e -> putStrLn $
        "[streams] processor close failed ("
          <> T.unpack (unProcessorName (prName pr)) <> "): " <> show e
      _      -> pure ()
  ses <- readIORef (engineStores engine)
  forM_ (Map.elems ses) $ \se -> do
    eclose <- try (storeEntryClose se) :: IO (Either SomeException ())
    case eclose of
      Left e -> putStrLn $
        "[streams] store close failed ("
          <> T.unpack (unStoreName (storeEntryName se)) <> "): " <> show e
      _      -> pure ()
  collectorClose (engineCollector engine)

storeByName :: Engine -> StoreName -> IO (Maybe StoreEntry)
storeByName engine sn = Map.lookup sn <$> readIORef (engineStores engine)

streamTimeOfEngine :: Engine -> IO Timestamp
streamTimeOfEngine engine =
  unStreamTime <$> readIORef (engineStreamTime engine)

wallClockTimeOfEngine :: Engine -> IO Timestamp
wallClockTimeOfEngine engine = readIORef (engineWallClock engine)

nowAsTimestamp :: IO Timestamp
nowAsTimestamp = utcTimeToTimestamp <$> Clock.getCurrentTime
