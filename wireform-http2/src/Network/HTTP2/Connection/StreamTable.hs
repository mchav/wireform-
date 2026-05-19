module Network.HTTP2.Connection.StreamTable
  ( StreamTable (..)
  , StreamState (..)
  , ClosedReason (..)
  , Stream (..)
  , newStreamTable
  , lookupStream
  , insertStream
  , removeStream
  , updateStreamState
  , activeStreamCount
  , allStreams
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Word

import Network.HTTP2.Connection.FlowControl
import Network.HTTP2.Types

data StreamState
  = StreamIdle
  | StreamReservedLocal
  | StreamReservedRemote
  | StreamOpen
  | StreamHalfClosedLocal
  | StreamHalfClosedRemote
  | StreamClosed !ClosedReason
  deriving stock (Eq, Show)

data ClosedReason
  = ClosedByEndStream
  | ClosedByReset !ErrorCode
  | ClosedByGoAway
  deriving stock (Eq, Show)

data Stream = Stream
  { streamId :: !StreamId
  , streamState :: !(TVar StreamState)
  , streamFlowControl :: !FlowControl
  , streamHeaderBuffer :: !(TVar ByteString)
  , streamRecvWindow :: !FlowControl
  }

data StreamTable = StreamTable
  { stStreams :: !(TVar (Map.Map StreamId Stream))
  , stNextStreamId :: !(TVar StreamId)
  }

newStreamTable :: Bool -> IO StreamTable
newStreamTable isServer = do
  streams <- newTVarIO Map.empty
  nextId <- newTVarIO (if isServer then 2 else 1)
  pure StreamTable
    { stStreams = streams
    , stNextStreamId = nextId
    }

lookupStream :: StreamTable -> StreamId -> STM (Maybe Stream)
lookupStream st sid = do
  streams <- readTVar (stStreams st)
  pure (Map.lookup sid streams)

insertStream :: StreamTable -> Stream -> STM ()
insertStream st stream = do
  modifyTVar' (stStreams st) (Map.insert (streamId stream) stream)

removeStream :: StreamTable -> StreamId -> STM ()
removeStream st sid = do
  modifyTVar' (stStreams st) (Map.delete sid)

updateStreamState :: Stream -> StreamState -> STM ()
updateStreamState stream newState = writeTVar (streamState stream) newState

activeStreamCount :: StreamTable -> STM Int
activeStreamCount st = do
  streams <- readTVar (stStreams st)
  let isActive s = do
        state <- readTVar (streamState s)
        pure $ case state of
          StreamOpen -> True
          StreamHalfClosedLocal -> True
          StreamHalfClosedRemote -> True
          _ -> False
  count <- traverse isActive (Map.elems streams)
  pure (length (filter id count))

allStreams :: StreamTable -> STM [Stream]
allStreams st = Map.elems <$> readTVar (stStreams st)
