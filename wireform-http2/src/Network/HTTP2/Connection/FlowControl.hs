module Network.HTTP2.Connection.FlowControl
  ( FlowControl
  , newFlowControl
  , consumeWindow
  , releaseWindow
  , availableWindow
  , updateInitialWindowSize
  ) where

import Control.Concurrent.STM
import Data.Int

data FlowControl = FlowControl
  { fcWindow :: !(TVar Int32)
  }

newFlowControl :: Int32 -> STM FlowControl
newFlowControl initial = FlowControl <$> newTVar initial

{-# INLINE consumeWindow #-}
consumeWindow :: FlowControl -> Int32 -> STM Bool
consumeWindow fc amount = do
  w <- readTVar (fcWindow fc)
  if w >= amount
    then do
      writeTVar (fcWindow fc) (w - amount)
      pure True
    else pure False

{-# INLINE releaseWindow #-}
releaseWindow :: FlowControl -> Int32 -> STM (Either Int32 ())
releaseWindow fc amount = do
  w <- readTVar (fcWindow fc)
  let w64 = fromIntegral w + fromIntegral amount :: Int64
  if w64 > 2147483647
    then pure (Left (fromIntegral w64))
    else do
      writeTVar (fcWindow fc) (fromIntegral w64)
      pure (Right ())

{-# INLINE availableWindow #-}
availableWindow :: FlowControl -> STM Int32
availableWindow fc = readTVar (fcWindow fc)

updateInitialWindowSize :: FlowControl -> Int32 -> Int32 -> STM (Either Int32 ())
updateInitialWindowSize fc oldSize newSize = do
  w <- readTVar (fcWindow fc)
  let diff = fromIntegral newSize - fromIntegral oldSize :: Int64
      w64 = fromIntegral w + diff
  if w64 > 2147483647
    then pure (Left (fromIntegral w64))
    else do
      writeTVar (fcWindow fc) (fromIntegral w64)
      pure (Right ())
