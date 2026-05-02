{-# LANGUAGE BangPatterns #-}
-- | NDJSON (Newline-Delimited JSON) decoder.
--
-- Uses 'Wireform.FFI.findByteBS' to find newline boundaries in
-- 16-byte chunks via SIMD, then delegates each line to aeson
-- for JSON parsing.
module NDJSON.Decode
  ( decode
  , decodeStream
  , decodeRecords
  , decodeConcurrent
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import qualified Data.Aeson as Aeson
import qualified Wireform.FFI as WF

-- | Fast newline scan. Wraps 'Wireform.FFI.findByteBS' with the
-- @0x0A@ target pinned so call sites read cleanly.
findNewline :: ByteString -> Int -> Int
findNewline bs off = WF.findByteBS bs off 0x0A
{-# INLINE findNewline #-}

decode :: ByteString -> Either String (Vector Aeson.Value)
decode bs = do
  let !lines_ = splitLines bs
  V.mapM parseLine lines_

decodeStream :: ByteString -> (Aeson.Value -> IO ()) -> IO (Either String ())
decodeStream bs callback = do
  let !len = BS.length bs
  go 0 len
  where
    go !off !len
      | off >= len = pure (Right ())
      | otherwise =
          let !nlPos = findNewline bs off
              !lineLen = nlPos - off
          in if lineLen == 0
               then go (nlPos + 1) len
               else do
                 let !line = BSU.unsafeTake lineLen (BSU.unsafeDrop off bs)
                 case Aeson.eitherDecodeStrict' line of
                   Left err  -> pure (Left err)
                   Right val -> do
                     callback val
                     go (nlPos + 1) len

decodeRecords :: Aeson.FromJSON a => ByteString -> Either String (Vector a)
decodeRecords bs = do
  let !lines_ = splitLines bs
  V.mapM (\line -> case Aeson.eitherDecodeStrict' line of
    Left err  -> Left err
    Right val -> Right val) lines_

decodeConcurrent :: ByteString -> Int -> (Aeson.Value -> IO ()) -> IO (Either String ())
decodeConcurrent bs bufSize callback = do
  chan <- newTBQueueIO (fromIntegral bufSize)
  resultVar <- newTVarIO (Right ())

  _ <- forkIO $ do
    let !len = BS.length bs
        produce !off
          | off >= len = atomically $ writeTBQueue chan Nothing
          | otherwise =
              let !nlPos = findNewline bs off
                  !lineLen = nlPos - off
              in if lineLen == 0
                   then produce (nlPos + 1)
                   else do
                     let !line = BSU.unsafeTake lineLen (BSU.unsafeDrop off bs)
                     case Aeson.eitherDecodeStrict' line of
                       Left err -> do
                         atomically $ do
                           writeTVar resultVar (Left err)
                           writeTBQueue chan Nothing
                       Right val -> do
                         atomically $ writeTBQueue chan (Just val)
                         produce (nlPos + 1)
    produce 0

  let consume = do
        mval <- atomically $ readTBQueue chan
        case mval of
          Nothing  -> atomically $ readTVar resultVar
          Just val -> do
            callback val
            consume
  consume

splitLines :: ByteString -> Vector ByteString
splitLines bs = V.create $ do
  let !len = BS.length bs
      initCap = max 16 (len `div` 60)
  mv <- MV.new initCap
  let go !off !count !cap !vec
        | off >= len = pure (vec, count)
        | otherwise =
            let !nlPos = findNewline bs off
                !lineLen = nlPos - off
            in if lineLen == 0
                 then go (nlPos + 1) count cap vec
                 else do
                   let !line = BSU.unsafeTake lineLen (BSU.unsafeDrop off bs)
                   vec' <- if count >= cap
                           then MV.grow vec cap
                           else pure vec
                   let cap' = if count >= cap then cap * 2 else cap
                   MV.write vec' count line
                   go (nlPos + 1) (count + 1) cap' vec'
  (vec, count) <- go 0 0 initCap mv
  pure (MV.take count vec)

parseLine :: ByteString -> Either String Aeson.Value
parseLine line = case Aeson.eitherDecodeStrict' line of
  Left err  -> Left err
  Right val -> Right val
