module Network.HTTP2.Internal.BitOps
  ( readWord16BE
  , readWord32BE
  , readWord24BE
  , writeWord16BE
  , writeWord32BE
  , writeWord24BE
  ) where

import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.Storable

{-# INLINE readWord16BE #-}
readWord16BE :: Ptr Word8 -> IO Word16
readWord16BE p = do
  b0 <- peekByteOff p 0 :: IO Word8
  b1 <- peekByteOff p 1 :: IO Word8
  pure $! (fromIntegral b0 `unsafeShiftL` 8) .|. fromIntegral b1

{-# INLINE readWord32BE #-}
readWord32BE :: Ptr Word8 -> IO Word32
readWord32BE p = do
  b0 <- peekByteOff p 0 :: IO Word8
  b1 <- peekByteOff p 1 :: IO Word8
  b2 <- peekByteOff p 2 :: IO Word8
  b3 <- peekByteOff p 3 :: IO Word8
  pure $! (fromIntegral b0 `unsafeShiftL` 24)
    .|. (fromIntegral b1 `unsafeShiftL` 16)
    .|. (fromIntegral b2 `unsafeShiftL` 8)
    .|. fromIntegral b3

{-# INLINE readWord24BE #-}
readWord24BE :: Ptr Word8 -> IO Word32
readWord24BE p = do
  b0 <- peekByteOff p 0 :: IO Word8
  b1 <- peekByteOff p 1 :: IO Word8
  b2 <- peekByteOff p 2 :: IO Word8
  pure $! (fromIntegral b0 `unsafeShiftL` 16)
    .|. (fromIntegral b1 `unsafeShiftL` 8)
    .|. fromIntegral b2

{-# INLINE writeWord16BE #-}
writeWord16BE :: Ptr Word8 -> Word16 -> IO ()
writeWord16BE p w = do
  pokeByteOff p 0 (fromIntegral (w `unsafeShiftR` 8) :: Word8)
  pokeByteOff p 1 (fromIntegral w :: Word8)

{-# INLINE writeWord32BE #-}
writeWord32BE :: Ptr Word8 -> Word32 -> IO ()
writeWord32BE p w = do
  pokeByteOff p 0 (fromIntegral (w `unsafeShiftR` 24) :: Word8)
  pokeByteOff p 1 (fromIntegral (w `unsafeShiftR` 16) :: Word8)
  pokeByteOff p 2 (fromIntegral (w `unsafeShiftR` 8) :: Word8)
  pokeByteOff p 3 (fromIntegral w :: Word8)

{-# INLINE writeWord24BE #-}
writeWord24BE :: Ptr Word8 -> Word32 -> IO ()
writeWord24BE p w = do
  pokeByteOff p 0 (fromIntegral (w `unsafeShiftR` 16) :: Word8)
  pokeByteOff p 1 (fromIntegral (w `unsafeShiftR` 8) :: Word8)
  pokeByteOff p 2 (fromIntegral w :: Word8)
