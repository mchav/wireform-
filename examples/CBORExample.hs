{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.ByteString as BS
import CBOR.Class (ToCBOR, FromCBOR, encodeCBOR, decodeCBOR)

data Measurement = Measurement
  { sensor :: !Text
  , value  :: !Double
  , unit   :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToCBOR, FromCBOR)

main :: IO ()
main = do
  let m = Measurement "temperature" 23.5 "celsius"

  let bytes = encodeCBOR m
  putStrLn $ "Encoded Measurement to " ++ show (BS.length bytes) ++ " bytes"

  case decodeCBOR bytes of
    Right decoded -> putStrLn $ "Decoded: " ++ show (decoded :: Measurement)
    Left err      -> putStrLn $ "Error: " ++ err

  putStrLn $ "Roundtrip: " ++ show (decodeCBOR (encodeCBOR m) == Right m)
