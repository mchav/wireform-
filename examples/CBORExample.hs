{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import CBOR.Class (FromCBOR, ToCBOR, decodeCBOR, encodeCBOR)
import Data.ByteString qualified as BS
import Data.Text (Text)
import GHC.Generics (Generic)


data Measurement = Measurement
  { sensor :: !Text
  , value :: !Double
  , unit :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToCBOR, FromCBOR)


main :: IO ()
main = do
  let m = Measurement "temperature" 23.5 "celsius"

  let bytes = encodeCBOR m
  putStrLn $ "Encoded Measurement to " ++ show (BS.length bytes) ++ " bytes"

  case decodeCBOR bytes of
    Right decoded -> putStrLn $ "Decoded: " ++ show (decoded :: Measurement)
    Left err -> putStrLn $ "Error: " ++ err

  putStrLn $ "Roundtrip: " ++ show (decodeCBOR (encodeCBOR m) == Right m)

-- ---------------------------------------------------------------------------
-- Alternative approaches
-- ---------------------------------------------------------------------------

-- CBOR is schema-less. Generic deriving is primary.
-- For schema-driven CBOR, use CDDL:
--   [cddl|
--     person = { name: tstr, age: uint }
--   |]
