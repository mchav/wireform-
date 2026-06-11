{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString qualified as BS
import Data.Text (Text)
import GHC.Generics (Generic)
import Ion.Class (FromIon, ToIon, decodeIon, encodeIon)


data Event = Event
  { eventType :: !Text
  , timestamp :: !Int
  , payload :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToIon, FromIon)


main :: IO ()
main = do
  let evt = Event "click" 1700000000 "button-submit"

  let bytes = encodeIon evt
  putStrLn $ "Encoded Event to " ++ show (BS.length bytes) ++ " bytes"

  case decodeIon bytes of
    Right decoded -> putStrLn $ "Decoded: " ++ show (decoded :: Event)
    Left err -> putStrLn $ "Error: " ++ err

  putStrLn $ "Roundtrip: " ++ show (decodeIon (encodeIon evt) == Right evt)

-- ---------------------------------------------------------------------------
-- Alternative approaches
-- ---------------------------------------------------------------------------

-- Ion is schema-less. Generic deriving is primary.
-- For schema-driven Ion, use ISL:
--   [isl|
--     type::{ name: person, type: struct, fields: { name: string, age: int } }
--   |]
