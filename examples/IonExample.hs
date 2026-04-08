{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.ByteString as BS
import Ion.Class (ToIon, FromIon, encodeIon, decodeIon)

data Event = Event
  { eventType :: !Text
  , timestamp :: !Int
  , payload   :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToIon, FromIon)

main :: IO ()
main = do
  let evt = Event "click" 1700000000 "button-submit"

  let bytes = encodeIon evt
  putStrLn $ "Encoded Event to " ++ show (BS.length bytes) ++ " bytes"

  case decodeIon bytes of
    Right decoded -> putStrLn $ "Decoded: " ++ show (decoded :: Event)
    Left err      -> putStrLn $ "Error: " ++ err

  putStrLn $ "Roundtrip: " ++ show (decodeIon (encodeIon evt) == Right evt)
