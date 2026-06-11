{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BSON.Class (FromBSON, ToBSON, decodeBSON, encodeBSON)
import Data.ByteString qualified as BS
import Data.Text (Text)
import GHC.Generics (Generic)


data User = User
  { username :: !Text
  , score :: !Int
  , active :: !Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToBSON, FromBSON)


main :: IO ()
main = do
  let user = User "alice" 42 True

  let bytes = encodeBSON user
  putStrLn $ "Encoded User to " ++ show (BS.length bytes) ++ " bytes"

  case decodeBSON bytes of
    Right decoded -> putStrLn $ "Decoded: " ++ show (decoded :: User)
    Left err -> putStrLn $ "Error: " ++ err

  putStrLn $ "Roundtrip: " ++ show (decodeBSON (encodeBSON user) == Right user)
