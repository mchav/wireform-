{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text qualified as T
import EDN.Class (FromEDN, ToEDN, decodeEDN, encodeEDN)
import GHC.Generics (Generic)


data Config = Config
  { host :: !Text
  , port :: !Int
  , debug :: !Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToEDN, FromEDN)


main :: IO ()
main = do
  let cfg = Config "localhost" 8080 True

  let text = encodeEDN cfg
  putStrLn $ "EDN:\n" ++ T.unpack text

  case decodeEDN text of
    Right decoded -> putStrLn $ "Decoded: " ++ show (decoded :: Config)
    Left err -> putStrLn $ "Error: " ++ err

  putStrLn $ "Roundtrip: " ++ show (decodeEDN (encodeEDN cfg) == Right cfg)
