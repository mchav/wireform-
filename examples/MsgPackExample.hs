{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString qualified as BS
import Data.Text (Text)
import GHC.Generics (Generic)
import MsgPack.Class (FromMsgPack, ToMsgPack, decodeMsgPack, encodeMsgPack)


data Person = Person
  { name :: !Text
  , age :: !Int
  , email :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToMsgPack, FromMsgPack)


main :: IO ()
main = do
  let alice = Person "Alice" 30 "alice@example.com"

  let bytes = encodeMsgPack alice
  putStrLn $ "Encoded Person to " ++ show (BS.length bytes) ++ " bytes"

  case decodeMsgPack bytes of
    Right decoded -> putStrLn $ "Decoded: " ++ show (decoded :: Person)
    Left err -> putStrLn $ "Error: " ++ err

  putStrLn $ "Roundtrip: " ++ show (decodeMsgPack (encodeMsgPack alice) == Right alice)

-- ---------------------------------------------------------------------------
-- Alternative approaches
-- ---------------------------------------------------------------------------

-- MsgPack is schema-less, so Generic deriving is the primary approach.
-- No TH/QQ needed — just derive ToMsgPack/FromMsgPack.
