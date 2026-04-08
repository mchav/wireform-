{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.ByteString as BS
import XML.Class (ToXML, FromXML, encodeXML, decodeXML)

data Book = Book
  { title  :: !Text
  , author :: !Text
  , year   :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToXML, FromXML)

main :: IO ()
main = do
  let book = Book "The Art of War" "Sun Tzu" (-500)

  let xml = encodeXML book
  putStrLn $ "XML encoded to " ++ show (BS.length xml) ++ " bytes"

  case decodeXML xml of
    Right decoded -> putStrLn $ "Decoded: " ++ show (decoded :: Book)
    Left err      -> putStrLn $ "Error: " ++ err

  putStrLn $ "Roundtrip: " ++ show (decodeXML (encodeXML book) == Right book)
