{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString qualified as BS
import Data.Text (Text)
import GHC.Generics (Generic)
import XML.Class (FromXML, ToXML, decodeXML, encodeXML)


data Book = Book
  { title :: !Text
  , author :: !Text
  , year :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToXML, FromXML)


main :: IO ()
main = do
  let book = Book "The Art of War" "Sun Tzu" (-500)

  let xml = encodeXML book
  putStrLn $ "XML encoded to " ++ show (BS.length xml) ++ " bytes"

  case decodeXML xml of
    Right decoded -> putStrLn $ "Decoded: " ++ show (decoded :: Book)
    Left err -> putStrLn $ "Error: " ++ err

  putStrLn $ "Roundtrip: " ++ show (decodeXML (encodeXML book) == Right book)

-- ---------------------------------------------------------------------------
-- Alternative approaches
-- ---------------------------------------------------------------------------

-- Approach 1: Generic deriving (as shown above)
--   The simplest way — just derive Generic, ToXML, and FromXML.

-- Approach 2: TH from XSD
--   Use the xsd quasiquoter or deriveXSD to generate types from an XSD schema
--   at compile time:
--
--   [xsd|
--     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
--       <xs:complexType name="Person">
--         <xs:sequence>
--           <xs:element name="name" type="xs:string"/>
--           <xs:element name="age" type="xs:integer"/>
--         </xs:sequence>
--       </xs:complexType>
--     </xs:schema>
--   |]
--
--   This generates:
--     data Person = Person { name :: !Text, age :: !Integer }
--       deriving stock (Show, Eq, Generic)
--       deriving anyclass (ToXML, FromXML)

-- Approach 3: CLI codegen
--   wireform-gen xsd -i schema.xsd -o src/Gen/
