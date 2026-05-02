{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

-- | One Haskell record, four wire formats.
--
-- This example shows how a single ANN-annotated data type drives the
-- generation of @Proto@, @CBOR@, @MsgPack@, and @JSON@ instances at
-- compile time using the shared @wireform-derive@ vocabulary. Run it
-- with:
--
-- @cabal run example-derive@
--
-- The same modifier ('rename', 'tag', 'skip', 'forBackend', ...) is
-- consulted by every per-format deriver, so changing a wire-key here
-- updates every wire format simultaneously.
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Word (Word32)
import GHC.Generics (Generic)

import qualified Data.Aeson as A
import qualified Proto.Decode as PD
import qualified Proto.Encode as PE

import qualified CBOR.Class as CBOR
import qualified MsgPack.Class as MP

import Wireform.Derive
  ( backendJSON
  , forBackend
  , rename
  , renameStyle
  , NameStyle (..)
  , skip
  , tag
  )
import qualified Wireform.Derive.Aeson as DAeson
import qualified CBOR.Derive    as DCBOR
import qualified MsgPack.Derive as DMP
import qualified Proto.Derive   as DProto

-- ---------------------------------------------------------------------------
-- Shared annotations
-- ---------------------------------------------------------------------------

data Person = Person
  { personFullName :: !Text
  , personAge      :: !Word32
  , personBalance  :: !Int64
  , personSecret   :: !Text
    -- ^ Skipped only for JSON; preserved on the binary wires.
  } deriving stock (Show, Eq, Generic)

{-# ANN type Person ("Person" :: String) #-}

-- Proto requires explicit field numbers.
{-# ANN personFullName (tag 1) #-}
{-# ANN personAge      (tag 2) #-}
{-# ANN personBalance  (tag 3) #-}
{-# ANN personSecret   (tag 4) #-}

-- Default snake_case for every backend...
{-# ANN personFullName (renameStyle SnakeCase) #-}
{-# ANN personAge      (renameStyle SnakeCase) #-}
{-# ANN personBalance  (renameStyle SnakeCase) #-}
{-# ANN personSecret   (renameStyle SnakeCase) #-}

-- ...except JSON, which gets the camelCase wire-key for `personFullName`.
{-# ANN personFullName (forBackend backendJSON (rename "fullName")) #-}

-- And `personSecret` is hidden from JSON only.
{-# ANN personSecret   (forBackend backendJSON skip) #-}

-- ---------------------------------------------------------------------------
-- Splice four per-format instance groups in one go. Each consults
-- the `personFoo` annotations above, filtered to its own backend.
-- ---------------------------------------------------------------------------

DProto.deriveProto    ''Person
DCBOR.deriveCBOR      ''Person
DMP.deriveMsgPack     ''Person
DAeson.deriveJSON     ''Person

-- ---------------------------------------------------------------------------
-- Demo
-- ---------------------------------------------------------------------------

samplePerson :: Person
samplePerson = Person
  { personFullName = T.pack "Ada Lovelace"
  , personAge      = 36
  , personBalance  = 4_200_000
  , personSecret   = T.pack "let only the binary wires see this"
  }

main :: IO ()
main = do
  putStrLn "=== One ADT, four wire formats ==="
  putStrLn ""

  let protoBytes = PE.encodeMessage samplePerson
  putStrLn $ "proto:    " <> show (BS.length protoBytes) <> " bytes"
  case PD.decodeMessage protoBytes :: Either PD.DecodeError Person of
    Right p -> putStrLn $ "  decoded: " <> show p
    Left  e -> putStrLn $ "  decode error: " <> show e

  let cborBytes = CBOR.encodeCBOR samplePerson
  putStrLn $ "cbor:     " <> show (BS.length cborBytes) <> " bytes"

  let mpBytes = MP.encodeMsgPack samplePerson
  putStrLn $ "msgpack:  " <> show (BS.length mpBytes) <> " bytes"

  let jsonBytes = BL.toStrict (A.encode samplePerson)
  putStrLn $ "json:     " <> show (BS.length jsonBytes) <> " bytes"
  TIO.putStrLn $ "  payload: " <> TE.decodeUtf8 jsonBytes

  putStrLn ""
  putStrLn "Note: `personFullName` is `full_name` on every binary wire"
  putStrLn "but `fullName` in JSON, and `personSecret` is omitted from"
  putStrLn "JSON entirely — all driven by the same ANN annotations."
