{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Annotated test fixtures for 'Proto.Derive'.
--
-- Each field carries an explicit @tag N@ modifier (proto requires it).
-- A nested 'Address' submessage exercises the recursive
-- 'MessageEncode' \/ 'MessageDecode' path, and 'wireOverride' is used
-- on a couple of fields to force ZigZag and fixed-width encodings.
module Test.Proto.Derive.Types
  ( User (..)
  , Address (..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

import Wireform.Derive (tag, wireOverride, WireOverride (..))

data Address = Address
  { addrStreet :: !Text
  , addrCity   :: !Text
  , addrZip    :: !Word32
  } deriving stock (Show, Eq, Generic)

{-# ANN type Address ("Address" :: String) #-}
{-# ANN addrStreet (tag 1) #-}
{-# ANN addrCity   (tag 2) #-}
{-# ANN addrZip    (tag 3) #-}

data User = User
  { userId      :: !Int64
  , userName    :: !Text
  , userActive  :: !Bool
  , userScore   :: !Double
  , userTagBits :: !Word64
  , userBlob    :: !ByteString
  , userOffset  :: !Int32
    -- ^ Encoded as @sint32@ (ZigZag) via @wireOverride WireZigZag@.
  , userPort    :: !Word32
    -- ^ Encoded as @fixed32@ via @wireOverride WireFixed@.
  , userAddr    :: !(Maybe Address)
    -- ^ Optional submessage.
  } deriving stock (Show, Eq, Generic)

{-# ANN type User ("User" :: String) #-}
{-# ANN userId      (tag 1) #-}
{-# ANN userName    (tag 2) #-}
{-# ANN userActive  (tag 3) #-}
{-# ANN userScore   (tag 4) #-}
{-# ANN userTagBits (tag 5) #-}
{-# ANN userBlob    (tag 6) #-}
{-# ANN userOffset  (tag 7) #-}
{-# ANN userOffset  (wireOverride WireZigZag) #-}
{-# ANN userPort    (tag 8) #-}
{-# ANN userPort    (wireOverride WireFixed) #-}
{-# ANN userAddr    (tag 9) #-}
