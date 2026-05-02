{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Test fixtures exercising the proto deriver's repeated, map,
-- oneof, and enum support. These records carry no @ANN@
-- pragmas; the modifier vocabulary is supplied inline at the
-- 'Proto.Derive.deriveProtoFromTranslated' call site in
-- "Test.Proto.Derive.RichInstances", mimicking what an IDL bridge
-- would do.
module Test.Proto.Derive.RichTypes
  ( -- * Enum field
    Color (..)
  , Painting (..)

    -- * Repeated fields
  , Item (..)
  , Inventory (..)
  , LooseInventory (..)

    -- * Map field
  , Tagged (..)

    -- * Oneof field
  , Avatar (..)
  , Profile (..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)

-- | Colour enum (proto3 enum).
--
-- @Red@ is 0 (the proto default); 'Green' \/ 'Blue' are 1 \/ 2.
data Color = ColRed | ColGreen | ColBlue
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

-- | A record with one enum-typed field.
data Painting = Painting
  { pTitle  :: !Text
  , pColor  :: !Color
  } deriving stock (Show, Eq, Generic)

-- | A scalar submessage used as the element type of a repeated field.
data Item = Item
  { iName  :: !Text
  , iCount :: !Int32
  } deriving stock (Show, Eq, Generic)

-- | A record with a 'V.Vector'-backed repeated field of submessages.
data Inventory = Inventory
  { invName  :: !Text
  , invItems :: !(V.Vector Item)
  } deriving stock (Show, Eq, Generic)

-- | A record with a list-backed repeated field of strings.
data LooseInventory = LooseInventory
  { liId   :: !Int32
  , liTags :: ![Text]
  } deriving stock (Show, Eq, Generic)

-- | A record with a proto3 @map<string, string>@ field.
data Tagged = Tagged
  { tagName :: !Text
  , tagAttrs :: !(Map Text Text)
  } deriving stock (Show, Eq, Generic)

-- | Proto @oneof@ payload; each constructor carries one value.
data Avatar
  = AvatarUrl  !Text
  | AvatarBlob !ByteString
  | AvatarSeed !Int32
  deriving stock (Show, Eq, Generic)

-- | A record carrying a oneof field as @Maybe Avatar@.
data Profile = Profile
  { profName   :: !Text
  , profAvatar :: !(Maybe Avatar)
  } deriving stock (Show, Eq, Generic)
