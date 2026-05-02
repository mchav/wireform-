{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Annotated types exercised by the Aeson deriver round-trip tests.
module Test.Derive.Aeson.Types
  ( -- * Record
    Address (..)
  , defaultAddrInternal
    -- * Newtype
  , UserId (..)
    -- * Enum
  , Color (..)
    -- * Sum
  , Shape (..)
  ) where

import Data.Text (Text)

import Wireform.Derive.Backend
import Wireform.Derive.Modifier
import Wireform.Derive.NameStyle

-- | Default value supplied by the deriver for the skipped field.
defaultAddrInternal :: Text
defaultAddrInternal = "<missing>"

-- ---------------------------------------------------------------------------
-- Record (with rename + renameStyle + per-backend overrides)
-- ---------------------------------------------------------------------------

data Address = Address
  { addrStreet :: !Text
  , addrCity   :: !Text
  , addrZip    :: !Text
  , addrInternal :: !Text
  } deriving (Eq, Show)

{-# ANN addrStreet (rename "street") #-}
{-# ANN addrCity   (renameStyle SnakeCase) #-}
{-# ANN addrZip    (renameStyle (StripPrefix "addr" `andThen` SnakeCase)) #-}
-- | Internal field skipped from JSON entirely (no defaults required —
-- the round-trip test seeds the value directly).
{-# ANN addrInternal (forBackend backendJSON skip) #-}
{-# ANN addrInternal (forBackend backendJSON (defaults 'defaultAddrInternal)) #-}

-- ---------------------------------------------------------------------------
-- Newtype
-- ---------------------------------------------------------------------------

newtype UserId = UserId { unUserId :: Int }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Enum (renamed constructors)
-- ---------------------------------------------------------------------------

data Color = Red | Green | DarkBlue
  deriving (Eq, Show)

{-# ANN Red       (rename "red") #-}
{-# ANN Green     (rename "green") #-}
{-# ANN DarkBlue  (renameStyle KebabCase) #-}

-- ---------------------------------------------------------------------------
-- Sum-of-products
-- ---------------------------------------------------------------------------

data Shape
  = Point
  | Circle !Double
  | Rect !Double !Double
  deriving (Eq, Show)

{-# ANN Point  (rename "point") #-}
{-# ANN Circle (rename "circle") #-}
{-# ANN Rect   (renameStyle SnakeCase) #-}
