{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Annotated fixture types for the FlatBuffers deriver round-trip
-- tests. Positional vtable layout, so 'rename' is intentionally
-- unused; what we exercise here is 'skip' + 'defaults', 'tag'
-- overrides on enum ordinals, and optional-via-'Maybe'.
module Test.FlatBuffers.Derive.Types
  ( Position (..)
  , Tag (..)
  , Color (..)
  , defaultLabel
  ) where

import Data.Int (Int32)
import Data.Text (Text)

import Wireform.Derive.Backend
import Wireform.Derive.Modifier

data Position = Position
  { posName   :: !Text
  , posX      :: !Int32
  , posY      :: !Int32
  , posNote   :: !(Maybe Text)
  , posLabel  :: !Text
  } deriving (Eq, Show)

defaultLabel :: Text
defaultLabel = "<no-label>"

{-# ANN posLabel (forBackend backendFlatBuffers skip) #-}
{-# ANN posLabel (forBackend backendFlatBuffers (defaults 'defaultLabel)) #-}

newtype Tag = Tag { unTag :: Int32 }
  deriving (Eq, Show)

-- | Multiple-ordinal enum exercising both the positional default and
-- an explicit @tag N@ override on one constructor.
data Color = Red | Green | DarkBlue
  deriving (Eq, Show)

{-# ANN DarkBlue (tag 42) #-}
