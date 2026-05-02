{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Annotated fixture types for the CapnProto deriver round-trip
-- tests. Struct layout splits scalar fields into the data section
-- and pointer-shaped fields ('Text', 'ByteString', lists, …) into
-- the pointer section, both keyed by declaration order.
module Test.CapnProto.Derive.Types
  ( Position (..)
  , Blob (..)
  , Tag (..)
  , UserId (..)
  , User (..)
  , Color (..)
  , Profile (..)
  , defaultLabel
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)

import Wireform.Derive.Backend
import Wireform.Derive.Modifier

data Position = Position
  { posName  :: !Text
  , posX     :: !Int32
  , posY     :: !Int32
  , posNote  :: !(Maybe Text)
  , posLabel :: !Text
  } deriving (Eq, Show)

defaultLabel :: Text
defaultLabel = "<no-label>"

{-# ANN posLabel (forBackend backendCapnProto skip) #-}
{-# ANN posLabel (forBackend backendCapnProto (defaults 'defaultLabel)) #-}

data Blob = Blob
  { blobName  :: !Text
  , blobBytes :: !ByteString
  } deriving (Eq, Show)

newtype Tag = Tag { unTag :: Int32 }
  deriving (Eq, Show)

newtype UserId = UserId Int32
  deriving (Eq, Show)

data User = User
  { userId   :: !UserId
  , userName :: !Text
  } deriving (Eq, Show)

{-# ANN userId (forBackend backendCapnProto (coerced ''Int32)) #-}

data Color = Red | Green | DarkBlue
  deriving (Eq, Show)

{-# ANN DarkBlue (tag 42) #-}

data Profile = Profile
  { profAge    :: !Int32
  , profActive :: !Bool
  , profScore  :: !Double
  } deriving (Eq, Show)
