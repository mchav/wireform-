{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Annotated fixture types for the Bond deriver round-trip tests.
--
-- Bond is a positional, ID-keyed format (analogous to Thrift), so
-- 'rename' / 'renameStyle' modifiers are ignored on the wire. The
-- 'tag' modifier explicitly assigns a 'Word16' field id to a record
-- field or to a sum constructor.
module Test.Bond.Derive.Types
  ( -- * Record (mixed default + tag-overridden field IDs + skip)
    Profile (..)
  , defaultSecret
    -- * Newtype
  , Tag (..)
    -- * Enum (with one explicit tag)
  , Color (..)
    -- * Sum (single-field-struct union)
  , Shape (..)
  ) where

import Data.Int (Int32)
import Data.Text (Text)

import Wireform.Derive.Backend
import Wireform.Derive.Modifier

-- ---------------------------------------------------------------------------
-- Record
-- ---------------------------------------------------------------------------

-- | A record exercising the four record paths: default positional
-- field ids (1, 2, 4), an explicit 'tag' override (id 7 on
-- 'profileScore'), and a skipped field with a default
-- ('profileSecret').
data Profile = Profile
  { profileName   :: !Text   -- field id 1 by default
  , profileAge    :: !Int32  -- field id 2 by default
  , profileScore  :: !Double -- explicit tag 7
  , profileActive :: !Bool   -- field id 4 by default
  , profileSecret :: !Text   -- skipped under Bond, default value
  } deriving (Eq, Show)

defaultSecret :: Text
defaultSecret = "<redacted>"

{-# ANN profileScore  (forBackend backendBond (tag 7)) #-}
{-# ANN profileSecret (forBackend backendBond skip) #-}
{-# ANN profileSecret (forBackend backendBond (defaults 'defaultSecret)) #-}

-- ---------------------------------------------------------------------------
-- Newtype
-- ---------------------------------------------------------------------------

newtype Tag = Tag { unTag :: Int32 }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Enum
-- ---------------------------------------------------------------------------

data Color = Red | Green | DarkBlue
  deriving (Eq, Show)

{-# ANN DarkBlue (forBackend backendBond (tag 99)) #-}

-- ---------------------------------------------------------------------------
-- Sum (Bond union shape: single-field struct keyed by ctor id)
-- ---------------------------------------------------------------------------

data Shape
  = Origin                  -- ctor id 1 (positional)
  | Circle !Double          -- ctor id 2 (positional)
  | Rect   !Double !Double  -- explicit ctor id 10
  deriving (Eq, Show)

{-# ANN Rect (forBackend backendBond (tag 10)) #-}
