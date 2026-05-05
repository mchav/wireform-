{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Annotated fixture types for the YAML deriver round-trip tests.
module Test.YAML.Derive.Types
  ( Profile (..)
  , Tag (..)
  , Color (..)
  , Shape (..)
  , defaultPrivate
  ) where

import Data.Text (Text)

import Wireform.Derive.Backend
import Wireform.Derive.Modifier
import Wireform.Derive.NameStyle

data Profile = Profile
  { profileName    :: !Text
  , profileAge     :: !Int
  , profileEmail   :: !Text
  , profilePrivate :: !Text
  } deriving (Eq, Show)

defaultPrivate :: Text
defaultPrivate = "<redacted>"

{-# ANN profileName    (rename "name") #-}
{-# ANN profileAge     (renameStyle SnakeCase) #-}
{-# ANN profileEmail   (renameStyle (StripPrefix "profile" `andThen` SnakeCase)) #-}
{-# ANN profilePrivate (forBackend backendYAML skip) #-}
{-# ANN profilePrivate (forBackend backendYAML (defaults 'defaultPrivate)) #-}

newtype Tag = Tag { unTag :: Int }
  deriving (Eq, Show)

data Color = Red | Green | DarkBlue
  deriving (Eq, Show)

{-# ANN Red      (rename "red") #-}
{-# ANN Green    (rename "green") #-}
{-# ANN DarkBlue (renameStyle KebabCase) #-}

data Shape
  = Origin
  | Circle !Double
  | Rect !Double !Double
  deriving (Eq, Show)

{-# ANN Origin (rename "origin") #-}
{-# ANN Circle (rename "circle") #-}
{-# ANN Rect   (renameStyle SnakeCase) #-}
