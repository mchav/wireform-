{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Annotated fixture types for the Bencode deriver round-trip tests.
module Test.Bencode.Derive.Types (
  Profile (..),
  Tag (..),
  Color (..),
  Shape (..),
  defaultPrivate,
) where

import Data.Text (Text)
import Wireform.Derive.Backend
import Wireform.Derive.Modifier
import Wireform.Derive.NameStyle


data Profile = Profile
  { profileName :: !Text
  , profileAge :: !Int
  , profileEmail :: !Text
  , profilePrivate :: !Text
  }
  deriving (Eq, Show)


defaultPrivate :: Text
defaultPrivate = "<redacted>"


{-# ANN profileName (rename "name") #-}


{-# ANN profileAge (renameStyle SnakeCase) #-}


{-# ANN profileEmail (renameStyle (StripPrefix "profile" `andThen` SnakeCase)) #-}


{-# ANN profilePrivate (forBackend backendBencode skip) #-}
{-# ANN profilePrivate (forBackend backendBencode (defaults 'defaultPrivate)) #-}


newtype Tag = Tag {unTag :: Int}
  deriving (Eq, Show)


data Color = Red | Green | DarkBlue
  deriving (Eq, Show)


{-# ANN Red (rename "red") #-}


{-# ANN Green (rename "green") #-}


{-# ANN DarkBlue (renameStyle KebabCase) #-}


{- | Bencode has no 'Double' scalar type, so 'Shape' carries 'Int' here
rather than 'Double' like the BSON/CBOR fixtures.
-}
data Shape
  = Origin
  | Circle !Int
  | Rect !Int !Int
  deriving (Eq, Show)


{-# ANN Origin (rename "origin") #-}


{-# ANN Circle (rename "circle") #-}


{-# ANN Rect (renameStyle SnakeCase) #-}
