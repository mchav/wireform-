{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.HTML.Derive.Types
  ( User (..)
  , Color (..)
  , Shape (..)
  ) where

import Data.Text (Text)

import Wireform.Derive.Modifier
import Wireform.Derive.NameStyle

import HTML.Derive (asAttr)

data User = User
  { userId    :: !Int
  , userName  :: !Text
  , userEmail :: !Text
  } deriving (Eq, Show)

{-# ANN userId    asAttr #-}
{-# ANN userId    (rename "id") #-}
{-# ANN userName  (renameStyle KebabCase) #-}
{-# ANN userEmail (renameStyle (StripPrefix "user" `andThen` KebabCase)) #-}

data Color = Red | Green | Blue
  deriving (Eq, Show)

{-# ANN Red   (renameStyle LowerCase) #-}
{-# ANN Green (renameStyle LowerCase) #-}
{-# ANN Blue  (renameStyle LowerCase) #-}

data Shape
  = Origin
  | Square !Int
  deriving (Eq, Show)

{-# ANN Origin (renameStyle LowerCase) #-}
{-# ANN Square (renameStyle LowerCase) #-}
