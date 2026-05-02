{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.CSV.Derive.Types
  ( Person (..)
  , defaultNotes
  ) where

import Data.Text (Text)

import Wireform.Derive.Backend
import Wireform.Derive.Modifier
import Wireform.Derive.NameStyle

data Person = Person
  { personName  :: !Text
  , personAge   :: !Int
  , personEmail :: !Text
  , personNotes :: !Text
  } deriving (Eq, Show)

defaultNotes :: Text
defaultNotes = ""

{-# ANN personName  (rename "name") #-}
{-# ANN personAge   (renameStyle SnakeCase) #-}
{-# ANN personEmail (renameStyle (StripPrefix "person" `andThen` SnakeCase)) #-}
{-# ANN personNotes (forBackend backendCSV skip) #-}
{-# ANN personNotes (forBackend backendCSV (defaults 'defaultNotes)) #-}
