{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Annotated fixture types for the XML deriver round-trip tests.
module Test.XML.Derive.Types (
  User (..),
  Status (..),
  Color (..),
  Shape (..),
) where

import Data.Text (Text)
import Wireform.Derive.Modifier
import Wireform.Derive.NameStyle
import XML.Derive (asAttribute)


-- ---------------------------------------------------------------------------
-- Record with attribute / element split via 'asAttribute'
-- ---------------------------------------------------------------------------

data User = User
  { userId :: !Int
  , userName :: !Text
  , userEmail :: !Text
  }
  deriving (Eq, Show)


{-# ANN userId asAttribute #-}
{-# ANN userId (rename "id") #-}


{-# ANN userName (renameStyle KebabCase) #-}


{-# ANN userEmail (renameStyle (StripPrefix "user" `andThen` KebabCase)) #-}


-- ---------------------------------------------------------------------------
-- Record without attributes (all elements)
-- ---------------------------------------------------------------------------

data Status = Status
  { statusCode :: !Int
  , statusMessage :: !Text
  }
  deriving (Eq, Show)


{-# ANN statusCode (renameStyle KebabCase) #-}


{-# ANN statusMessage (renameStyle KebabCase) #-}


-- ---------------------------------------------------------------------------
-- Enum + Sum
-- ---------------------------------------------------------------------------

data Color = Red | Green | Blue
  deriving (Eq, Show)


{-# ANN Red (renameStyle LowerCase) #-}


{-# ANN Green (renameStyle LowerCase) #-}


{-# ANN Blue (renameStyle LowerCase) #-}


data Shape
  = Origin
  | Square !Int
  deriving (Eq, Show)


{-# ANN Origin (renameStyle LowerCase) #-}


{-# ANN Square (renameStyle LowerCase) #-}
