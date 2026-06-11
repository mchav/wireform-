{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.NDJSON.Derive.Types (
  Event (..),
) where

import Data.Text (Text)
import Wireform.Derive.Modifier
import Wireform.Derive.NameStyle


data Event = Event
  { eventId :: !Int
  , eventName :: !Text
  , eventScore :: !Double
  }
  deriving (Eq, Show)


{-# ANN eventId (renameStyle SnakeCase) #-}


{-# ANN eventName (rename "name") #-}


{-# ANN eventScore (renameStyle (StripPrefix "event" `andThen` SnakeCase)) #-}
