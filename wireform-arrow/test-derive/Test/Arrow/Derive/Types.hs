{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Annotated fixture types for the Arrow deriver round-trip tests.
module Test.Arrow.Derive.Types (
  Profile (..),
  Tag (..),
  WithTag (..),
  Event (..),
  Score (..),
  Result (..),
  Outcome (..),
  defaultPrivate,
) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Wireform.Derive.Backend
import Wireform.Derive.Modifier
import Wireform.Derive.NameStyle


{- | A record with a mix of rename, renameStyle, and skip+defaults
modifiers.
-}
data Profile = Profile
  { profileName :: !Text
  , profileAge :: !Int32
  , profileEmail :: !Text
  , profilePrivate :: !Text
  }
  deriving (Eq, Show)


defaultPrivate :: Text
defaultPrivate = "<redacted>"


{-# ANN profileName (rename "name") #-}


{-# ANN profileAge (renameStyle SnakeCase) #-}


{-# ANN profileEmail (renameStyle (StripPrefix "profile" `andThen` SnakeCase)) #-}


{-# ANN profilePrivate (forBackend backendArrow skip) #-}
{-# ANN profilePrivate (forBackend backendArrow (defaults 'defaultPrivate)) #-}


{- | A newtype wrapping a primitive Arrow type. The deriver
generates 'HasEncoder' / 'HasDecoder' pass-through instances
for it so it can be used as a column inside another record.
-}
newtype Tag = Tag {unTag :: Int64}
  deriving (Eq, Show)


{- | A record that embeds the 'Tag' newtype, exercising the
newtype's pass-through column instances at the row level.
-}
data WithTag = WithTag
  { wtId :: !Tag
  , wtName :: !Text
  }
  deriving (Eq, Show)


{- | Exercises the 'Maybe' lift through 'nullable' / 'nullableD'
in the column codec. The deriver inherits this behaviour from
the @{\-# OVERLAPPING #-\}@ instances in "Arrow.Record.Generic".
-}
data Event = Event
  { eventId :: !Int32
  , eventNote :: !(Maybe Text)
  }
  deriving (Eq, Show)


{- | A newtype the deriver does /not/ generate column instances
for. Instead, the field that carries it uses the 'coerced'
modifier to delegate to the underlying 'Int32' codec.
-}
newtype Score = Score {unScore :: Int32}
  deriving (Eq, Show)


{- | Exercises 'coerced'. The 'resultScore' field uses
'Data.Coerce.coerce' to bridge between 'Score' and 'Int32' on
both encoder and decoder sides.
-}
data Result = Result
  { resultName :: !Text
  , resultScore :: !Score
  }
  deriving (Eq, Show)


{-# ANN resultScore (coerced ''Int32) #-}


{- | A sum type used to verify the deriver rejects non-record
shapes at splice time.
-}
data Outcome = OutcomeWin Int32 | OutcomeLose
  deriving (Eq, Show)
