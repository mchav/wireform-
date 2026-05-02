{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Test fixtures for 'Proto.Derive.deriveProtoFromTranslated'.
--
-- Unlike 'Test.Proto.Derive.Types', these records carry no @ANN@
-- annotations: the modifier vocabulary is supplied inline at the
-- 'deriveProtoFromTranslated' call site instead. This exercises the
-- IDL-bridge entry point intended for use from 'Proto.TH.loadProto'.
module Test.Proto.Derive.TranslatedTypes
  ( AddressT (..)
  , UserT (..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

data AddressT = AddressT
  { taddrStreet :: !Text
  , taddrCity   :: !Text
  , taddrZip    :: !Word32
  } deriving stock (Show, Eq, Generic)

data UserT = UserT
  { tuserId      :: !Int64
  , tuserName    :: !Text
  , tuserActive  :: !Bool
  , tuserScore   :: !Double
  , tuserTagBits :: !Word64
  , tuserBlob    :: !ByteString
  , tuserOffset  :: !Int32
  , tuserPort    :: !Word32
  , tuserAddr    :: !(Maybe AddressT)
  } deriving stock (Show, Eq, Generic)
