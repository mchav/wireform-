{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | TH splice site that drives 'Proto.TH.Derive.deriveProtoFromTranslated'
against the bare records in 'Test.Proto.Derive.TranslatedTypes'.

Modifiers are supplied inline as a @['Modifier']@ list per field —
the same shape that an IDL bridge (e.g. a future 'Proto.TH.loadProto'
rewrite) would synthesise from a parsed @MessageDef@.
-}
module Test.Proto.Derive.TranslatedInstances () where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import Language.Haskell.TH (Type (ConT))
import Proto.TH.Derive (
  TranslatedMessage (..),
  deriveProtoFromTranslated,
  translatedField,
 )
import Test.Proto.Derive.TranslatedTypes (
  AddressT (..),
  UserT (..),
 )
import Wireform.Derive (WireOverride (..), tag, wireOverride)


deriveProtoFromTranslated
  TranslatedMessage
    { tmType = ConT ''AddressT
    , tmConstructor = 'AddressT
    , tmProtoName = T.pack "AddressT"
    , tmFields =
        [ translatedField 'taddrStreet (ConT ''T.Text) False [tag 1]
        , translatedField 'taddrCity (ConT ''T.Text) False [tag 2]
        , translatedField 'taddrZip (ConT ''Word32) False [tag 3]
        ]
    , tmUnknownFieldsSel = Nothing
    }


deriveProtoFromTranslated
  TranslatedMessage
    { tmType = ConT ''UserT
    , tmConstructor = 'UserT
    , tmProtoName = T.pack "UserT"
    , tmFields =
        [ translatedField 'tuserId (ConT ''Int64) False [tag 1]
        , translatedField 'tuserName (ConT ''T.Text) False [tag 2]
        , translatedField 'tuserActive (ConT ''Bool) False [tag 3]
        , translatedField 'tuserScore (ConT ''Double) False [tag 4]
        , translatedField 'tuserTagBits (ConT ''Word64) False [tag 5]
        , translatedField 'tuserBlob (ConT ''ByteString) False [tag 6]
        , translatedField
            'tuserOffset
            (ConT ''Int32)
            False
            [tag 7, wireOverride WireZigZag]
        , translatedField
            'tuserPort
            (ConT ''Word32)
            False
            [tag 8, wireOverride WireFixed]
        , translatedField 'tuserAddr (ConT ''AddressT) True [tag 9]
        ]
    , tmUnknownFieldsSel = Nothing
    }
