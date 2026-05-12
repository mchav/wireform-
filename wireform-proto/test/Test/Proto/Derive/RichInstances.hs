{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Bridge-driven instance splices for "Test.Proto.Derive.RichTypes",
exercising 'Proto.Derive.deriveProtoFromTranslated' with each of
the new field shapes (enum, repeated, map, oneof).
-}
module Test.Proto.Derive.RichInstances () where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map -- for instances
import Data.Text qualified as T
import Data.Vector qualified as V -- for instances
import Language.Haskell.TH (Type (AppT, ConT))
import Proto.Derive (
  TranslatedField (..),
  TranslatedMessage (..),
  TranslatedOneofVariant (..),
  deriveProtoFromTranslated,
  translatedField,
 )
import Proto.Repr qualified as PR
import Test.Proto.Derive.RichTypes (
  Avatar (..),
  Color,
  Inventory (..),
  Item (..),
  LooseInventory (..),
  Painting (..),
  Profile (..),
  Tagged (..),
 )
import Wireform.Derive (mapKey, tag)
import Wireform.Derive.Modifier (MapKeyScalar (..))


-- A reference to make GHC keep the instances/imports for Map and
-- Vector around even on minimal compilation modes.
_keepImports :: (Map.Map T.Text T.Text, V.Vector ())
_keepImports = (Map.empty, V.empty)


-- ---------------------------------------------------------------------------
-- Enum
-- ---------------------------------------------------------------------------

deriveProtoFromTranslated
  TranslatedMessage
    { tmType = ConT ''Painting
    , tmConstructor = 'Painting
    , tmProtoName = T.pack "Painting"
    , tmFields =
        [ translatedField 'pTitle (ConT ''T.Text) False [tag 1]
        , (translatedField 'pColor (ConT ''Color) False [tag 2])
            { tfIsEnum = True
            }
        ]
    , tmUnknownFieldsSel = Nothing
    }


-- ---------------------------------------------------------------------------
-- Repeated submessage (Vector-backed) and repeated scalar (list-backed)
-- ---------------------------------------------------------------------------

deriveProtoFromTranslated
  TranslatedMessage
    { tmType = ConT ''Item
    , tmConstructor = 'Item
    , tmProtoName = T.pack "Item"
    , tmFields =
        [ translatedField 'iName (ConT ''T.Text) False [tag 1]
        , translatedField 'iCount (ConT ''Int32) False [tag 2]
        ]
    , tmUnknownFieldsSel = Nothing
    }


deriveProtoFromTranslated
  TranslatedMessage
    { tmType = ConT ''Inventory
    , tmConstructor = 'Inventory
    , tmProtoName = T.pack "Inventory"
    , tmFields =
        [ translatedField 'invName (ConT ''T.Text) False [tag 1]
        , (translatedField 'invItems (ConT ''Item) False [tag 2])
            { tfRepeated = Just PR.vectorAdapter
            }
        ]
    , tmUnknownFieldsSel = Nothing
    }


deriveProtoFromTranslated
  TranslatedMessage
    { tmType = ConT ''LooseInventory
    , tmConstructor = 'LooseInventory
    , tmProtoName = T.pack "LooseInventory"
    , tmFields =
        [ translatedField 'liId (ConT ''Int32) False [tag 1]
        , (translatedField 'liTags (ConT ''T.Text) False [tag 2])
            { tfRepeated = Just PR.listAdapter
            }
        ]
    , tmUnknownFieldsSel = Nothing
    }


-- ---------------------------------------------------------------------------
-- Map<string, string>
-- ---------------------------------------------------------------------------

deriveProtoFromTranslated
  TranslatedMessage
    { tmType = ConT ''Tagged
    , tmConstructor = 'Tagged
    , tmProtoName = T.pack "Tagged"
    , tmFields =
        [ translatedField 'tagName (ConT ''T.Text) False [tag 1]
        , ( translatedField
              'tagAttrs
              (ConT ''T.Text)
              False
              [tag 2, mapKey MapKeyString]
          )
            { tfMapKey = Just MapKeyString
            }
        ]
    , tmUnknownFieldsSel = Nothing
    }


-- ---------------------------------------------------------------------------
-- Oneof
-- ---------------------------------------------------------------------------

deriveProtoFromTranslated
  TranslatedMessage
    { tmType = ConT ''Profile
    , tmConstructor = 'Profile
    , tmProtoName = T.pack "Profile"
    , tmFields =
        [ translatedField 'profName (ConT ''T.Text) False [tag 1]
        , (translatedField 'profAvatar (AppT (ConT ''Maybe) (ConT ''Avatar)) False [])
            { tfOneofVariants =
                [ TranslatedOneofVariant 'AvatarUrl (ConT ''T.Text) [tag 6]
                , TranslatedOneofVariant 'AvatarBlob (ConT ''ByteString) [tag 7]
                , TranslatedOneofVariant 'AvatarSeed (ConT ''Int32) [tag 8]
                ]
            }
        ]
    , tmUnknownFieldsSel = Nothing
    }
