{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Splice site for the byte-equivalence regression. Emits two
families of instances side by side:

1. @loadProto "test\/data\/derive_regression.proto"@ produces
   @RegItem@ \/ @RegInventory@ via "Proto.TH"; this is the
   reference implementation.
2. 'deriveProtoFromTranslated' produces matching instances for
   'BridgeRegItem' \/ 'BridgeRegInventory' from
   "Test.Proto.TH.Derive.RegressionTypes".

The actual byte-equality assertion lives in "Test.Proto.TH.Derive";
this module only wires up the splices.
-}
module Test.Proto.TH.Derive.RegressionInstances (
  -- * loadProto-generated types (re-exported for tests)
  RegItem (..),
  RegInventory (..),
  defaultRegItem,
  defaultRegInventory,
) where

import Data.Int (Int32)
import Data.Text qualified as T
import Data.Vector qualified as V -- needed by the loadProto splice
import Language.Haskell.TH (Type (ConT))
import Proto.Repr qualified as PR
import Proto.TH (loadProto)
import Proto.TH.Derive (
  TranslatedField (..),
  TranslatedMessage (..),
  deriveProtoFromTranslated,
  translatedField,
 )
import Test.Proto.TH.Derive.RegressionTypes (
  BridgeRegInventory (..),
  BridgeRegItem (..),
 )
import Wireform.Derive (tag)


-- Keep GHC from optimising away the imports the loadProto splice
-- transitively needs.
_unused :: (V.Vector Int, T.Text, Int32)
_unused = (V.empty, T.empty, 0)


{- | Reference instances for @RegItem@ \/ @RegInventory@. The path
is resolved from the package's source directory (cabal's cwd
during the TH splice).
-}
$(loadProto "test/data/derive_regression.proto")


-- | Bridge instances for the parallel record types.
deriveProtoFromTranslated
  TranslatedMessage
    { tmType = ConT ''BridgeRegItem
    , tmConstructor = 'BridgeRegItem
    , tmProtoName = T.pack "BridgeRegItem"
    , tmFields =
        [ translatedField 'brName (ConT ''T.Text) False [tag 1]
        , translatedField 'brCount (ConT ''Int32) False [tag 2]
        ]
    , tmUnknownFieldsSel = Nothing
    }


deriveProtoFromTranslated
  TranslatedMessage
    { tmType = ConT ''BridgeRegInventory
    , tmConstructor = 'BridgeRegInventory
    , tmProtoName = T.pack "BridgeRegInventory"
    , tmFields =
        [ translatedField 'briName (ConT ''T.Text) False [tag 1]
        , (translatedField 'briItems (ConT ''BridgeRegItem) False [tag 2])
            { tfRepeated = Just PR.vectorAdapter
            }
        ]
    , tmUnknownFieldsSel = Nothing
    }
