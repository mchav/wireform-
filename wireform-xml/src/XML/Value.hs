{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

{- | XML DOM types, aeson-style.

'Node' is the core type representing an XML node in a document tree.
Uses 'Vector' for child nodes and attributes for O(1) indexing.
-}
module XML.Value (
  Node (..),
  Name (..),
  Attribute (..),
  Document (..),
  XMLDecl (..),
  simpleName,
  qualifiedName,
  elementName,
  elementChildren,
  elementAttributes,
) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)


data Node
  = Element !Name !(Vector Attribute) !(Vector Node)
  | Text !Text
  | CData !Text
  | Comment !Text
  | ProcessingInstruction !Text !Text
  deriving stock (Show, Eq, Generic)


instance NFData Node where
  rnf (Element n as cs) = rnf n `seq` rnf as `seq` rnf cs
  rnf (Text t) = rnf t
  rnf (CData t) = rnf t
  rnf (Comment t) = rnf t
  rnf (ProcessingInstruction t1 t2) = rnf t1 `seq` rnf t2


data Name = Name
  { nameLocal :: !Text
  , namePrefix :: !(Maybe Text)
  , nameNamespace :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Ord, Generic)


instance NFData Name where
  rnf (Name l p n) = rnf l `seq` rnf p `seq` rnf n


data Attribute = Attribute !Name !Text
  deriving stock (Show, Eq, Generic)


instance NFData Attribute where
  rnf (Attribute n v) = rnf n `seq` rnf v


data Document = Document
  { docProlog :: !(Maybe XMLDecl)
  , docRoot :: !Node
  }
  deriving stock (Show, Eq, Generic)


instance NFData Document where
  rnf (Document p r) = rnf p `seq` rnf r


data XMLDecl = XMLDecl
  { xmlVersion :: !Text
  , xmlEncoding :: !(Maybe Text)
  , xmlStandalone :: !(Maybe Bool)
  }
  deriving stock (Show, Eq, Generic)


instance NFData XMLDecl where
  rnf (XMLDecl v e s) = rnf v `seq` rnf e `seq` rnf s


simpleName :: Text -> Name
simpleName t = Name t Nothing Nothing


qualifiedName :: Text -> Text -> Name
qualifiedName pfx local = Name local (Just pfx) Nothing


elementName :: Node -> Maybe Name
elementName (Element n _ _) = Just n
elementName _ = Nothing


elementChildren :: Node -> Vector Node
elementChildren (Element _ _ cs) = cs
elementChildren _ = mempty


elementAttributes :: Node -> Vector Attribute
elementAttributes (Element _ as _) = as
elementAttributes _ = mempty
