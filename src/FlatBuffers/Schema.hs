module FlatBuffers.Schema
  ( FlatBuffersSchema (..)
  , FBDeclaration (..)
  , TableDef (..)
  , TableField (..)
  , FBStructDef (..)
  , FBEnumDef (..)
  , FBUnionDef (..)
  , FBType (..)
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)

data FlatBuffersSchema = FlatBuffersSchema
  { fbsNamespace      :: !(Maybe Text)
  , fbsIncludes       :: !(Vector Text)
  , fbsDecls          :: !(Vector FBDeclaration)
  , fbsRootType       :: !(Maybe Text)
  , fbsFileIdentifier :: !(Maybe Text)
  , fbsFileExtension  :: !(Maybe Text)
  } deriving stock (Show, Eq)

data FBDeclaration
  = FBTable !TableDef
  | FBStruct !FBStructDef
  | FBEnum !FBEnumDef
  | FBUnion !FBUnionDef
  deriving stock (Show, Eq)

data TableDef = TableDef
  { tdName   :: !Text
  , tdFields :: !(Vector TableField)
  } deriving stock (Show, Eq)

data TableField = TableField
  { tfName       :: !Text
  , tfType       :: !FBType
  , tfDefault    :: !(Maybe Text)
  , tfDeprecated :: !Bool
  } deriving stock (Show, Eq)

data FBStructDef = FBStructDef
  { fsdName   :: !Text
  , fsdFields :: !(Vector (Text, FBType))
  } deriving stock (Show, Eq)

data FBEnumDef = FBEnumDef
  { fedName           :: !Text
  , fedUnderlyingType :: !FBType
  , fedValues         :: !(Vector (Text, Maybe Int64))
  } deriving stock (Show, Eq)

data FBUnionDef = FBUnionDef
  { fudName    :: !Text
  , fudMembers :: !(Vector Text)
  } deriving stock (Show, Eq)

data FBType
  = FTBool | FTByte | FTUByte | FTShort | FTUShort | FTInt | FTUInt
  | FTLong | FTULong | FTFloat | FTDouble | FTString
  | FTVector !FBType | FTNamed !Text
  deriving stock (Show, Eq)
