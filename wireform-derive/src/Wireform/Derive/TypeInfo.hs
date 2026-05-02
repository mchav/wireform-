{-# LANGUAGE TemplateHaskell #-}

-- | A simplified, format-agnostic view over a Haskell @data@ /
-- @newtype@ declaration suitable for code generation.
--
-- Adapted from riz0id's @serde-th@ but kept thinner: we cover the four
-- shapes ('TypeShapeNewtype', 'TypeShapeRecord', 'TypeShapeEnum',
-- 'TypeShapeSum') that wireform's per-format derivers actually act on,
-- and defer to @th-abstraction@ for the heavy lifting.
module Wireform.Derive.TypeInfo
  ( TypeInfo (..)
  , TypeShape (..)
  , ConInfo (..)
  , FieldInfo (..)
  , reifyTypeInfo

    -- * Convenience
  , typeInfoConstructors
  , isRecordShape
  , isEnumShape
  , isNewtypeShape
  ) where

import Language.Haskell.TH (Cxt, Name, Q, Type, nameBase, reportError)
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (..)
  , ConstructorVariant (..)
  , DatatypeInfo (..)
  , DatatypeVariant (..)
  , reifyDatatype
  )

-- | Reified summary of a data declaration.
data TypeInfo = TypeInfo
  { typeInfoName     :: !Name
    -- ^ Type-constructor name (e.g. @\'\'Person@).
  , typeInfoContext  :: !Cxt
    -- ^ Datatype context (rare; non-empty only for the legacy
    -- @data Eq a => T a@ syntax).
  , typeInfoVarTypes :: ![Type]
    -- ^ The instantiated type variables, one entry per free variable
    -- in declaration order. Suitable for splicing into @instance@
    -- heads.
  , typeInfoShape    :: !TypeShape
  } deriving (Show)

-- | High-level shape of a data declaration. The four shapes carry
-- enough information for the per-format derivers in this package; types
-- that do not fit cleanly (GADTs, existentials, type families) are
-- rejected at 'reifyTypeInfo' time.
data TypeShape
  = -- | A @newtype@. Always exactly one constructor with one field.
    TypeShapeNewtype !ConInfo
  | -- | A @data@ with exactly one record constructor.
    TypeShapeRecord !ConInfo
  | -- | A @data@ in which every constructor is nullary (a C-style
    -- enum). Encodes especially compactly across most formats.
    TypeShapeEnum ![ConInfo]
  | -- | A @data@ with a mix of constructors, some carrying fields,
    -- some not.
    TypeShapeSum ![ConInfo]
  deriving (Show)

-- | A single constructor as seen by the derivers.
data ConInfo = ConInfo
  { conInfoName    :: !Name
  , conInfoIsRecord :: !Bool
  , conInfoFields  :: ![FieldInfo]
  } deriving (Show)

-- | A single field.
data FieldInfo = FieldInfo
  { fieldInfoName :: !(Maybe Name)
    -- ^ The record selector, if any. Positional constructors leave
    -- this as 'Nothing'; per-format derivers index by position in
    -- 'conInfoFields' in that case.
  , fieldInfoType :: !Type
  } deriving (Show)

-- | Reify a 'TypeInfo' from a type-constructor 'Name'. Reports a
-- splice-time error and aborts if the input does not refer to a
-- @data@ or @newtype@ supported by this package.
reifyTypeInfo :: Name -> Q TypeInfo
reifyTypeInfo typeName = do
  dti <- reifyDatatype typeName
  shape <- shapeOf dti
  pure TypeInfo
    { typeInfoName     = datatypeName dti
    , typeInfoContext  = datatypeContext dti
    , typeInfoVarTypes = datatypeInstTypes dti
    , typeInfoShape    = shape
    }
  where
    shapeOf :: DatatypeInfo -> Q TypeShape
    shapeOf dti = case datatypeVariant dti of
      Newtype -> case datatypeCons dti of
        [c] -> pure (TypeShapeNewtype (toConInfo c))
        _   -> reifyError dti "newtype must have exactly one constructor"
      Datatype  -> classifyData dti
      DataInstance      -> reifyError dti "data instances are not supported"
      NewtypeInstance   -> reifyError dti "newtype instances are not supported"
      TypeData          -> reifyError dti "type data declarations are not supported"

    classifyData :: DatatypeInfo -> Q TypeShape
    classifyData dti = case datatypeCons dti of
      []  -> reifyError dti "type has no constructors"
      [c] | constructorVariant c == NormalConstructor && null (constructorFields c)
            -> pure (TypeShapeEnum [toConInfo c])
          | RecordConstructor _ <- constructorVariant c
            -> pure (TypeShapeRecord (toConInfo c))
          | otherwise
            -> pure (TypeShapeSum [toConInfo c])
      cs
        | all isNullary cs
            -> pure (TypeShapeEnum (map toConInfo cs))
        | otherwise
            -> pure (TypeShapeSum (map toConInfo cs))

    isNullary :: ConstructorInfo -> Bool
    isNullary c =
         constructorVariant c == NormalConstructor
      && null (constructorFields c)

    reifyError :: DatatypeInfo -> String -> Q a
    reifyError dti msg = do
      reportError $
        "Wireform.Derive.TypeInfo.reifyTypeInfo: "
          ++ nameBase (datatypeName dti)
          ++ ": " ++ msg
      fail msg

-- | Convert a th-abstraction 'ConstructorInfo' into our smaller
-- 'ConInfo' representation.
toConInfo :: ConstructorInfo -> ConInfo
toConInfo c = case constructorVariant c of
  RecordConstructor fieldNames ->
    ConInfo
      { conInfoName    = constructorName c
      , conInfoIsRecord = True
      , conInfoFields  =
          zipWith (FieldInfo . Just) fieldNames (constructorFields c)
      }
  _ ->
    ConInfo
      { conInfoName    = constructorName c
      , conInfoIsRecord = False
      , conInfoFields  =
          map (FieldInfo Nothing) (constructorFields c)
      }

-- | All constructors in a 'TypeInfo', flattened across every shape.
typeInfoConstructors :: TypeInfo -> [ConInfo]
typeInfoConstructors ti = case typeInfoShape ti of
  TypeShapeNewtype c -> [c]
  TypeShapeRecord  c -> [c]
  TypeShapeEnum    cs -> cs
  TypeShapeSum     cs -> cs

isRecordShape :: TypeShape -> Bool
isRecordShape = \case
  TypeShapeRecord _ -> True
  _                 -> False

isEnumShape :: TypeShape -> Bool
isEnumShape = \case
  TypeShapeEnum _ -> True
  _               -> False

isNewtypeShape :: TypeShape -> Bool
isNewtypeShape = \case
  TypeShapeNewtype _ -> True
  _                  -> False
