{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Annotation-driven Template Haskell deriver for XML
-- 'XML.Class.ToXML' / 'XML.Class.FromXML' instances.
--
-- == Encoding shape
--
-- * 'TypeShapeNewtype' — pass-through to the inner field's instance.
-- * 'TypeShapeRecord'  — wraps the record in an outer
--   @\<TypeName\>\</TypeName\>@ element. Each field is, by default,
--   a nested child element. Fields tagged with the 'AsAttribute'
--   'XmlFieldOpt' extension are emitted as XML attributes on the
--   wrapper instead.
-- * 'TypeShapeEnum'    — element whose tag is the constructor name.
-- * 'TypeShapeSum'     — element whose tag is the constructor name,
--   with the contents emitted as nested children.
--
-- == Per-backend customisation via extension modifier
--
-- This deriver introduces 'XmlFieldOpt', a backend-specific
-- 'Wireform.Derive.Extension.BackendModifier'. The annotation
-- @{-# ANN field (extension AsAttribute) #-}@ flips a record field
-- from element-based to attribute-based emission. This pattern can
-- be copied wholesale by other format-specific extension types.
module XML.Derive
  ( -- * Deriver
    deriveXML
  , deriveToXML
  , deriveFromXML

    -- * Backend extension vocabulary
  , XmlFieldOpt (..)
  , asAttribute
  , asElement
  ) where

import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable (foldlM)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Language.Haskell.TH

import qualified XML.Class as X
import qualified XML.Value as XV

import Wireform.Derive.Backend
import Wireform.Derive.Extension (BackendModifier (..), extension, lookupExtension)
import Wireform.Derive.Modifier (Modifier)
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

-- ---------------------------------------------------------------------------
-- Backend extension: attribute / element distinction
-- ---------------------------------------------------------------------------

-- | XML-specific per-field flags. Use via
-- 'Wireform.Derive.Extension.extension':
--
-- @
-- {-\# ANN userId (extension AsAttribute) \#-}
-- @
data XmlFieldOpt
  = -- | Emit this field as an XML attribute on the wrapper element
    -- instead of a nested child element.
    AsAttribute
  | -- | Emit this field as a nested element (the default; provided
    -- for explicit overrides of an inherited 'AsAttribute').
    AsElement
  deriving stock (Eq, Show, Read, Typeable, Data, Generic)

instance BackendModifier XmlFieldOpt where
  backendModifierTag _ = "wireform-xml.field-opt"

asAttribute :: Modifier
asAttribute = extension AsAttribute

asElement :: Modifier
asElement = extension AsElement

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

deriveXML :: Name -> Q [Dec]
deriveXML nm = (++) <$> deriveToXML nm <*> deriveFromXML nm

deriveToXML :: Name -> Q [Dec]
deriveToXML nm = do
  ti   <- reifyTypeInfo nm
  body <- toXMLBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''X.ToXML) typ)
              [FunD 'X.toXML [Clause [] (NormalB body) []]]
  pure [decl]

deriveFromXML :: Name -> Q [Dec]
deriveFromXML nm = do
  ti   <- reifyTypeInfo nm
  body <- fromXMLBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''X.FromXML) typ)
              [FunD 'X.fromXML [Clause [] (NormalB body) []]]
  pure [decl]

-- ---------------------------------------------------------------------------
-- ToXML
-- ---------------------------------------------------------------------------

toXMLBody :: TypeInfo -> Q Exp
toXMLBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> toXMLNewtype c
  TypeShapeRecord  c   -> toXMLRecord (typeInfoName ti) c
  TypeShapeEnum    cs  -> toXMLEnum cs
  TypeShapeSum     cs  -> toXMLSum cs

toXMLNewtype :: ConInfo -> Q Exp
toXMLNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [| X.toXML ($(varE sel) $(varE x)) |]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [| X.toXML $(varE x) |]
  _ -> fail "XML.Derive: newtype must have exactly one field"

-- | Records become @\<TypeName attrs…\>children…\</TypeName\>@.
toXMLRecord :: Name -> ConInfo -> Q Exp
toXMLRecord typeNm c = do
  x <- newName "x"
  pieces <- mapM (toXMLField (varE x)) (conInfoFields c)
  let attrs    = ListE [a | (Attr,    a) <- concat pieces]
      children = ListE [e | (Child,   e) <- concat pieces]
      elemNm   = T.pack (nameBase typeNm)
  lamE [varP x]
    [| XV.Element
         (XV.simpleName $(litE (stringL (T.unpack elemNm))))
         (V.fromList $(pure attrs))
         (V.fromList $(pure children)) |]

data FieldDest = Attr | Child

toXMLField :: Q Exp -> FieldInfo -> Q [(FieldDest, Exp)]
toXMLField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendXML selName
  if miSkip mi
    then pure []
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [| X.toXML $getter |]
            Just _  -> [| X.toXML (coerce $getter) |]
      case lookupExtension @XmlFieldOpt mi of
        Just AsAttribute -> do
          -- Attribute: stringify the inner value via FromXML's
          -- companion route — for now, only allow Text-shaped
          -- payloads (call X.toXML and then expect Text).
          a <- [| XV.Attribute (XV.simpleName $(pure keyExp))
                               (extractAttrText $encoded) |]
          pure [(Attr, a)]
        _ -> do
          -- Default: child element wrapping the inner toXML node.
          -- If the inner is already an Element, we wrap in a named
          -- element to preserve the field tag.
          e <- [| XV.Element (XV.simpleName $(pure keyExp))
                             V.empty
                             (V.singleton $encoded) |]
          pure [(Child, e)]

-- | Extract a 'Text' rendition from a 'Node' for use as an attribute
-- value. Mirrors the conversions in 'XML.Class.FromXML' for 'Text'.
extractAttrText :: XV.Node -> T.Text
extractAttrText (XV.Text t)   = t
extractAttrText (XV.CData t)  = t
extractAttrText (XV.Element _ _ cs)
  | V.null cs = T.empty
  | otherwise = T.concat (V.toList (V.map extractAttrText cs))
extractAttrText _ = T.empty

toXMLEnum :: [ConInfo] -> Q Exp
toXMLEnum cs = do
  v <- newName "v"
  matches <- mapM enumToXMLMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)

enumToXMLMatch :: ConInfo -> Q Match
enumToXMLMatch c = do
  mi <- reifyModifierInfoFor backendXML (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  body <- [| XV.Element (XV.simpleName $(pure keyExp))
                        V.empty V.empty |]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])

toXMLSum :: [ConInfo] -> Q Exp
toXMLSum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToXML cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)

sumCtorToXML :: ConInfo -> Q Match
sumCtorToXML c = do
  mi <- reifyModifierInfoFor backendXML (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
      childExps = ListE (map (AppE (VarE 'X.toXML) . VarE) fieldNames)
  body <- [| XV.Element (XV.simpleName $(pure keyExp))
                        V.empty
                        (V.fromList $(pure childExps)) |]
  pure (Match pat (NormalB body) [])

-- ---------------------------------------------------------------------------
-- FromXML
-- ---------------------------------------------------------------------------

fromXMLBody :: TypeInfo -> Q Exp
fromXMLBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> fromXMLNewtype c
  TypeShapeRecord  c   -> fromXMLRecord (typeInfoName ti) c
  TypeShapeEnum    cs  -> fromXMLEnum cs
  TypeShapeSum     cs  -> fromXMLSum cs

fromXMLNewtype :: ConInfo -> Q Exp
fromXMLNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [| fmap $(conE (conInfoName c)) . X.fromXML |]
  _               -> fail "XML.Derive: newtype must have exactly one field"

fromXMLRecord :: Name -> ConInfo -> Q Exp
fromXMLRecord typeNm c = do
  v        <- newName "v"
  attrsVar <- newName "attrs"
  childrenVar <- newName "children"
  bodyE    <- recordParser attrsVar childrenVar c
  let _typeName = nameBase typeNm  -- we don't enforce wrapper name match yet
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'XV.Element [wildP, varP attrsVar, varP childrenVar])
               (normalB (pure bodyE))
               []
       , match wildP
               (normalB
                  [| Left "XML.Derive: expected Element for record type" |])
               []
       ])

recordParser :: Name -> Name -> ConInfo -> Q Exp
recordParser attrsVar childrenVar c = do
  let conName = conInfoName c
      fields  = conInfoFields c
  case fields of
    []        -> [| Right $(conE conName) |]
    (f0 : fs) -> do
      e0 <- fieldParser attrsVar childrenVar f0
      hd <- [| $(conE conName) <$> $(pure e0) |]
      foldlM
        (\acc f -> do
            ef <- fieldParser attrsVar childrenVar f
            [| $(pure acc) <*> $(pure ef) |])
        hd
        fs

fieldParser :: Name -> Name -> FieldInfo -> Q Exp
fieldParser attrsVar childrenVar (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendXML selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [| Right $(varE defNm) |]
      Nothing    -> [| Left ("XML.Derive: missing 'defaults' for skipped field "
                              ++ $(litE (stringL (nameBase selName)))) |]
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      base <- case lookupExtension @XmlFieldOpt mi of
        Just AsAttribute ->
          [| case lookupAttribute $(pure keyExp) $(varE attrsVar) of
               Nothing -> Left ("XML.Derive: missing attribute "
                                ++ T.unpack $(pure keyExp))
               Just t  -> X.fromXML (XV.Text t) |]
        _ ->
          [| case lookupChildElement $(pure keyExp) $(varE childrenVar) of
               Nothing  -> Left ("XML.Derive: missing child element "
                                 ++ T.unpack $(pure keyExp))
               Just inner -> X.fromXML inner |]
      case miCoerce mi of
        Nothing -> pure base
        Just _  -> [| fmap coerce $(pure base) |]

-- | Look up an attribute by local name.
lookupAttribute :: T.Text -> V.Vector XV.Attribute -> Maybe T.Text
lookupAttribute name attrs = V.foldr step Nothing attrs
  where
    step (XV.Attribute (XV.Name local _ _) value) acc
      | local == name = Just value
      | otherwise     = acc

-- | Look up the first child Element with the given local name and
-- return its sole child if any (matches our 'toXMLField' wrapper).
lookupChildElement :: T.Text -> V.Vector XV.Node -> Maybe XV.Node
lookupChildElement name nodes = V.foldr step Nothing nodes
  where
    step (XV.Element (XV.Name local _ _) _ cs) acc
      | local == name =
          case V.length cs of
            0 -> Just (XV.Text T.empty)
            1 -> Just (V.head cs)
            _ -> Just (XV.Element (XV.simpleName name) V.empty cs)
    step _ acc = acc

fromXMLEnum :: [ConInfo] -> Q Exp
fromXMLEnum cs = do
  v <- newName "v"
  s <- newName "s"
  branches <- mapM (enumDispatch s) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE (AppE (VarE 'mappend)
                  (LitE (StringL "XML.Derive: unknown enum tag ")))
               (AppE (VarE 'T.unpack) (VarE s))
        )
      multi = MultiIfE
        (map (\(g, e) -> (g, AppE (ConE 'Right) e)) branches
         ++ [(NormalG (ConE 'True), AppE (ConE 'Left) (snd fallback))])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'XV.Element
                  [conP 'XV.Name [varP s, wildP, wildP], wildP, wildP])
               (normalB (pure multi))
               []
       , match wildP
               (normalB [| Left "XML.Derive: enum expected Element" |])
               []
       ])

enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendXML (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp  = ConE (conInfoName c)
  pure (NormalG guardExp, bodyExp)

fromXMLSum :: [ConInfo] -> Q Exp
fromXMLSum cs = do
  v        <- newName "v"
  tagVar   <- newName "tag"
  childrenVar <- newName "kids"
  branches <- mapM (sumDispatch tagVar childrenVar) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE (ConE 'Left)
            (AppE (AppE (VarE 'mappend)
                      (LitE (StringL "XML.Derive: unknown sum tag ")))
                  (AppE (VarE 'T.unpack) (VarE tagVar)))
        )
      multi = MultiIfE (branches ++ [fallback])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'XV.Element
                  [conP 'XV.Name [varP tagVar, wildP, wildP],
                   wildP, varP childrenVar])
               (normalB (pure multi))
               []
       , match wildP
               (normalB [| Left "XML.Derive: sum expected Element" |])
               []
       ])

sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar childrenVar c = do
  mi <- reifyModifierInfoFor backendXML (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE tagVar)) (VarE '(==)) (Just keyExp)
  body <- case conInfoFields c of
    []  -> [| Right $(conE (conInfoName c)) |]
    fs  -> sumNAry childrenVar (conInfoName c) (length fs)
  pure (NormalG guardExp, body)

sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry childrenVar conName arity = do
  let parseI :: Int -> Q Exp
      parseI i =
        [| X.fromXML ($(varE childrenVar) V.! $(litE (integerL (fromIntegral i)))) |]
  hd <- do
    e0 <- parseI 0
    [| $(conE conName) <$> $(pure e0) |]
  body <- foldlM
    (\acc i -> do
        ei <- parseI i
        [| $(pure acc) <*> $(pure ei) |])
    hd
    [1 .. arity - 1]
  let conNameStr = nameBase conName
  [| if V.length $(varE childrenVar) == $(litE (integerL (fromIntegral arity)))
       then $(pure body)
       else Left ("XML.Derive: " ++ conNameStr
                  ++ " expected " ++ show $(litE (integerL (fromIntegral arity)))
                  ++ " children, got "
                  ++ show (V.length $(varE childrenVar))) |]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing  =
  fail "XML.Derive: cannot derive XML for non-record positional field"

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
