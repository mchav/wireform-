{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Annotation-driven Template Haskell deriver for HTML
-- 'HTML.Class.ToHTML' / 'HTML.Class.FromHTML' instances.
--
-- The encoding shape mirrors 'XML.Derive':
--
-- * 'TypeShapeNewtype' — pass-through.
-- * 'TypeShapeRecord'  — wraps the record in
--   @\<typeName\> attrs… children…\</typeName\>@. Field-level
--   @{-\# ANN field 'asAttr' \#-}@ flips a field from a child element
--   to an attribute on the wrapper.
-- * 'TypeShapeEnum'    — element whose tag is the constructor name.
-- * 'TypeShapeSum'     — element whose tag is the constructor name,
--   with children carrying the contents.
--
-- Tag and attribute names are emitted lowercase to match HTML5
-- convention; the deriver uses 'idiomaticFor' 'backendHTML'
-- (kebab-case) by default.
module HTML.Derive
  ( deriveHTML
  , deriveToHTML
  , deriveFromHTML
    -- * Backend extension vocabulary
  , HtmlFieldOpt (..)
  , asAttr
  , asChild
  ) where

import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable (foldlM)
import qualified Data.Primitive.SmallArray as SA
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Haskell.TH

import qualified HTML.Class as H
import qualified HTML.Value as HV

import Wireform.Derive.Backend
import Wireform.Derive.Extension (BackendModifier (..), extension, lookupExtension)
import Wireform.Derive.Modifier (Modifier)
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

-- ---------------------------------------------------------------------------
-- Backend extension
-- ---------------------------------------------------------------------------

-- | HTML-specific per-field flags. Use via
-- 'Wireform.Derive.Extension.extension':
--
-- @
-- {-\# ANN userId asAttr \#-}
-- @
data HtmlFieldOpt
  = -- | Emit this field as an HTML attribute on the wrapper element
    -- instead of a nested child.
    AsAttr
  | -- | Emit this field as a child element (the default; provided
    -- for explicit overrides).
    AsChild
  deriving stock (Eq, Show, Read, Typeable, Data, Generic)

instance BackendModifier HtmlFieldOpt where
  backendModifierTag _ = "wireform-html.field-opt"

asAttr :: Modifier
asAttr = extension AsAttr

asChild :: Modifier
asChild = extension AsChild

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

deriveHTML :: Name -> Q [Dec]
deriveHTML nm = (++) <$> deriveToHTML nm <*> deriveFromHTML nm

deriveToHTML :: Name -> Q [Dec]
deriveToHTML nm = do
  ti   <- reifyTypeInfo nm
  body <- toHTMLBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''H.ToHTML) typ)
              [FunD 'H.toHTML [Clause [] (NormalB body) []]]
  pure [decl]

deriveFromHTML :: Name -> Q [Dec]
deriveFromHTML nm = do
  ti   <- reifyTypeInfo nm
  body <- fromHTMLBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''H.FromHTML) typ)
              [FunD 'H.fromHTML [Clause [] (NormalB body) []]]
  pure [decl]

-- ---------------------------------------------------------------------------
-- ToHTML
-- ---------------------------------------------------------------------------

toHTMLBody :: TypeInfo -> Q Exp
toHTMLBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> toHTMLNewtype c
  TypeShapeRecord  c   -> toHTMLRecord (typeInfoName ti) c
  TypeShapeEnum    cs  -> toHTMLEnum cs
  TypeShapeSum     cs  -> toHTMLSum cs

toHTMLNewtype :: ConInfo -> Q Exp
toHTMLNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [| H.toHTML ($(varE sel) $(varE x)) |]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [| H.toHTML $(varE x) |]
  _ -> fail "HTML.Derive: newtype must have exactly one field"

toHTMLRecord :: Name -> ConInfo -> Q Exp
toHTMLRecord typeNm c = do
  x <- newName "x"
  pieces <- mapM (toHTMLField (varE x)) (conInfoFields c)
  let attrs    = ListE [a | (Attr, a)  <- concat pieces]
      children = ListE [e | (Child, e) <- concat pieces]
      elemNm   = T.toLower (T.pack (nameBase typeNm))
  lamE [varP x]
    [| HV.HTMLElement
         $(litE (stringL (T.unpack elemNm)))
         (SA.smallArrayFromList $(pure attrs))
         (SA.smallArrayFromList $(pure children)) |]

data FieldDest = Attr | Child

toHTMLField :: Q Exp -> FieldInfo -> Q [(FieldDest, Exp)]
toHTMLField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendHTML selName
  if miSkip mi
    then pure []
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [| H.toHTML $getter |]
            Just _  -> [| H.toHTML (coerce $getter) |]
      case lookupExtension @HtmlFieldOpt mi of
        Just AsAttr -> do
          a <- [| HV.HTMLAttribute $(pure keyExp)
                                   (extractAttrText $encoded) |]
          pure [(Attr, a)]
        _ -> do
          e <- [| HV.HTMLElement $(pure keyExp)
                                 (SA.smallArrayFromList [])
                                 (SA.smallArrayFromList [$encoded]) |]
          pure [(Child, e)]

extractAttrText :: HV.HTMLNode -> T.Text
extractAttrText (HV.HTMLText t) = t
extractAttrText n               = HV.textContent n

toHTMLEnum :: [ConInfo] -> Q Exp
toHTMLEnum cs = do
  v <- newName "v"
  matches <- mapM enumToHTMLMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)

enumToHTMLMatch :: ConInfo -> Q Match
enumToHTMLMatch c = do
  mi <- reifyModifierInfoFor backendHTML (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  body <- [| HV.HTMLElement $(pure keyExp)
                            (SA.smallArrayFromList [])
                            (SA.smallArrayFromList []) |]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])

toHTMLSum :: [ConInfo] -> Q Exp
toHTMLSum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToHTML cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)

sumCtorToHTML :: ConInfo -> Q Match
sumCtorToHTML c = do
  mi <- reifyModifierInfoFor backendHTML (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
      childExps = ListE (map (AppE (VarE 'H.toHTML) . VarE) fieldNames)
  body <- [| HV.HTMLElement $(pure keyExp)
                            (SA.smallArrayFromList [])
                            (SA.smallArrayFromList $(pure childExps)) |]
  pure (Match pat (NormalB body) [])

-- ---------------------------------------------------------------------------
-- FromHTML
-- ---------------------------------------------------------------------------

fromHTMLBody :: TypeInfo -> Q Exp
fromHTMLBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> fromHTMLNewtype c
  TypeShapeRecord  c   -> fromHTMLRecord c
  TypeShapeEnum    cs  -> fromHTMLEnum cs
  TypeShapeSum     cs  -> fromHTMLSum cs

fromHTMLNewtype :: ConInfo -> Q Exp
fromHTMLNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [| fmap $(conE (conInfoName c)) . H.fromHTML |]
  _               -> fail "HTML.Derive: newtype must have exactly one field"

fromHTMLRecord :: ConInfo -> Q Exp
fromHTMLRecord c = do
  v        <- newName "v"
  attrsVar <- newName "attrs"
  childrenVar <- newName "kids"
  bodyE    <- recordParser attrsVar childrenVar c
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'HV.HTMLElement [wildP, varP attrsVar, varP childrenVar])
               (normalB (pure bodyE))
               []
       , match wildP
               (normalB
                  [| Left "HTML.Derive: expected HTMLElement for record type" |])
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
  mi <- reifyModifierInfoFor backendHTML selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [| Right $(varE defNm) |]
      Nothing    -> [| Left ("HTML.Derive: missing 'defaults' for skipped field "
                              ++ $(litE (stringL (nameBase selName)))) |]
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      base <- case lookupExtension @HtmlFieldOpt mi of
        Just AsAttr ->
          [| case lookupHTMLAttr $(pure keyExp) $(varE attrsVar) of
               Nothing -> Left ("HTML.Derive: missing attribute "
                                ++ T.unpack $(pure keyExp))
               Just t  -> H.fromHTML (HV.HTMLText t) |]
        _ ->
          [| case lookupHTMLChild $(pure keyExp) $(varE childrenVar) of
               Nothing  -> Left ("HTML.Derive: missing child element "
                                 ++ T.unpack $(pure keyExp))
               Just inner -> H.fromHTML inner |]
      case miCoerce mi of
        Nothing -> pure base
        Just _  -> [| fmap coerce $(pure base) |]

lookupHTMLAttr :: T.Text -> SA.SmallArray HV.HTMLAttribute -> Maybe T.Text
lookupHTMLAttr name attrs = goSA attrs $ \(HV.HTMLAttribute k v) ->
  if k == name then Just v else Nothing

lookupHTMLChild :: T.Text -> SA.SmallArray HV.HTMLNode -> Maybe HV.HTMLNode
lookupHTMLChild name nodes = goSA nodes $ \case
  HV.HTMLElement local _ cs
    | local == name -> case SA.sizeofSmallArray cs of
        0 -> Just (HV.HTMLText T.empty)
        1 -> Just (SA.indexSmallArray cs 0)
        _ -> Just (HV.HTMLElement name (SA.smallArrayFromList []) cs)
  _ -> Nothing

-- | First-match scan over a 'SmallArray'.
goSA :: SA.SmallArray a -> (a -> Maybe b) -> Maybe b
goSA arr f = go 0
  where
    !n = SA.sizeofSmallArray arr
    go !i
      | i >= n    = Nothing
      | otherwise = case f (SA.indexSmallArray arr i) of
          Just b  -> Just b
          Nothing -> go (i + 1)

fromHTMLEnum :: [ConInfo] -> Q Exp
fromHTMLEnum cs = do
  v <- newName "v"
  s <- newName "s"
  branches <- mapM (enumDispatch s) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE (AppE (VarE 'mappend)
                  (LitE (StringL "HTML.Derive: unknown enum tag ")))
               (AppE (VarE 'T.unpack) (VarE s))
        )
      multi = MultiIfE
        (map (\(g, e) -> (g, AppE (ConE 'Right) e)) branches
         ++ [(NormalG (ConE 'True), AppE (ConE 'Left) (snd fallback))])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'HV.HTMLElement [varP s, wildP, wildP])
               (normalB (pure multi))
               []
       , match wildP
               (normalB [| Left "HTML.Derive: enum expected HTMLElement" |])
               []
       ])

enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendHTML (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp  = ConE (conInfoName c)
  pure (NormalG guardExp, bodyExp)

fromHTMLSum :: [ConInfo] -> Q Exp
fromHTMLSum cs = do
  v        <- newName "v"
  tagVar   <- newName "tag"
  childrenVar <- newName "kids"
  branches <- mapM (sumDispatch tagVar childrenVar) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE (ConE 'Left)
            (AppE (AppE (VarE 'mappend)
                      (LitE (StringL "HTML.Derive: unknown sum tag ")))
                  (AppE (VarE 'T.unpack) (VarE tagVar)))
        )
      multi = MultiIfE (branches ++ [fallback])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'HV.HTMLElement [varP tagVar, wildP, varP childrenVar])
               (normalB (pure multi))
               []
       , match wildP
               (normalB [| Left "HTML.Derive: sum expected HTMLElement" |])
               []
       ])

sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar childrenVar c = do
  mi <- reifyModifierInfoFor backendHTML (conInfoName c)
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
        [| H.fromHTML
             (SA.indexSmallArray $(varE childrenVar)
                                 $(litE (integerL (fromIntegral i)))) |]
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
  [| if SA.sizeofSmallArray $(varE childrenVar)
        == $(litE (integerL (fromIntegral arity)))
       then $(pure body)
       else Left ("HTML.Derive: " ++ conNameStr
                  ++ " expected " ++ show $(litE (integerL (fromIntegral arity)))
                  ++ " children, got "
                  ++ show (SA.sizeofSmallArray $(varE childrenVar))) |]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing  =
  fail "HTML.Derive: cannot derive HTML for non-record positional field"

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
