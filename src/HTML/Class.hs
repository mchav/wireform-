{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclass-based HTML serialization with GHC Generics support.
module HTML.Class
  ( ToHTML(..)
  , FromHTML(..)
  , encodeHTMLTyped
  , decodeHTMLTyped
  , GToHTML(..)
  , GFromHTML(..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics

import HTML.Value
import qualified HTML.Encode as HE
import qualified HTML.Parse as HP

class ToHTML a where
  toHTML :: a -> HTMLNode
  default toHTML :: (Generic a, GToHTML (Rep a)) => a -> HTMLNode
  toHTML = gToHTML . from

class FromHTML a where
  fromHTML :: HTMLNode -> Either String a
  default fromHTML :: (Generic a, GFromHTML (Rep a)) => HTMLNode -> Either String a
  fromHTML n = to <$> gFromHTML n

encodeHTMLTyped :: ToHTML a => a -> ByteString
encodeHTMLTyped a = HE.encodeHTML (HTMLDocument Nothing (toHTML a))

decodeHTMLTyped :: FromHTML a => ByteString -> Either String a
decodeHTMLTyped bs =
  let !doc = HP.parseHTML bs
  in fromHTML (htmlRoot doc)

instance ToHTML Text where
  toHTML = HTMLText

instance FromHTML Text where
  fromHTML (HTMLText t) = Right t
  fromHTML n = Right (textContent n)

instance ToHTML Int where
  toHTML = HTMLText . T.pack . show

instance FromHTML Int where
  fromHTML n = do
    t <- fromHTML n
    case reads (T.unpack (t :: Text)) of
      [(v, "")] -> Right v
      _ -> Left $ "FromHTML Int: cannot parse " ++ show t

instance ToHTML Integer where
  toHTML = HTMLText . T.pack . show

instance FromHTML Integer where
  fromHTML n = do
    t <- fromHTML n
    case reads (T.unpack (t :: Text)) of
      [(v, "")] -> Right v
      _ -> Left $ "FromHTML Integer: cannot parse " ++ show t

instance ToHTML Double where
  toHTML = HTMLText . T.pack . show

instance FromHTML Double where
  fromHTML n = do
    t <- fromHTML n
    case reads (T.unpack (t :: Text)) of
      [(v, "")] -> Right v
      _ -> Left $ "FromHTML Double: cannot parse " ++ show t

instance ToHTML Bool where
  toHTML True = HTMLText "true"
  toHTML False = HTMLText "false"

instance FromHTML Bool where
  fromHTML n = do
    t <- fromHTML n
    case T.toLower (t :: Text) of
      "true"  -> Right True
      "1"     -> Right True
      "false" -> Right False
      "0"     -> Right False
      _ -> Left $ "FromHTML Bool: cannot parse " ++ show t

instance ToHTML a => ToHTML (Maybe a) where
  toHTML Nothing = HTMLElement "span" V.empty V.empty
  toHTML (Just x) = toHTML x

instance FromHTML a => FromHTML (Maybe a) where
  fromHTML (HTMLElement "span" _ cs)
    | V.null cs = Right Nothing
  fromHTML n = Just <$> fromHTML n

instance ToHTML a => ToHTML [a] where
  toHTML xs = HTMLElement "ul" V.empty
    (V.fromList [HTMLElement "li" V.empty (V.singleton (toHTML x)) | x <- xs])

instance FromHTML a => FromHTML [a] where
  fromHTML (HTMLElement _ _ cs) = traverse fromChild (V.toList cs)
    where
      fromChild (HTMLElement _ _ innerCs)
        | V.length innerCs == 1 = fromHTML (V.head innerCs)
        | V.null innerCs = fromHTML (HTMLText T.empty)
        | otherwise = Left "FromHTML [a]: expected single child per item"
      fromChild n = fromHTML n
  fromHTML _ = Left "FromHTML [a]: expected element"

instance ToHTML a => ToHTML (Vector a) where
  toHTML = toHTML . V.toList

instance FromHTML a => FromHTML (Vector a) where
  fromHTML n = V.fromList <$> fromHTML n

instance ToHTML HTMLNode where
  toHTML = id

instance FromHTML HTMLNode where
  fromHTML = Right

-- GHC.Generics support

class GToHTML f where
  gToHTML :: f p -> HTMLNode

class GFromHTML f where
  gFromHTML :: HTMLNode -> Either String (f p)

instance (Datatype d, GToHTMLCon f) => GToHTML (M1 D d f) where
  gToHTML (M1 x) = gToHTMLCon (datatypeName (undefined :: M1 D d f p)) x

instance GFromHTMLCon f => GFromHTML (M1 D d f) where
  gFromHTML n = M1 <$> gFromHTMLCon n

class GToHTMLCon f where
  gToHTMLCon :: String -> f p -> HTMLNode

class GFromHTMLCon f where
  gFromHTMLCon :: HTMLNode -> Either String (f p)

instance GToHTMLFields f => GToHTMLCon (M1 C c f) where
  gToHTMLCon typeName (M1 x) =
    let fields = gToHTMLFields x
        children = V.fromList
          [ HTMLElement (T.pack k) V.empty (V.singleton v)
          | (k, v) <- fields ]
    in HTMLElement (T.pack typeName) V.empty children

instance GFromHTMLFields f => GFromHTMLCon (M1 C c f) where
  gFromHTMLCon (HTMLElement _ _ cs) = M1 <$> gFromHTMLFields (lookupChild cs)
  gFromHTMLCon _ = Left "GFromHTML: expected HTMLElement for record type"

lookupChild :: Vector HTMLNode -> Text -> Maybe HTMLNode
lookupChild cs name = go 0
  where
    !len = V.length cs
    go !i
      | i >= len = Nothing
      | HTMLElement n _ innerCs <- cs V.! i
      , n == name =
          if V.length innerCs == 1
            then Just (V.head innerCs)
            else Just (HTMLElement n V.empty innerCs)
      | otherwise = go (i + 1)

class GToHTMLFields f where
  gToHTMLFields :: f p -> [(String, HTMLNode)]

class GFromHTMLFields f where
  gFromHTMLFields :: (Text -> Maybe HTMLNode) -> Either String (f p)

instance (GToHTMLFields a, GToHTMLFields b) => GToHTMLFields (a :*: b) where
  gToHTMLFields (a :*: b) = gToHTMLFields a ++ gToHTMLFields b

instance (GFromHTMLFields a, GFromHTMLFields b) => GFromHTMLFields (a :*: b) where
  gFromHTMLFields lkup = (:*:) <$> gFromHTMLFields lkup <*> gFromHTMLFields lkup

instance (Selector s, ToHTML a) => GToHTMLFields (M1 S s (K1 i a)) where
  gToHTMLFields m@(M1 (K1 x)) = [(selName m, toHTML x)]

instance (Selector s, FromHTML a) => GFromHTMLFields (M1 S s (K1 i a)) where
  gFromHTMLFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromHTML: missing field " ++ T.unpack name
         Just v  -> M1 . K1 <$> fromHTML v
