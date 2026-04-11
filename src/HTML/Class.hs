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
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB
import qualified Data.Text.Lazy.Builder.RealFloat as TLB
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList, sizeofSmallArray, indexSmallArray)
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
  toHTML = HTMLText . TL.toStrict . TLB.toLazyText . TLB.decimal

instance FromHTML Int where
  fromHTML n = do
    t <- fromHTML n
    case TR.signed TR.decimal (t :: Text) of
      Right (v, rest) | T.null rest -> Right v
      _ -> Left $ "FromHTML Int: cannot parse " <> T.unpack t

instance ToHTML Integer where
  toHTML = HTMLText . TL.toStrict . TLB.toLazyText . TLB.decimal

instance FromHTML Integer where
  fromHTML n = do
    t <- fromHTML n
    case TR.signed TR.decimal (t :: Text) of
      Right (v, rest) | T.null rest -> Right v
      _ -> Left $ "FromHTML Integer: cannot parse " <> T.unpack t

instance ToHTML Double where
  toHTML = HTMLText . TL.toStrict . TLB.toLazyText . TLB.realFloat

instance FromHTML Double where
  fromHTML n = do
    t <- fromHTML n
    case TR.double (t :: Text) of
      Right (v, rest) | T.null rest -> Right v
      _ -> Left $ "FromHTML Double: cannot parse " <> T.unpack t

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
      _ -> Left $ "FromHTML Bool: cannot parse " <> T.unpack t

instance ToHTML a => ToHTML (Maybe a) where
  toHTML Nothing = HTMLElement "span" V.empty mempty
  toHTML (Just x) = toHTML x

instance FromHTML a => FromHTML (Maybe a) where
  fromHTML (HTMLElement "span" _ cs)
    | sizeofSmallArray cs == 0 = Right Nothing
  fromHTML n = Just <$> fromHTML n

instance ToHTML a => ToHTML [a] where
  toHTML xs = HTMLElement "ul" V.empty
    (smallArrayFromList (map (\x -> HTMLElement "li" V.empty (smallArrayFromList [toHTML x])) xs))

instance FromHTML a => FromHTML [a] where
  fromHTML (HTMLElement _ _ cs) = traverse fromChild (toList cs)
    where
      fromChild (HTMLElement _ _ innerCs)
        | sizeofSmallArray innerCs == 1 = fromHTML (indexSmallArray innerCs 0)
        | sizeofSmallArray innerCs == 0 = fromHTML (HTMLText T.empty)
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
        children = smallArrayFromList
          (map (\(k, v) -> HTMLElement (T.pack k) V.empty (smallArrayFromList [v])) fields)
    in HTMLElement (T.pack typeName) V.empty children

instance GFromHTMLFields f => GFromHTMLCon (M1 C c f) where
  gFromHTMLCon (HTMLElement _ _ cs) = M1 <$> gFromHTMLFields (lookupChild cs)
  gFromHTMLCon _ = Left "GFromHTML: expected HTMLElement for record type"

lookupChild :: SmallArray HTMLNode -> Text -> Maybe HTMLNode
lookupChild cs name = go 0
  where
    !len = sizeofSmallArray cs
    go !i
      | i >= len = Nothing
      | HTMLElement n _ innerCs <- indexSmallArray cs i
      , n == name =
          if sizeofSmallArray innerCs == 1
            then Just (indexSmallArray innerCs 0)
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
         Nothing -> Left $ "GFromHTML: missing field " <> T.unpack name
         Just v  -> M1 . K1 <$> fromHTML v
