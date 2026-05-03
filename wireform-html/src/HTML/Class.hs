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
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB
import qualified Data.Text.Lazy.Builder.RealFloat as TLB
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList, sizeofSmallArray, indexSmallArray)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Version (Version, makeVersion, versionBranch)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics
import Numeric.Natural (Natural)

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
  toHTML Nothing = HTMLElement "span" mempty mempty
  toHTML (Just x) = toHTML x

instance FromHTML a => FromHTML (Maybe a) where
  fromHTML (HTMLElement "span" _ cs)
    | sizeofSmallArray cs == 0 = Right Nothing
  fromHTML n = Just <$> fromHTML n

instance ToHTML a => ToHTML [a] where
  toHTML xs = HTMLElement "ul" mempty
    (smallArrayFromList (map (\x -> HTMLElement "li" mempty (smallArrayFromList [toHTML x])) xs))

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

-- Aeson-parity instances ---------------------------------------------------

intToHTML :: Int -> HTMLNode
intToHTML = HTMLText . TL.toStrict . TLB.toLazyText . TLB.decimal

readSignedDec :: Integral a => String -> Text -> Either String a
readSignedDec label t =
  case TR.signed TR.decimal (t :: Text) of
    Right (v, rest) | T.null rest -> Right v
    _ -> Left ("FromHTML " <> label <> ": cannot parse " <> T.unpack t)

instance ToHTML Char where
  toHTML c = HTMLText (T.singleton c)

instance FromHTML Char where
  fromHTML n = do
    t <- fromHTML n
    case T.length t of
      1 -> Right (T.head t)
      _ -> Left "FromHTML Char: expected single character"

instance ToHTML Float where
  toHTML = HTMLText . TL.toStrict . TLB.toLazyText . TLB.realFloat

instance FromHTML Float where
  fromHTML n = do
    t <- fromHTML n
    case TR.double t of
      Right (v, rest) | T.null rest -> Right (realToFrac (v :: Double))
      _ -> Left ("FromHTML Float: cannot parse " <> T.unpack t)

instance ToHTML Int8 where
  toHTML = intToHTML . fromIntegral

instance FromHTML Int8 where
  fromHTML n = fromHTML n >>= readSignedDec "Int8"

instance ToHTML Int16 where
  toHTML = intToHTML . fromIntegral

instance FromHTML Int16 where
  fromHTML n = fromHTML n >>= readSignedDec "Int16"

instance ToHTML Int32 where
  toHTML = intToHTML . fromIntegral

instance FromHTML Int32 where
  fromHTML n = fromHTML n >>= readSignedDec "Int32"

instance ToHTML Int64 where
  toHTML = intToHTML . fromIntegral

instance FromHTML Int64 where
  fromHTML n = fromHTML n >>= readSignedDec "Int64"

instance ToHTML Word where
  toHTML = intToHTML . fromIntegral

instance FromHTML Word where
  fromHTML n = fromHTML n >>= readSignedDec "Word"

instance ToHTML Word8 where
  toHTML = intToHTML . fromIntegral

instance FromHTML Word8 where
  fromHTML n = fromHTML n >>= readSignedDec "Word8"

instance ToHTML Word16 where
  toHTML = intToHTML . fromIntegral

instance FromHTML Word16 where
  fromHTML n = fromHTML n >>= readSignedDec "Word16"

instance ToHTML Word32 where
  toHTML = intToHTML . fromIntegral

instance FromHTML Word32 where
  fromHTML n = fromHTML n >>= readSignedDec "Word32"

instance ToHTML Word64 where
  toHTML = intToHTML . fromIntegral

instance FromHTML Word64 where
  fromHTML n = fromHTML n >>= readSignedDec "Word64"

instance ToHTML Natural where
  toHTML = toHTML . toInteger

instance FromHTML Natural where
  fromHTML n = do
    i <- fromHTML n
    if (i :: Integer) < 0
      then Left "FromHTML Natural: negative integer"
      else Right (fromInteger i)

instance ToHTML TL.Text where
  toHTML = HTMLText . TL.toStrict

instance FromHTML TL.Text where
  fromHTML n = TL.fromStrict <$> fromHTML n

instance ToHTML ByteString where
  toHTML = HTMLText . TE.decodeUtf8

instance FromHTML ByteString where
  fromHTML n = TE.encodeUtf8 <$> fromHTML n

instance ToHTML BSL.ByteString where
  toHTML = HTMLText . TE.decodeUtf8 . BSL.toStrict

instance FromHTML BSL.ByteString where
  fromHTML n = BSL.fromStrict <$> fromHTML n

instance ToHTML () where
  toHTML () = HTMLElement "span" mempty mempty

instance FromHTML () where
  fromHTML _ = Right ()

instance ToHTML a => ToHTML (NonEmpty a) where
  toHTML = toHTML . NE.toList

instance FromHTML a => FromHTML (NonEmpty a) where
  fromHTML n = do
    xs <- fromHTML n
    case xs of
      []     -> Left "FromHTML NonEmpty: empty list"
      (y:ys) -> Right (y :| ys)

instance (ToHTML a, ToHTML b) => ToHTML (Either a b) where
  toHTML (Left  x) = HTMLElement "div" mempty (smallArrayFromList [HTMLElement "left"  mempty (smallArrayFromList [toHTML x])])
  toHTML (Right x) = HTMLElement "div" mempty (smallArrayFromList [HTMLElement "right" mempty (smallArrayFromList [toHTML x])])

instance (FromHTML a, FromHTML b) => FromHTML (Either a b) where
  fromHTML (HTMLElement _ _ cs)
    | sizeofSmallArray cs == 1
    , HTMLElement tag _ inner <- indexSmallArray cs 0
    , sizeofSmallArray inner == 1 = case tag of
        "left"  -> Left  <$> fromHTML (indexSmallArray inner 0)
        "right" -> Right <$> fromHTML (indexSmallArray inner 0)
        _       -> Left "FromHTML Either: expected left/right child"
  fromHTML _ = Left "FromHTML Either: expected single child"

instance (Ord a, ToHTML a) => ToHTML (Set a) where
  toHTML = toHTML . Set.toList

instance (Ord a, FromHTML a) => FromHTML (Set a) where
  fromHTML n = Set.fromList <$> fromHTML n

instance ToHTML a => ToHTML (Seq a) where
  toHTML s = toHTML (foldr (:) [] s)

instance FromHTML a => FromHTML (Seq a) where
  fromHTML n = Seq.fromList <$> fromHTML n

instance ToHTML v => ToHTML (Map Text v) where
  toHTML m = HTMLElement "dl" mempty
    (smallArrayFromList
      (concatMap
        (\(k, v) ->
          [ HTMLElement "dt" mempty (smallArrayFromList [HTMLText k])
          , HTMLElement "dd" mempty (smallArrayFromList [toHTML v])
          ])
        (Map.toList m)))

instance FromHTML v => FromHTML (Map Text v) where
  fromHTML (HTMLElement _ _ cs) = Map.fromList <$> goPairs (toList cs)
    where
      goPairs (HTMLElement "dt" _ k : HTMLElement "dd" _ vCs : rest)
        | sizeofSmallArray k >= 1
        , HTMLText keyText <- indexSmallArray k 0
        , sizeofSmallArray vCs == 1 = do
            v <- fromHTML (indexSmallArray vCs 0)
            ((keyText, v) :) <$> goPairs rest
      goPairs [] = Right []
      goPairs _  = Left "FromHTML (Map Text v): expected dt/dd pairs"
  fromHTML _ = Left "FromHTML (Map Text v): expected dl element"

instance ToHTML v => ToHTML (HashMap Text v) where
  toHTML = toHTML . Map.fromList . HM.toList

instance FromHTML v => FromHTML (HashMap Text v) where
  fromHTML n = do
    m <- fromHTML n :: Either String (Map Text v)
    Right (HM.fromList (Map.toList m))

instance ToHTML v => ToHTML (IntMap v) where
  toHTML m = toHTML (Map.fromList [(T.pack (show k), v) | (k, v) <- IntMap.toList m])

instance FromHTML v => FromHTML (IntMap v) where
  fromHTML n = do
    m <- fromHTML n :: Either String (Map Text v)
    pairs <- traverse decodePair (Map.toList m)
    Right (IntMap.fromList pairs)
    where
      decodePair (k, v) = case TR.signed TR.decimal k of
        Right (i, rest) | T.null rest -> Right (i, v)
        _ -> Left "FromHTML IntMap: cannot parse Int key"

instance ToHTML IntSet where
  toHTML = toHTML . IntSet.toList

instance FromHTML IntSet where
  fromHTML n = IntSet.fromList <$> fromHTML n

instance (Hashable a, ToHTML a) => ToHTML (HashSet a) where
  toHTML = toHTML . HS.toList

instance (Eq a, Hashable a, FromHTML a) => FromHTML (HashSet a) where
  fromHTML n = HS.fromList <$> fromHTML n

instance (ToHTML a, ToHTML b) => ToHTML (a, b) where
  toHTML (a, b) = HTMLElement "tuple" mempty
    (smallArrayFromList [toHTML a, toHTML b])

instance (FromHTML a, FromHTML b) => FromHTML (a, b) where
  fromHTML (HTMLElement _ _ cs)
    | sizeofSmallArray cs == 2 =
        (,) <$> fromHTML (indexSmallArray cs 0)
            <*> fromHTML (indexSmallArray cs 1)
  fromHTML _ = Left "FromHTML (a,b): expected element with 2 children"

instance (ToHTML a, ToHTML b, ToHTML c) => ToHTML (a, b, c) where
  toHTML (a, b, c) = HTMLElement "tuple" mempty
    (smallArrayFromList [toHTML a, toHTML b, toHTML c])

instance (FromHTML a, FromHTML b, FromHTML c) => FromHTML (a, b, c) where
  fromHTML (HTMLElement _ _ cs)
    | sizeofSmallArray cs == 3 =
        (,,) <$> fromHTML (indexSmallArray cs 0)
             <*> fromHTML (indexSmallArray cs 1)
             <*> fromHTML (indexSmallArray cs 2)
  fromHTML _ = Left "FromHTML (a,b,c): expected element with 3 children"

instance (ToHTML a, ToHTML b, ToHTML c, ToHTML d) => ToHTML (a, b, c, d) where
  toHTML (a, b, c, d) = HTMLElement "tuple" mempty
    (smallArrayFromList [toHTML a, toHTML b, toHTML c, toHTML d])

instance (FromHTML a, FromHTML b, FromHTML c, FromHTML d) => FromHTML (a, b, c, d) where
  fromHTML (HTMLElement _ _ cs)
    | sizeofSmallArray cs == 4 =
        (,,,) <$> fromHTML (indexSmallArray cs 0)
              <*> fromHTML (indexSmallArray cs 1)
              <*> fromHTML (indexSmallArray cs 2)
              <*> fromHTML (indexSmallArray cs 3)
  fromHTML _ = Left "FromHTML (a,b,c,d): expected element with 4 children"

instance ToHTML a => ToHTML (Identity a) where
  toHTML (Identity x) = toHTML x

instance FromHTML a => FromHTML (Identity a) where
  fromHTML n = Identity <$> fromHTML n

instance ToHTML a => ToHTML (Const a b) where
  toHTML (Const x) = toHTML x

instance FromHTML a => FromHTML (Const a b) where
  fromHTML n = Const <$> fromHTML n

instance ToHTML a => ToHTML (Down a) where
  toHTML (Down x) = toHTML x

instance FromHTML a => FromHTML (Down a) where
  fromHTML n = Down <$> fromHTML n

instance ToHTML Version where
  toHTML = toHTML . versionBranch

instance FromHTML Version where
  fromHTML n = makeVersion <$> fromHTML n

instance (Integral a, ToHTML a) => ToHTML (Ratio a) where
  toHTML r = toHTML (numerator r, denominator r)

instance (Integral a, FromHTML a) => FromHTML (Ratio a) where
  fromHTML n = do
    (num, den) <- fromHTML n
    if den == 0
      then Left "FromHTML Ratio: zero denominator"
      else Right (num % den)

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
          (map (\(k, v) -> HTMLElement (T.pack k) mempty (smallArrayFromList [v])) fields)
    in HTMLElement (T.pack typeName) mempty children

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
            else Just (HTMLElement n mempty innerCs)
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
