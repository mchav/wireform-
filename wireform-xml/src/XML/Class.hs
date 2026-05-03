{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclasses for XML serialization with GHC Generics support.
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- import GHC.Generics (Generic)
-- import XML.Class
--
-- data Person = Person { name :: Text, age :: Int } deriving (Generic)
-- instance ToXML Person
-- instance FromXML Person
--
-- let bs = encodeXML (Person \"John\" 30)
-- let Right p = decodeXML bs :: Either String Person
-- @
module XML.Class
  ( ToXML(..)
  , FromXML(..)
  , encodeXML
  , encodeXMLDirect
  , decodeXML
  , genericToEncoding
  , GToXML(..)
  , GFromXML(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB
import qualified Data.Text.Read as TR
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Version (Version, makeVersion, versionBranch)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics
import Numeric.Natural (Natural)

import XML.Value
import qualified XML.Encode as XE
import qualified XML.Decode as XD
import XML.Encoding (Encoding)
import qualified XML.Encoding as Enc

class ToXML a where
  toXML :: a -> Node
  default toXML :: (Generic a, GToXML (Rep a)) => a -> Node
  toXML = gToXML . from

  -- | aeson-style direct encoder. XML's nested tag balance and
  -- namespace context defeat a streaming 'Builder', so 'Encoding'
  -- wraps a fully-built 'Node'.
  toEncoding :: a -> Encoding
  toEncoding = Enc.node . toXML

class FromXML a where
  fromXML :: Node -> Either String a
  default fromXML :: (Generic a, GFromXML (Rep a)) => Node -> Either String a
  fromXML n = to <$> gFromXML n

-- | Convenience encode.
encodeXML :: ToXML a => a -> ByteString
encodeXML a = XE.encode (Document Nothing (toXML a))

-- | Encode directly via 'toEncoding'.
encodeXMLDirect :: ToXML a => a -> ByteString
encodeXMLDirect = Enc.encodingToByteString . toEncoding

genericToEncoding :: (Generic a, GToXML (Rep a)) => a -> Encoding
genericToEncoding = Enc.node . gToXML . from

-- | Convenience decode.
decodeXML :: FromXML a => ByteString -> Either String a
decodeXML bs = do
  doc <- XD.decode bs
  fromXML (docRoot doc)

-- Instances for standard types

instance ToXML Text where
  toXML = Text

instance FromXML Text where
  fromXML (Text t) = Right t
  fromXML (CData t) = Right t
  fromXML (Element _ _ cs)
    | V.null cs = Right T.empty
    | V.length cs == 1, Text t <- V.head cs = Right t
    | V.length cs == 1, CData t <- V.head cs = Right t
    | otherwise = Right (T.concat (V.toList (V.map extractText cs)))
  fromXML _ = Left "FromXML Text: expected text content"

extractText :: Node -> Text
extractText (Text t) = t
extractText (CData t) = t
extractText (Element _ _ cs) = T.concat (V.toList (V.map extractText cs))
extractText _ = T.empty

instance ToXML Int where
  toXML = Text . T.pack . show

instance FromXML Int where
  fromXML n = do
    t <- fromXML n
    case reads (T.unpack t) of
      [(v, "")] -> Right v
      _ -> Left $ "FromXML Int: cannot parse " ++ show t

instance ToXML Integer where
  toXML = Text . T.pack . show

instance FromXML Integer where
  fromXML n = do
    t <- fromXML n
    case reads (T.unpack t) of
      [(v, "")] -> Right v
      _ -> Left $ "FromXML Integer: cannot parse " ++ show t

instance ToXML Double where
  toXML = Text . T.pack . show

instance FromXML Double where
  fromXML n = do
    t <- fromXML n
    case reads (T.unpack t) of
      [(v, "")] -> Right v
      _ -> Left $ "FromXML Double: cannot parse " ++ show t

instance ToXML Bool where
  toXML True = Text "true"
  toXML False = Text "false"

instance FromXML Bool where
  fromXML n = do
    t <- fromXML n
    case T.toLower t of
      "true"  -> Right True
      "1"     -> Right True
      "false" -> Right False
      "0"     -> Right False
      _ -> Left $ "FromXML Bool: cannot parse " ++ show t

instance ToXML a => ToXML (Maybe a) where
  toXML Nothing = Element (simpleName "none") V.empty V.empty
  toXML (Just x) = toXML x

instance FromXML a => FromXML (Maybe a) where
  fromXML (Element n _ cs)
    | nameLocal n == "none" && V.null cs = Right Nothing
  fromXML node = Just <$> fromXML node

instance ToXML a => ToXML [a] where
  toXML xs = Element (simpleName "list") V.empty
    (V.fromList (map (\x -> Element (simpleName "item") V.empty (V.singleton (toXML x))) xs))

instance FromXML a => FromXML [a] where
  fromXML (Element _ _ cs) = traverse fromChild (V.toList cs)
    where
      fromChild (Element _ _ innerCs)
        | V.length innerCs == 1 = fromXML (V.head innerCs)
        | V.null innerCs = fromXML (Text T.empty)
        | otherwise = fromXML (Element (simpleName "wrapper") V.empty innerCs)
      fromChild n = fromXML n
  fromXML _ = Left "FromXML [a]: expected element with children"

instance ToXML a => ToXML (Vector a) where
  toXML xs = toXML (V.toList xs)

instance FromXML a => FromXML (Vector a) where
  fromXML n = V.fromList <$> fromXML n

instance (ToXML v) => ToXML (Map Text v) where
  toXML m = Element (simpleName "map") V.empty
    (V.fromList [ Element (simpleName k) V.empty (V.singleton (toXML v))
                | (k, v) <- Map.toList m ])

instance (FromXML v) => FromXML (Map Text v) where
  fromXML (Element _ _ cs) = do
    pairs <- traverse toPair (V.toList cs)
    Right (Map.fromList pairs)
    where
      toPair (Element name _ innerCs)
        | V.length innerCs == 1 = do
            v <- fromXML (V.head innerCs)
            Right (nameLocal name, v)
        | V.null innerCs = do
            v <- fromXML (Text T.empty)
            Right (nameLocal name, v)
        | otherwise = Left "FromXML Map: expected single child per entry"
      toPair _ = Left "FromXML Map: expected element children"
  fromXML _ = Left "FromXML Map: expected element"

-- Aeson-parity instances ---------------------------------------------------

intToText :: Int -> Text
intToText = TL.toStrict . TLB.toLazyText . TLB.decimal

readSignedDecimal :: (Integral a) => String -> Text -> Either String a
readSignedDecimal label t =
  case TR.signed TR.decimal (t :: Text) of
    Right (v, rest) | T.null rest -> Right v
    _ -> Left ("FromXML " <> label <> ": cannot parse " <> show t)

instance ToXML Char where
  toXML c = Text (T.singleton c)

instance FromXML Char where
  fromXML n = do
    t <- fromXML n
    case T.length t of
      1 -> Right (T.head t)
      _ -> Left "FromXML Char: expected single character"

instance ToXML Float where
  toXML = Text . T.pack . show

instance FromXML Float where
  fromXML n = do
    t <- fromXML n
    case TR.double t of
      Right (v, rest) | T.null rest -> Right (realToFrac (v :: Double))
      _ -> Left ("FromXML Float: cannot parse " <> T.unpack t)

instance ToXML Int8 where
  toXML = Text . intToText . fromIntegral

instance FromXML Int8 where
  fromXML n = fromXML n >>= readSignedDecimal "Int8"

instance ToXML Int16 where
  toXML = Text . intToText . fromIntegral

instance FromXML Int16 where
  fromXML n = fromXML n >>= readSignedDecimal "Int16"

instance ToXML Int32 where
  toXML = Text . intToText . fromIntegral

instance FromXML Int32 where
  fromXML n = fromXML n >>= readSignedDecimal "Int32"

instance ToXML Int64 where
  toXML = Text . intToText . fromIntegral

instance FromXML Int64 where
  fromXML n = fromXML n >>= readSignedDecimal "Int64"

instance ToXML Word where
  toXML = Text . intToText . fromIntegral

instance FromXML Word where
  fromXML n = fromXML n >>= readSignedDecimal "Word"

instance ToXML Word8 where
  toXML = Text . intToText . fromIntegral

instance FromXML Word8 where
  fromXML n = fromXML n >>= readSignedDecimal "Word8"

instance ToXML Word16 where
  toXML = Text . intToText . fromIntegral

instance FromXML Word16 where
  fromXML n = fromXML n >>= readSignedDecimal "Word16"

instance ToXML Word32 where
  toXML = Text . intToText . fromIntegral

instance FromXML Word32 where
  fromXML n = fromXML n >>= readSignedDecimal "Word32"

instance ToXML Word64 where
  toXML = Text . intToText . fromIntegral

instance FromXML Word64 where
  fromXML n = fromXML n >>= readSignedDecimal "Word64"

instance ToXML Natural where
  toXML = toXML . toInteger

instance FromXML Natural where
  fromXML n = do
    i <- fromXML n
    if (i :: Integer) < 0
      then Left "FromXML Natural: negative integer"
      else Right (fromInteger i)

instance ToXML TL.Text where
  toXML = Text . TL.toStrict

instance FromXML TL.Text where
  fromXML n = TL.fromStrict <$> fromXML n

-- | XML has no native binary type; bytes are encoded as their UTF-8
-- decoding.
instance ToXML ByteString where
  toXML = Text . TE.decodeUtf8

instance FromXML ByteString where
  fromXML n = TE.encodeUtf8 <$> fromXML n

instance ToXML BSL.ByteString where
  toXML = Text . TE.decodeUtf8 . BSL.toStrict

instance FromXML BSL.ByteString where
  fromXML n = BSL.fromStrict <$> fromXML n

instance ToXML () where
  toXML () = Element (simpleName "unit") V.empty V.empty

instance FromXML () where
  fromXML _ = Right ()

instance ToXML a => ToXML (NonEmpty a) where
  toXML = toXML . NE.toList

instance FromXML a => FromXML (NonEmpty a) where
  fromXML n = do
    xs <- fromXML n
    case xs of
      []     -> Left "FromXML NonEmpty: empty list"
      (y:ys) -> Right (y :| ys)

instance (ToXML a, ToXML b) => ToXML (Either a b) where
  toXML (Left  x) = Element (simpleName "Left")  V.empty (V.singleton (toXML x))
  toXML (Right x) = Element (simpleName "Right") V.empty (V.singleton (toXML x))

instance (FromXML a, FromXML b) => FromXML (Either a b) where
  fromXML (Element name _ cs)
    | nameLocal name == "Left",  V.length cs == 1 = Left  <$> fromXML (V.head cs)
    | nameLocal name == "Right", V.length cs == 1 = Right <$> fromXML (V.head cs)
  fromXML _ = Left "FromXML Either: expected Left/Right element"

instance (Ord a, ToXML a) => ToXML (Set a) where
  toXML = toXML . Set.toList

instance (Ord a, FromXML a) => FromXML (Set a) where
  fromXML n = Set.fromList <$> fromXML n

instance ToXML a => ToXML (Seq a) where
  toXML s = toXML (foldr (:) [] s)

instance FromXML a => FromXML (Seq a) where
  fromXML n = Seq.fromList <$> fromXML n

instance ToXML v => ToXML (HashMap Text v) where
  toXML m = Element (simpleName "map") V.empty
    (V.fromList [ Element (simpleName k) V.empty (V.singleton (toXML v))
                | (k, v) <- HM.toList m ])

instance FromXML v => FromXML (HashMap Text v) where
  fromXML n = do
    m <- fromXML n :: Either String (Map Text v)
    Right (HM.fromList (Map.toList m))

instance ToXML v => ToXML (IntMap v) where
  toXML m = Element (simpleName "map") V.empty
    (V.fromList [ Element (simpleName (intToText k)) V.empty (V.singleton (toXML v))
                | (k, v) <- IntMap.toList m ])

instance FromXML v => FromXML (IntMap v) where
  fromXML n = do
    m <- fromXML n :: Either String (Map Text v)
    pairs <- traverse decodePair (Map.toList m)
    Right (IntMap.fromList pairs)
    where
      decodePair (k, v) = case TR.signed TR.decimal k of
        Right (i, rest) | T.null rest -> Right (i, v)
        _ -> Left "FromXML IntMap: cannot parse Int key"

instance ToXML IntSet where
  toXML = toXML . IntSet.toList

instance FromXML IntSet where
  fromXML n = IntSet.fromList <$> fromXML n

instance (Hashable a, ToXML a) => ToXML (HashSet a) where
  toXML = toXML . HS.toList

instance (Eq a, Hashable a, FromXML a) => FromXML (HashSet a) where
  fromXML n = HS.fromList <$> fromXML n

instance (ToXML a, ToXML b) => ToXML (a, b) where
  toXML (a, b) = Element (simpleName "tuple") V.empty
    (V.fromList [Element (simpleName "_1") V.empty (V.singleton (toXML a))
                ,Element (simpleName "_2") V.empty (V.singleton (toXML b))])

instance (FromXML a, FromXML b) => FromXML (a, b) where
  fromXML (Element _ _ cs)
    | V.length cs == 2 = (,) <$> fromTupleField (cs V.! 0) <*> fromTupleField (cs V.! 1)
  fromXML _ = Left "FromXML (a,b): expected element with 2 children"

instance (ToXML a, ToXML b, ToXML c) => ToXML (a, b, c) where
  toXML (a, b, c) = Element (simpleName "tuple") V.empty
    (V.fromList [Element (simpleName "_1") V.empty (V.singleton (toXML a))
                ,Element (simpleName "_2") V.empty (V.singleton (toXML b))
                ,Element (simpleName "_3") V.empty (V.singleton (toXML c))])

instance (FromXML a, FromXML b, FromXML c) => FromXML (a, b, c) where
  fromXML (Element _ _ cs)
    | V.length cs == 3 =
        (,,) <$> fromTupleField (cs V.! 0)
             <*> fromTupleField (cs V.! 1)
             <*> fromTupleField (cs V.! 2)
  fromXML _ = Left "FromXML (a,b,c): expected element with 3 children"

instance (ToXML a, ToXML b, ToXML c, ToXML d) => ToXML (a, b, c, d) where
  toXML (a, b, c, d) = Element (simpleName "tuple") V.empty
    (V.fromList [Element (simpleName "_1") V.empty (V.singleton (toXML a))
                ,Element (simpleName "_2") V.empty (V.singleton (toXML b))
                ,Element (simpleName "_3") V.empty (V.singleton (toXML c))
                ,Element (simpleName "_4") V.empty (V.singleton (toXML d))])

instance (FromXML a, FromXML b, FromXML c, FromXML d) => FromXML (a, b, c, d) where
  fromXML (Element _ _ cs)
    | V.length cs == 4 =
        (,,,) <$> fromTupleField (cs V.! 0)
              <*> fromTupleField (cs V.! 1)
              <*> fromTupleField (cs V.! 2)
              <*> fromTupleField (cs V.! 3)
  fromXML _ = Left "FromXML (a,b,c,d): expected element with 4 children"

fromTupleField :: FromXML a => Node -> Either String a
fromTupleField (Element _ _ innerCs)
  | V.length innerCs == 1 = fromXML (V.head innerCs)
fromTupleField n = fromXML n

instance ToXML a => ToXML (Identity a) where
  toXML (Identity x) = toXML x

instance FromXML a => FromXML (Identity a) where
  fromXML n = Identity <$> fromXML n

instance ToXML a => ToXML (Const a b) where
  toXML (Const x) = toXML x

instance FromXML a => FromXML (Const a b) where
  fromXML n = Const <$> fromXML n

instance ToXML a => ToXML (Down a) where
  toXML (Down x) = toXML x

instance FromXML a => FromXML (Down a) where
  fromXML n = Down <$> fromXML n

instance ToXML Version where
  toXML = toXML . versionBranch

instance FromXML Version where
  fromXML n = makeVersion <$> fromXML n

instance (Integral a, ToXML a) => ToXML (Ratio a) where
  toXML r = Element (simpleName "ratio") V.empty
    (V.fromList [Element (simpleName "num") V.empty (V.singleton (toXML (numerator r)))
                ,Element (simpleName "den") V.empty (V.singleton (toXML (denominator r)))])

instance (Integral a, FromXML a) => FromXML (Ratio a) where
  fromXML (Element _ _ cs)
    | V.length cs == 2 = do
        n <- fromTupleField (cs V.! 0)
        d <- fromTupleField (cs V.! 1)
        if d == 0
          then Left "FromXML Ratio: zero denominator"
          else Right (n % d)
  fromXML _ = Left "FromXML Ratio: expected ratio element"

instance ToXML Node where
  toXML = id

instance FromXML Node where
  fromXML = Right

-- GHC.Generics support

class GToXML f where
  gToXML :: f p -> Node

class GFromXML f where
  gFromXML :: Node -> Either String (f p)

-- Datatype metadata
instance (Datatype d, GToXMLCon f) => GToXML (M1 D d f) where
  gToXML (M1 x) = gToXMLCon (datatypeName (undefined :: M1 D d f p)) x

instance (GFromXMLCon f) => GFromXML (M1 D d f) where
  gFromXML n = M1 <$> gFromXMLCon n

class GToXMLCon f where
  gToXMLCon :: String -> f p -> Node

class GFromXMLCon f where
  gFromXMLCon :: Node -> Either String (f p)

-- Constructor metadata
instance (GToXMLFields f) => GToXMLCon (M1 C c f) where
  gToXMLCon typeName (M1 x) =
    let fields = gToXMLFields x
        children = V.fromList [ Element (simpleName (T.pack k)) V.empty (V.singleton v)
                              | (k, v) <- fields ]
    in Element (simpleName (T.pack typeName)) V.empty children

instance (GFromXMLFields f) => GFromXMLCon (M1 C c f) where
  gFromXMLCon (Element _ _ cs) = M1 <$> gFromXMLFields (lookupChild cs)
  gFromXMLCon _ = Left "GFromXML: expected Element for record type"

lookupChild :: Vector Node -> Text -> Maybe Node
lookupChild cs name = go 0
  where
    !len = V.length cs
    go !i
      | i >= len = Nothing
      | Element n _ innerCs <- cs V.! i
      , nameLocal n == name =
          if V.length innerCs == 1
            then Just (V.head innerCs)
            else Just (Element n V.empty innerCs)
      | otherwise = go (i + 1)

class GToXMLFields f where
  gToXMLFields :: f p -> [(String, Node)]

class GFromXMLFields f where
  gFromXMLFields :: (Text -> Maybe Node) -> Either String (f p)

-- Product type
instance (GToXMLFields a, GToXMLFields b) => GToXMLFields (a :*: b) where
  gToXMLFields (a :*: b) = gToXMLFields a ++ gToXMLFields b

instance (GFromXMLFields a, GFromXMLFields b) => GFromXMLFields (a :*: b) where
  gFromXMLFields lkup = (:*:) <$> gFromXMLFields lkup <*> gFromXMLFields lkup

-- Selector (field)
instance (Selector s, ToXML a) => GToXMLFields (M1 S s (K1 i a)) where
  gToXMLFields m@(M1 (K1 x)) = [(selName m, toXML x)]

instance (Selector s, FromXML a) => GFromXMLFields (M1 S s (K1 i a)) where
  gFromXMLFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromXML: missing field " ++ T.unpack name
         Just v  -> M1 . K1 <$> fromXML v
