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
  , decodeXML
  , GToXML(..)
  , GFromXML(..)
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics

import XML.Value
import qualified XML.Encode as XE
import qualified XML.Decode as XD

class ToXML a where
  toXML :: a -> Node
  default toXML :: (Generic a, GToXML (Rep a)) => a -> Node
  toXML = gToXML . from

class FromXML a where
  fromXML :: Node -> Either String a
  default fromXML :: (Generic a, GFromXML (Rep a)) => Node -> Either String a
  fromXML n = to <$> gFromXML n

-- | Convenience encode.
encodeXML :: ToXML a => a -> ByteString
encodeXML a = XE.encode (Document Nothing (toXML a))

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
