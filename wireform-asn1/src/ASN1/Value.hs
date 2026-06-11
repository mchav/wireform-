{- | ASN.1 value representation, covering the common universal types.

ASN.1 (Abstract Syntax Notation One) is the ITU-T standard for
describing data structures. This module defines values for common
universal tags: BOOLEAN, INTEGER, BIT STRING, OCTET STRING,
NULL, OID, UTF8String, PrintableString, IA5String, UTCTime,
GeneralizedTime, SEQUENCE, SET, and context-tagged values.

@
import qualified ASN1.Value as A
import qualified ASN1.Encode as AE
import qualified ASN1.Decode as AD
import qualified Data.Vector as V

let val = A.Sequence (V.fromList [A.Integer 42, A.UTF8String \"hello\"])
let bytes = AE.encode val
let Right decoded = AD.decode bytes
@
-}
module ASN1.Value (
  Value (..),
  TagClass (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word64)
import GHC.Generics (Generic)


data TagClass = Universal | Application | ContextSpecific | Private
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


data Value
  = Boolean !Bool
  | Integer !Integer
  | BitString !Int !ByteString
  | OctetString !ByteString
  | Null
  | OID !(Vector Word64)
  | UTF8String !Text
  | PrintableString !Text
  | IA5String !Text
  | UTCTime !Text
  | GeneralizedTime !Text
  | Sequence !(Vector Value)
  | Set !(Vector Value)
  | Tagged !TagClass !Int !Value
  | Other !TagClass !Bool !Int !ByteString
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


instance Aeson.ToJSON Value where
  toJSON (Boolean b) = Aeson.Bool b
  toJSON (Integer n) = Aeson.Number (fromInteger n)
  toJSON (BitString _ bs) = Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  toJSON (OctetString bs) = Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  toJSON Null = Aeson.Null
  toJSON (OID components) =
    Aeson.String $
      T.intercalate "." $
        map (T.pack . show) (V.toList components)
  toJSON (UTF8String t) = Aeson.String t
  toJSON (PrintableString t) = Aeson.String t
  toJSON (IA5String t) = Aeson.String t
  toJSON (UTCTime t) = Aeson.String t
  toJSON (GeneralizedTime t) = Aeson.String t
  toJSON (Sequence vs) = Aeson.Array (V.map Aeson.toJSON vs)
  toJSON (Set vs) = Aeson.Array (V.map Aeson.toJSON vs)
  toJSON (Tagged _ tag v) =
    Aeson.Object $
      KM.fromList
        [(Key.fromText (T.pack (show tag)), Aeson.toJSON v)]
  toJSON (Other _ _ tag bs) =
    Aeson.Object $
      KM.fromList
        [(Key.fromText (T.pack (show tag)), Aeson.String (TE.decodeUtf8 (Base64.encode bs)))]
