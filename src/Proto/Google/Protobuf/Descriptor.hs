{-# LANGUAGE BangPatterns #-}
-- | Haskell types for @google/protobuf/descriptor.proto@.
--
-- These are the core descriptor types used by protoc and other protobuf tools.
-- They represent the parsed form of .proto files and are used for:
--
-- * protoc plugin communication (CodeGeneratorRequest/Response)
-- * Runtime reflection and descriptor pools
-- * Dynamic message construction
module Proto.Google.Protobuf.Descriptor
  ( -- * File descriptor
    FileDescriptorProto (..)
  , defaultFileDescriptorProto

    -- * Message descriptor
  , DescriptorProto (..)
  , defaultDescriptorProto

    -- * Field descriptor
  , FieldDescriptorProto (..)
  , defaultFieldDescriptorProto
  , FieldDescriptorType (..)
  , FieldDescriptorLabel (..)

    -- * Enum descriptor
  , EnumDescriptorProto (..)
  , defaultEnumDescriptorProto
  , EnumValueDescriptorProto (..)
  , defaultEnumValueDescriptorProto

    -- * Service descriptor
  , ServiceDescriptorProto (..)
  , defaultServiceDescriptorProto
  , MethodDescriptorProto (..)
  , defaultMethodDescriptorProto

    -- * Oneof descriptor
  , OneofDescriptorProto (..)
  , defaultOneofDescriptorProto

    -- * File descriptor set
  , FileDescriptorSet (..)
  , defaultFileDescriptorSet
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Wire (Tag(..))
import Proto.Wire.Encode (fieldVarintSize, fieldTextSize, fieldBytesSize, fieldBoolSize)

newtype FileDescriptorSet = FileDescriptorSet
  { fdsFile :: V.Vector FileDescriptorProto
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultFileDescriptorSet :: FileDescriptorSet
defaultFileDescriptorSet = FileDescriptorSet V.empty

instance MessageEncode FileDescriptorSet where
  buildMessage (FileDescriptorSet fs) =
    V.foldl' (\acc f -> acc <> encodeFieldMessage 1 f) mempty fs

instance MessageDecode FileDescriptorSet where
  messageDecoder = loop V.empty
    where
      loop !fs = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure (FileDescriptorSet fs)
          UJust (Tag 1 _) -> do f <- decodeFieldMessage; loop (V.snoc fs f)
          UJust (Tag _ wt) -> skipField wt >> loop fs

data FileDescriptorProto = FileDescriptorProto
  { fdpName            :: !Text
  , fdpPackage         :: !Text
  , fdpDependency      :: !(V.Vector Text)
  , fdpMessageType     :: !(V.Vector DescriptorProto)
  , fdpEnumType        :: !(V.Vector EnumDescriptorProto)
  , fdpService         :: !(V.Vector ServiceDescriptorProto)
  , fdpSourceCodeInfo  :: !ByteString
  , fdpSyntax          :: !Text
  , fdpEdition         :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultFileDescriptorProto :: FileDescriptorProto
defaultFileDescriptorProto = FileDescriptorProto "" "" V.empty V.empty V.empty V.empty "" "" ""

instance MessageEncode FileDescriptorProto where
  buildMessage fdp =
    (if fdpName fdp == "" then mempty else encodeFieldString 1 (fdpName fdp)) <>
    (if fdpPackage fdp == "" then mempty else encodeFieldString 2 (fdpPackage fdp)) <>
    V.foldl' (\a d -> a <> encodeFieldString 3 d) mempty (fdpDependency fdp) <>
    V.foldl' (\a m -> a <> encodeFieldMessage 4 m) mempty (fdpMessageType fdp) <>
    V.foldl' (\a e -> a <> encodeFieldMessage 5 e) mempty (fdpEnumType fdp) <>
    V.foldl' (\a s -> a <> encodeFieldMessage 6 s) mempty (fdpService fdp) <>
    (if fdpSyntax fdp == "" then mempty else encodeFieldString 12 (fdpSyntax fdp)) <>
    (if fdpEdition fdp == "" then mempty else encodeFieldString 14 (fdpEdition fdp))

instance MessageDecode FileDescriptorProto where
  messageDecoder = loop defaultFileDescriptorProto
    where
      loop !fdp = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure fdp
          UJust (Tag 1 _)  -> do v <- decodeFieldString; loop fdp { fdpName = v }
          UJust (Tag 2 _)  -> do v <- decodeFieldString; loop fdp { fdpPackage = v }
          UJust (Tag 3 _)  -> do v <- decodeFieldString; loop fdp { fdpDependency = V.snoc (fdpDependency fdp) v }
          UJust (Tag 4 _)  -> do v <- decodeFieldMessage; loop fdp { fdpMessageType = V.snoc (fdpMessageType fdp) v }
          UJust (Tag 5 _)  -> do v <- decodeFieldMessage; loop fdp { fdpEnumType = V.snoc (fdpEnumType fdp) v }
          UJust (Tag 6 _)  -> do v <- decodeFieldMessage; loop fdp { fdpService = V.snoc (fdpService fdp) v }
          UJust (Tag 12 _) -> do v <- decodeFieldString; loop fdp { fdpSyntax = v }
          UJust (Tag 14 _) -> do v <- decodeFieldString; loop fdp { fdpEdition = v }
          UJust (Tag _ wt) -> skipField wt >> loop fdp

data DescriptorProto = DescriptorProto
  { dpName           :: !Text
  , dpField          :: !(V.Vector FieldDescriptorProto)
  , dpNestedType     :: !(V.Vector DescriptorProto)
  , dpEnumType       :: !(V.Vector EnumDescriptorProto)
  , dpOneofDecl      :: !(V.Vector OneofDescriptorProto)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultDescriptorProto :: DescriptorProto
defaultDescriptorProto = DescriptorProto "" V.empty V.empty V.empty V.empty

instance MessageEncode DescriptorProto where
  buildMessage dp =
    (if dpName dp == "" then mempty else encodeFieldString 1 (dpName dp)) <>
    V.foldl' (\a f -> a <> encodeFieldMessage 2 f) mempty (dpField dp) <>
    V.foldl' (\a n -> a <> encodeFieldMessage 3 n) mempty (dpNestedType dp) <>
    V.foldl' (\a e -> a <> encodeFieldMessage 4 e) mempty (dpEnumType dp) <>
    V.foldl' (\a o -> a <> encodeFieldMessage 8 o) mempty (dpOneofDecl dp)

instance MessageDecode DescriptorProto where
  messageDecoder = loop defaultDescriptorProto
    where
      loop !dp = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure dp
          UJust (Tag 1 _) -> do v <- decodeFieldString; loop dp { dpName = v }
          UJust (Tag 2 _) -> do v <- decodeFieldMessage; loop dp { dpField = V.snoc (dpField dp) v }
          UJust (Tag 3 _) -> do v <- decodeFieldMessage; loop dp { dpNestedType = V.snoc (dpNestedType dp) v }
          UJust (Tag 4 _) -> do v <- decodeFieldMessage; loop dp { dpEnumType = V.snoc (dpEnumType dp) v }
          UJust (Tag 8 _) -> do v <- decodeFieldMessage; loop dp { dpOneofDecl = V.snoc (dpOneofDecl dp) v }
          UJust (Tag _ wt) -> skipField wt >> loop dp

data FieldDescriptorType
  = TYPE_DOUBLE | TYPE_FLOAT | TYPE_INT64 | TYPE_UINT64 | TYPE_INT32
  | TYPE_FIXED64 | TYPE_FIXED32 | TYPE_BOOL | TYPE_STRING | TYPE_GROUP
  | TYPE_MESSAGE | TYPE_BYTES | TYPE_UINT32 | TYPE_ENUM | TYPE_SFIXED32
  | TYPE_SFIXED64 | TYPE_SINT32 | TYPE_SINT64
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass NFData

data FieldDescriptorLabel
  = LABEL_OPTIONAL | LABEL_REQUIRED | LABEL_REPEATED
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass NFData

data FieldDescriptorProto = FieldDescriptorProto
  { fdpFieldName     :: !Text
  , fdpFieldNumber   :: !Int32
  , fdpFieldLabel    :: !Int32
  , fdpFieldType     :: !Int32
  , fdpFieldTypeName :: !Text
  , fdpFieldDefault  :: !Text
  , fdpFieldOneofIdx :: !Int32
  , fdpFieldJsonName :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultFieldDescriptorProto :: FieldDescriptorProto
defaultFieldDescriptorProto = FieldDescriptorProto "" 0 0 0 "" "" (-1) ""

instance MessageEncode FieldDescriptorProto where
  buildMessage f =
    (if fdpFieldName f == "" then mempty else encodeFieldString 1 (fdpFieldName f)) <>
    (if fdpFieldNumber f == 0 then mempty else encodeFieldVarint 3 (fromIntegral (fdpFieldNumber f))) <>
    (if fdpFieldLabel f == 0 then mempty else encodeFieldVarint 4 (fromIntegral (fdpFieldLabel f))) <>
    (if fdpFieldType f == 0 then mempty else encodeFieldVarint 5 (fromIntegral (fdpFieldType f))) <>
    (if fdpFieldTypeName f == "" then mempty else encodeFieldString 6 (fdpFieldTypeName f)) <>
    (if fdpFieldDefault f == "" then mempty else encodeFieldString 7 (fdpFieldDefault f)) <>
    (if fdpFieldOneofIdx f < 0 then mempty else encodeFieldVarint 9 (fromIntegral (fdpFieldOneofIdx f))) <>
    (if fdpFieldJsonName f == "" then mempty else encodeFieldString 10 (fdpFieldJsonName f))

instance MessageDecode FieldDescriptorProto where
  messageDecoder = loop defaultFieldDescriptorProto
    where
      loop !f = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure f
          UJust (Tag 1 _)  -> do v <- decodeFieldString; loop f { fdpFieldName = v }
          UJust (Tag 3 _)  -> do v <- getVarint; loop f { fdpFieldNumber = fromIntegral v }
          UJust (Tag 4 _)  -> do v <- getVarint; loop f { fdpFieldLabel = fromIntegral v }
          UJust (Tag 5 _)  -> do v <- getVarint; loop f { fdpFieldType = fromIntegral v }
          UJust (Tag 6 _)  -> do v <- decodeFieldString; loop f { fdpFieldTypeName = v }
          UJust (Tag 7 _)  -> do v <- decodeFieldString; loop f { fdpFieldDefault = v }
          UJust (Tag 9 _)  -> do v <- getVarint; loop f { fdpFieldOneofIdx = fromIntegral v }
          UJust (Tag 10 _) -> do v <- decodeFieldString; loop f { fdpFieldJsonName = v }
          UJust (Tag _ wt) -> skipField wt >> loop f

data EnumDescriptorProto = EnumDescriptorProto
  { edpName  :: !Text
  , edpValue :: !(V.Vector EnumValueDescriptorProto)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultEnumDescriptorProto :: EnumDescriptorProto
defaultEnumDescriptorProto = EnumDescriptorProto "" V.empty

instance MessageEncode EnumDescriptorProto where
  buildMessage e =
    (if edpName e == "" then mempty else encodeFieldString 1 (edpName e)) <>
    V.foldl' (\a v -> a <> encodeFieldMessage 2 v) mempty (edpValue e)

instance MessageDecode EnumDescriptorProto where
  messageDecoder = loop defaultEnumDescriptorProto
    where
      loop !e = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure e
          UJust (Tag 1 _) -> do v <- decodeFieldString; loop e { edpName = v }
          UJust (Tag 2 _) -> do v <- decodeFieldMessage; loop e { edpValue = V.snoc (edpValue e) v }
          UJust (Tag _ wt) -> skipField wt >> loop e

data EnumValueDescriptorProto = EnumValueDescriptorProto
  { evdpName   :: !Text
  , evdpNumber :: !Int32
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultEnumValueDescriptorProto :: EnumValueDescriptorProto
defaultEnumValueDescriptorProto = EnumValueDescriptorProto "" 0

instance MessageEncode EnumValueDescriptorProto where
  buildMessage ev =
    (if evdpName ev == "" then mempty else encodeFieldString 1 (evdpName ev)) <>
    (if evdpNumber ev == 0 then mempty else encodeFieldVarint 2 (fromIntegral (evdpNumber ev)))

instance MessageDecode EnumValueDescriptorProto where
  messageDecoder = loop defaultEnumValueDescriptorProto
    where
      loop !ev = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure ev
          UJust (Tag 1 _) -> do v <- decodeFieldString; loop ev { evdpName = v }
          UJust (Tag 2 _) -> do v <- getVarint; loop ev { evdpNumber = fromIntegral v }
          UJust (Tag _ wt) -> skipField wt >> loop ev

data ServiceDescriptorProto = ServiceDescriptorProto
  { sdpName   :: !Text
  , sdpMethod :: !(V.Vector MethodDescriptorProto)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultServiceDescriptorProto :: ServiceDescriptorProto
defaultServiceDescriptorProto = ServiceDescriptorProto "" V.empty

instance MessageEncode ServiceDescriptorProto where
  buildMessage s =
    (if sdpName s == "" then mempty else encodeFieldString 1 (sdpName s)) <>
    V.foldl' (\a m -> a <> encodeFieldMessage 2 m) mempty (sdpMethod s)

instance MessageDecode ServiceDescriptorProto where
  messageDecoder = loop defaultServiceDescriptorProto
    where
      loop !s = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure s
          UJust (Tag 1 _) -> do v <- decodeFieldString; loop s { sdpName = v }
          UJust (Tag 2 _) -> do v <- decodeFieldMessage; loop s { sdpMethod = V.snoc (sdpMethod s) v }
          UJust (Tag _ wt) -> skipField wt >> loop s

data MethodDescriptorProto = MethodDescriptorProto
  { mdpName            :: !Text
  , mdpInputType       :: !Text
  , mdpOutputType      :: !Text
  , mdpClientStreaming  :: !Bool
  , mdpServerStreaming  :: !Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultMethodDescriptorProto :: MethodDescriptorProto
defaultMethodDescriptorProto = MethodDescriptorProto "" "" "" False False

instance MessageEncode MethodDescriptorProto where
  buildMessage m =
    (if mdpName m == "" then mempty else encodeFieldString 1 (mdpName m)) <>
    (if mdpInputType m == "" then mempty else encodeFieldString 2 (mdpInputType m)) <>
    (if mdpOutputType m == "" then mempty else encodeFieldString 3 (mdpOutputType m)) <>
    (if not (mdpClientStreaming m) then mempty else encodeFieldBool 5 True) <>
    (if not (mdpServerStreaming m) then mempty else encodeFieldBool 6 True)

instance MessageDecode MethodDescriptorProto where
  messageDecoder = loop defaultMethodDescriptorProto
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m
          UJust (Tag 1 _) -> do v <- decodeFieldString; loop m { mdpName = v }
          UJust (Tag 2 _) -> do v <- decodeFieldString; loop m { mdpInputType = v }
          UJust (Tag 3 _) -> do v <- decodeFieldString; loop m { mdpOutputType = v }
          UJust (Tag 5 _) -> do v <- decodeFieldBool; loop m { mdpClientStreaming = v }
          UJust (Tag 6 _) -> do v <- decodeFieldBool; loop m { mdpServerStreaming = v }
          UJust (Tag _ wt) -> skipField wt >> loop m

newtype OneofDescriptorProto = OneofDescriptorProto
  { odpName :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultOneofDescriptorProto :: OneofDescriptorProto
defaultOneofDescriptorProto = OneofDescriptorProto ""

instance MessageEncode OneofDescriptorProto where
  buildMessage o = if odpName o == "" then mempty else encodeFieldString 1 (odpName o)

instance MessageDecode OneofDescriptorProto where
  messageDecoder = loop defaultOneofDescriptorProto
    where
      loop !o = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure o
          UJust (Tag 1 _) -> do v <- decodeFieldString; loop o { odpName = v }
          UJust (Tag _ wt) -> skipField wt >> loop o
