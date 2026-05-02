{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Internal building blocks used by 'Proto.Derive' and intended for
-- reuse from other Template Haskell entry points (notably the IDL
-- bridge in 'Proto.TH').
--
-- The split exists so that callers who synthesise a fresh @data@
-- declaration in the same splice (and therefore cannot @qReify@ the
-- type) can still drive the same encoder \/ decoder \/ size body
-- generation logic by handing in a pre-computed @['ProtoField']@
-- list.
--
-- /Stability/: this module exposes the deriver's internals. Breaking
-- changes here only force a corresponding update in 'Proto.Derive'
-- and 'Proto.TH'; users who only call 'Proto.Derive.deriveProto'
-- will not be affected.
module Proto.Derive.Internal
  ( -- * Field model
    ProtoField (..)
  , protoField
  , ProtoFieldKind (..)
  , ProtoFieldType (..)
  , Scalar (..)
  , RepeatedRep (..)
  , OneofVariant (..)
  , scalarOfMapKey

    -- * Message-level metadata
  , MessageMeta (..)
  , defaultMessageMeta

    -- * Body builders (consume @['ProtoField']@)
  , buildMessageBody
  , buildMessageBodyWith
  , messageSizeBody
  , messageSizeBodyWith
  , messageDecoderBody
  , messageDecoderBodyWith

    -- * Instance synthesis (no reification required)
  , mkEncodeInstance
  , mkEncodeInstanceWith
  , mkSizeInstance
  , mkSizeInstanceWith
  , mkDecodeInstance
  , mkDecodeInstanceWith
  , mkIsMessageInstance

    -- * Convenience: synthesise the full instance group
  , synthesiseProtoInstances
  , synthesiseProtoInstancesWith

    -- * Wire-tag helpers
  , scalarWireType
  , wireVarint
  , wire64Bit
  , wireLengthDelimited
  , wire32Bit
  , tagByteFor
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.Int (Int32, Int64)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import Language.Haskell.TH

import qualified Proto.Decode as PD
import qualified Proto.Encode as PE
import qualified Proto.Encode.Archetype as PA
import qualified Proto.Message as PM
import Proto.Repr (BytesRep (..), StringRep (..))
import qualified Proto.Repr as PR
import Proto.Wire (Tag (..))
import qualified Proto.Wire.Encode as PWE
import Wireform.Derive.Modifier (MapKeyScalar (..))

-- ---------------------------------------------------------------------------
-- Field model (lifted out of Proto.Derive)
-- ---------------------------------------------------------------------------

-- | How the field is wrapped on the Haskell side.
data ProtoFieldKind
  = -- | Singular scalar / submessage / enum field. Encode with the
    -- proto3 default-skip rule (scalars only; submessages and enums
    -- always encode at least the tag, but a missing tag decodes to
    -- the zero value).
    FKBare
  | -- | @Maybe a@-wrapped singular field. Encode only when 'Just'.
    FKMaybe
  | -- | A repeated field carried by a 'V.Vector' or list. The
    -- 'RepeatedRep' picks the container shape; the element type
    -- lives in @pfInnerTy@ and the wire encoding in @pfType@.
    FKRepeated !RepeatedRep
  | -- | A proto3 @map<K, V>@ field carried by a strict
    -- 'Data.Map.Strict.Map'. The key's wire encoding is the
    -- supplied 'MapKeyScalar'; the value's wire encoding is in
    -- @pfType@ and the value's Haskell type is @pfInnerTy@.
    FKMap !MapKeyScalar
  | -- | A proto @oneof@ carried as a Haskell @Maybe SumType@. Each
    -- variant's tag, constructor, and payload type lives inline.
    -- @pfTag@ is unused for oneofs; @pfType@ / @pfInnerTy@ describe
    -- the carrier @Maybe SumType@ but are ignored by the body
    -- builders.
    FKOneof ![OneofVariant]
  deriving stock (Show)

-- | Container backing a repeated field. Wire format is identical
-- across all three; the choice only affects the Haskell-side
-- container type and the fold/snoc primitives we splice in.
data RepeatedRep
  = RepVector
  | RepList
  | RepSeq
  deriving stock (Eq, Show)

-- | One arm of a proto @oneof@.
data OneofVariant = OneofVariant
  { ovConstructor :: !Name
    -- ^ Sum-type constructor name.
  , ovTag         :: !Int
    -- ^ Proto field number for this arm.
  , ovInnerTy     :: !Type
    -- ^ The constructor's single argument type.
  , ovType        :: !ProtoFieldType
    -- ^ Wire encoding for the arm's payload.
  } deriving stock (Show)

-- | What lives inside the field on the wire.
data ProtoFieldType
  = -- | Recognised scalar.
    PFScalar !Scalar
  | -- | Submessage with existing 'PE.MessageEncode' \/
    -- 'PD.MessageDecode' \/ 'PE.MessageSize' instances.
    PFSubmessage
  | -- | A Haskell @Enum@ (any datatype with derived @Enum@).
    -- Encoded as a varint via 'fromEnum' / 'toEnum'.
    PFEnum
  deriving stock (Eq, Show)

-- | Proto wire scalars supported by the deriver.
data Scalar
  = SInt32  | SInt64
  | SUInt32 | SUInt64
  | SSInt32 | SSInt64
  | SFixed32 | SFixed64
  | SSFixed32 | SSFixed64
  | SBool | SFloat | SDouble
  | SString | SBytes
  deriving stock (Eq, Show)

-- | Project a 'MapKeyScalar' onto the deriver's 'Scalar' type so
-- the existing scalar-encoding machinery can be reused for map
-- keys.
scalarOfMapKey :: MapKeyScalar -> Scalar
scalarOfMapKey = \case
  MapKeyInt32    -> SInt32
  MapKeyInt64    -> SInt64
  MapKeyUInt32   -> SUInt32
  MapKeyUInt64   -> SUInt64
  MapKeySInt32   -> SSInt32
  MapKeySInt64   -> SSInt64
  MapKeyFixed32  -> SFixed32
  MapKeyFixed64  -> SFixed64
  MapKeySFixed32 -> SSFixed32
  MapKeySFixed64 -> SSFixed64
  MapKeyBool     -> SBool
  MapKeyString   -> SString

-- | A field after annotation resolution and type inspection. The
-- selector 'Name' is used for both the encoder getter and the
-- decoder's record assignment, so it must match the constructor that
-- will eventually be in scope.
data ProtoField = ProtoField
  { pfSelector :: !Name
  , pfTag      :: !Int
  , pfKind     :: !ProtoFieldKind
  , pfType     :: !ProtoFieldType
    -- ^ Wire encoding for the field's element/value (singular,
    -- repeated, map-value). Ignored for 'FKOneof' (each variant
    -- carries its own).
  , pfInnerTy  :: !Type
    -- ^ Type of the unwrapped value (no 'Maybe', no container).
    -- For maps, this is the value type. For oneofs this is the
    -- carrier @Maybe SumType@ — body builders ignore it.
  , pfStringRep :: !StringRep
    -- ^ Haskell representation for proto @string@ fields. Defaults
    -- to 'StrictTextRep'. Only consulted when @pfType = PFScalar
    -- SString@.
  , pfBytesRep  :: !BytesRep
    -- ^ Haskell representation for proto @bytes@ fields. Defaults
    -- to 'StrictBytesRep'. Only consulted when @pfType = PFScalar
    -- SBytes@.
  }

-- | Smart constructor with the rep choices defaulted to strict
-- 'Text' \/ 'ByteString'. Most call sites are happy with these.
protoField :: Name -> Int -> ProtoFieldKind -> ProtoFieldType -> Type -> ProtoField
protoField sel tg knd ty inner = ProtoField
  { pfSelector  = sel
  , pfTag       = tg
  , pfKind      = knd
  , pfType      = ty
  , pfInnerTy   = inner
  , pfStringRep = StrictTextRep
  , pfBytesRep  = StrictBytesRep
  }

-- ---------------------------------------------------------------------------
-- Message-level metadata
-- ---------------------------------------------------------------------------

-- | Per-message knobs that can\'t be expressed at the field level.
--
-- The current resident is 'mmUnknownFieldsSel': when set to
-- @Just sel@, the synthesised codecs honour an unknown-fields slot
-- on the record (the encoder appends captured tags after the
-- declared fields, the decoder routes any unrecognised tag into a
-- parallel accumulator, and the sizer adds the slot\'s on-wire
-- footprint). When 'Nothing', the codecs match the original
-- 'Proto.Derive.deriveProto' output and silently drop unknown
-- tags.
data MessageMeta = MessageMeta
  { mmUnknownFieldsSel :: !(Maybe Name)
    -- ^ Selector for an @[Decode.UnknownField]@ field on the
    -- record. When set, the codecs round-trip unknown tags through
    -- this slot.
  } deriving stock (Show)

defaultMessageMeta :: MessageMeta
defaultMessageMeta = MessageMeta { mmUnknownFieldsSel = Nothing }

-- ---------------------------------------------------------------------------
-- Wire-tag helpers
-- ---------------------------------------------------------------------------

wireVarint, wire64Bit, wireLengthDelimited, wire32Bit :: Int
wireVarint           = 0
wire64Bit            = 1
wireLengthDelimited  = 2
wire32Bit            = 5

-- | @(fn << 3) | wt@. Encoders below assume the result fits in one
-- byte; callers should check field-number bounds.
tagByteFor :: Int -> Int -> Int
tagByteFor fn wt = (fn * 8) + wt

scalarWireType :: Scalar -> Int
scalarWireType = \case
  SInt32     -> wireVarint
  SInt64     -> wireVarint
  SUInt32    -> wireVarint
  SUInt64    -> wireVarint
  SSInt32    -> wireVarint
  SSInt64    -> wireVarint
  SBool      -> wireVarint
  SFixed32   -> wire32Bit
  SSFixed32  -> wire32Bit
  SFloat     -> wire32Bit
  SFixed64   -> wire64Bit
  SSFixed64  -> wire64Bit
  SDouble    -> wire64Bit
  SString    -> wireLengthDelimited
  SBytes     -> wireLengthDelimited


-- ---------------------------------------------------------------------------
-- Body builders
-- ---------------------------------------------------------------------------

-- | RHS of @buildMessage@ for the supplied fields. Equivalent to
-- @'buildMessageBodyWith' 'defaultMessageMeta'@.
buildMessageBody :: [ProtoField] -> Q Exp
buildMessageBody = buildMessageBodyWith defaultMessageMeta

-- | RHS of @buildMessage@. When @mmUnknownFieldsSel = Just sel@,
-- appends @Decode.encodeUnknownFields (sel msg)@ after the
-- declared fields.
buildMessageBodyWith :: MessageMeta -> [ProtoField] -> Q Exp
buildMessageBodyWith meta fs = do
  msg   <- newName "msg"
  parts <- mapM (encodeOne msg) fs
  let ufPart = case mmUnknownFieldsSel meta of
        Nothing  -> Nothing
        Just sel -> Just (AppE (VarE 'PD.encodeUnknownFields)
                               (AppE (VarE sel) (VarE msg)))
      allParts = parts <> maybe [] (:[]) ufPart
  case allParts of
    [] -> lamE [varP msg] [| mempty |]
    _  ->
      let body = foldr1 (\a b -> InfixE (Just a) (VarE '(<>)) (Just b)) allParts
      in lamE [varP msg] (pure body)

-- | RHS of @messageSize@ for the supplied fields. Equivalent to
-- @'messageSizeBodyWith' 'defaultMessageMeta'@.
messageSizeBody :: [ProtoField] -> Q Exp
messageSizeBody = messageSizeBodyWith defaultMessageMeta

-- | RHS of @messageSize@. When @mmUnknownFieldsSel = Just sel@,
-- adds @Decode.unknownFieldsSize (sel msg)@.
messageSizeBodyWith :: MessageMeta -> [ProtoField] -> Q Exp
messageSizeBodyWith meta fs = do
  msg   <- newName "msg"
  parts <- mapM (sizeOne msg) fs
  let ufPart = case mmUnknownFieldsSel meta of
        Nothing  -> Nothing
        Just sel -> Just (AppE (VarE 'PD.unknownFieldsSize)
                               (AppE (VarE sel) (VarE msg)))
      allParts = parts <> maybe [] (:[]) ufPart
  case allParts of
    [] -> lamE [varP msg] [| 0 :: Int |]
    _  -> lamE [varP msg]
            (pure (foldr1 (\a b -> InfixE (Just a) (VarE '(+)) (Just b)) allParts))

-- | RHS of @messageDecoder@ for the supplied constructor and
-- fields. Equivalent to @'messageDecoderBodyWith'
-- 'defaultMessageMeta'@.
messageDecoderBody :: Name -> [ProtoField] -> Q Exp
messageDecoderBody = messageDecoderBodyWith defaultMessageMeta

-- | RHS of @messageDecoder@. The 'Name' is the record constructor
-- (typically the same as the type name for single-constructor
-- records). When @mmUnknownFieldsSel = Just sel@, captures
-- unrecognised tags into a parallel accumulator and writes the
-- reversed list to the slot in the final record.
messageDecoderBodyWith :: MessageMeta -> Name -> [ProtoField] -> Q Exp
messageDecoderBodyWith meta conName fs = do
  accNames <- mapM (\(i, _) -> newName ("acc_" ++ show (i :: Int))) (zip [0..] fs)
  let pairs = zip fs accNames
  initEs   <- mapM initFor pairs
  loopName <- newName "loop"
  ufAccM   <- case mmUnknownFieldsSel meta of
                Nothing -> pure Nothing
                Just _  -> Just <$> newName "acc_unknown_"
  loopBody <- decodeLoopBody meta conName loopName pairs ufAccM
  let allParams = map VarP accNames <> maybe [] (\n -> [VarP n]) ufAccM
      loopFun =
        FunD loopName
          [Clause allParams (NormalB loopBody) []]
      ufInit  = case mmUnknownFieldsSel meta of
                  Nothing -> []
                  Just _  -> [ListE []]
      bodyExp =
        LetE [loopFun]
          (foldl AppE (VarE loopName) (initEs <> ufInit))
  pure bodyExp

-- ---------------------------------------------------------------------------
-- Instance synthesis (no reification required)
-- ---------------------------------------------------------------------------

-- | Generate an 'PE.MessageEncode' instance for @ty@ with the
-- supplied field list. The @ty@ must be a fully applied type whose
-- record selectors match @pfSelector@ values.
mkEncodeInstance :: Type -> [ProtoField] -> Q Dec
mkEncodeInstance = mkEncodeInstanceWith defaultMessageMeta

mkEncodeInstanceWith :: MessageMeta -> Type -> [ProtoField] -> Q Dec
mkEncodeInstanceWith meta ty fs = do
  body <- buildMessageBodyWith meta fs
  pure $ InstanceD Nothing []
           (AppT (ConT ''PE.MessageEncode) ty)
           [FunD 'PE.buildMessage [Clause [] (NormalB body) []]]

-- | Generate an 'PE.MessageSize' instance.
mkSizeInstance :: Type -> [ProtoField] -> Q Dec
mkSizeInstance = mkSizeInstanceWith defaultMessageMeta

mkSizeInstanceWith :: MessageMeta -> Type -> [ProtoField] -> Q Dec
mkSizeInstanceWith meta ty fs = do
  body <- messageSizeBodyWith meta fs
  pure $ InstanceD Nothing []
           (AppT (ConT ''PE.MessageSize) ty)
           [FunD 'PE.messageSize [Clause [] (NormalB body) []]]

-- | Generate a 'PD.MessageDecode' instance using @conName@ as the
-- record constructor.
mkDecodeInstance :: Type -> Name -> [ProtoField] -> Q Dec
mkDecodeInstance = mkDecodeInstanceWith defaultMessageMeta

mkDecodeInstanceWith :: MessageMeta -> Type -> Name -> [ProtoField] -> Q Dec
mkDecodeInstanceWith meta ty conName fs = do
  body <- messageDecoderBodyWith meta conName fs
  -- AGENTS.md ("Performance" → "Decoder monad style") requires
  -- @{-# INLINE messageDecoder #-}@ on every instance: the
  -- continuation-passing dispatch in 'getTagOrU' is only worth its
  -- weight when GHC can see the lambda for each field. Mirrors what
  -- 'Proto.CodeGen' emits in pure-text codegen.
  pure $ InstanceD Nothing []
           (AppT (ConT ''PD.MessageDecode) ty)
           [ PragmaD (InlineP 'PD.messageDecoder Inline FunLike AllPhases)
           , FunD 'PD.messageDecoder [Clause [] (NormalB body) []]
           ]

-- | Generate a 'PM.IsMessage' instance whose 'PM.messageTypeName'
-- returns the supplied name as a 'Text' literal.
mkIsMessageInstance :: Type -> Text -> Q Dec
mkIsMessageInstance ty nameStr = do
  body <- [| const (T.pack $(litE (stringL (T.unpack nameStr)))) |]
  pure $ InstanceD Nothing []
           (AppT (ConT ''PM.IsMessage) ty)
           [FunD 'PM.messageTypeName [Clause [] (NormalB body) []]]

-- | One-shot: emit all four instances ('PE.MessageEncode',
-- 'PE.MessageSize', 'PD.MessageDecode', 'PM.IsMessage') for a
-- pre-translated message.
--
-- Suitable for use from 'Proto.TH.loadProto'-style splices that
-- emit a fresh @data@ declaration alongside the instances and so
-- cannot rely on 'Language.Haskell.TH.reify'.
synthesiseProtoInstances
  :: Type        -- ^ Fully applied type, e.g. @ConT ''Person@.
  -> Name        -- ^ Record constructor name (often equal to type name).
  -> Text        -- ^ Logical proto message name (used by 'PM.messageTypeName').
  -> [ProtoField]
  -> Q [Dec]
synthesiseProtoInstances = synthesiseProtoInstancesWith defaultMessageMeta

synthesiseProtoInstancesWith
  :: MessageMeta
  -> Type
  -> Name
  -> Text
  -> [ProtoField]
  -> Q [Dec]
synthesiseProtoInstancesWith meta ty conName protoName fs = do
  enc <- mkEncodeInstanceWith meta ty fs
  siz <- mkSizeInstanceWith   meta ty fs
  dec <- mkDecodeInstanceWith meta ty conName fs
  ism <- mkIsMessageInstance ty protoName
  pure [enc, siz, dec, ism]

-- ---------------------------------------------------------------------------
-- Encoder / size internals
-- ---------------------------------------------------------------------------

encodeOne :: Name -> ProtoField -> Q Exp
encodeOne msg pf = do
  let getter = AppE (VarE (pfSelector pf)) (VarE msg)
      tagInt = pfTagByte pf
  case pfKind pf of
    FKBare -> do
      v <- newName "v"
      letE [valD (varP v) (normalB (pure getter)) []]
        (condDefaultSkipE pf v (encodeSingleE pf tagInt v))
    FKMaybe -> do
      v <- newName "v"
      caseE (pure getter)
        [ match (conP 'Nothing []) (normalB [| mempty |]) []
        , match (conP 'Just [varP v])
            (normalB (encodeSingleE pf tagInt v)) []
        ]
    FKRepeated rep -> do
      v <- newName "v"
      acc <- newName "acc"
      perElement <- encodeSingleE pf tagInt v
      let foldFnE = repeatedFoldlE rep
          step = LamE [VarP acc, VarP v]
                    (InfixE (Just (VarE acc)) (VarE '(<>)) (Just perElement))
      pure (AppE (AppE (AppE foldFnE step) (VarE 'mempty)) getter)
    FKMap mks -> do
      kV <- newName "k"
      vV <- newName "v"
      acc <- newName "acc"
      keyEnc   <- encodeMapKeyE mks kV
      valEnc   <- encodeMapValueE pf vV
      let entry = AppE (AppE (AppE (VarE 'PE.encodeMapField)
                                    (LitE (IntegerL (fromIntegral (pfTag pf)))))
                              keyEnc)
                       valEnc
          step = LamE [VarP acc, VarP kV, VarP vV]
                   (InfixE (Just (VarE acc)) (VarE '(<>)) (Just entry))
      pure (AppE (AppE (AppE (VarE 'Map.foldlWithKey') step) (VarE 'mempty)) getter)
    FKOneof variants -> do
      inner <- newName "ov"
      arms <- traverse (oneofEncodeArm inner) variants
      let allArms = match (conP 'Nothing []) (normalB [| mempty |]) []
                  : zipWith (\v body ->
                      match (conP 'Just [conP (ovConstructor v) [varP inner]])
                            (normalB (pure body)) [])
                      variants arms
      caseE (pure getter) allArms

sizeOne :: Name -> ProtoField -> Q Exp
sizeOne msg pf = do
  let getter = AppE (VarE (pfSelector pf)) (VarE msg)
  case pfKind pf of
    FKBare -> do
      v <- newName "v"
      letE [valD (varP v) (normalB (pure getter)) []]
        (condDefaultSkipS pf v (sizeSingleE pf v))
    FKMaybe -> do
      v <- newName "v"
      caseE (pure getter)
        [ match (conP 'Nothing []) (normalB [| 0 :: Int |]) []
        , match (conP 'Just [varP v])
            (normalB (sizeSingleE pf v)) []
        ]
    FKRepeated rep -> do
      v   <- newName "v"
      acc <- newName "acc"
      per <- sizeSingleE pf v
      tagSz <- [| PWE.tagSize $(litE (integerL (fromIntegral (pfTag pf)))) |]
      let foldFnE = repeatedFoldlE rep
          step    = LamE [VarP acc, VarP v]
                       (InfixE (Just (VarE acc)) (VarE '(+))
                         (Just (InfixE (Just tagSz) (VarE '(+)) (Just per))))
      pure (AppE (AppE (AppE foldFnE step) (LitE (IntegerL 0))) getter)
    FKMap _mks ->
      -- Map size estimation matches 'Proto.TH': 10 bytes per entry
      -- as a coarse upper bound. Two-pass encoders need the size to
      -- be ≥ the actual byte count, and 10 covers the common
      -- (small key + small value) case for proto3 maps. Refining
      -- this to the exact entry size is a follow-up.
      [| if Map.null $(pure getter) then 0 else Map.size $(pure getter) * 10 |]
    FKOneof variants -> do
      inner <- newName "ov"
      arms <- traverse (oneofSizeArm inner) variants
      let allArms = match (conP 'Nothing []) (normalB [| 0 :: Int |]) []
                  : zipWith (\v body ->
                      match (conP 'Just [conP (ovConstructor v) [varP inner]])
                            (normalB (pure body)) [])
                      variants arms
      caseE (pure getter) allArms

oneofEncodeArm :: Name -> OneofVariant -> Q Exp
oneofEncodeArm v ov =
  let pseudo = protoField (ovConstructor ov) (ovTag ov)
                          FKBare (ovType ov) (ovInnerTy ov)
      tagInt = case ovType ov of
        PFScalar s   -> tagByteFor (ovTag ov) (scalarWireType s)
        PFSubmessage -> tagByteFor (ovTag ov) wireLengthDelimited
        PFEnum       -> tagByteFor (ovTag ov) wireVarint
  in encodeSingleE pseudo tagInt v

oneofSizeArm :: Name -> OneofVariant -> Q Exp
oneofSizeArm v ov =
  let pseudo = protoField (ovConstructor ov) (ovTag ov)
                          FKBare (ovType ov) (ovInnerTy ov)
  in [| PWE.tagSize $(litE (integerL (fromIntegral (ovTag ov))))
        + $(sizeSingleE pseudo v) |]

pfTagByte :: ProtoField -> Int
pfTagByte pf = case pfKind pf of
  FKMap _      -> tagByteFor (pfTag pf) wireLengthDelimited
  FKOneof _    -> 0
  _ -> case pfType pf of
    PFScalar s   -> tagByteFor (pfTag pf) (scalarWireType s)
    PFSubmessage -> tagByteFor (pfTag pf) wireLengthDelimited
    PFEnum       -> tagByteFor (pfTag pf) wireVarint

-- | Build the encoder for one map key. Keys are always singular
-- scalars at field number 1 inside the entry message.
encodeMapKeyE :: MapKeyScalar -> Name -> Q Exp
encodeMapKeyE mks kVar =
  let pseudo = protoField kVar 1 FKBare
                          (PFScalar (scalarOfMapKey mks)) (ConT ''Int)
      tagInt = tagByteFor 1 (scalarWireType (scalarOfMapKey mks))
  in encodeSingleE pseudo tagInt kVar

-- | Build the encoder for one map value at field number 2 inside
-- the entry message.
encodeMapValueE :: ProtoField -> Name -> Q Exp
encodeMapValueE pf vVar =
  let valueField = pf { pfTag = 2 }  -- inside the entry
      tagInt = case pfType pf of
        PFScalar s   -> tagByteFor 2 (scalarWireType s)
        PFSubmessage -> tagByteFor 2 wireLengthDelimited
        PFEnum       -> tagByteFor 2 wireVarint
  in encodeSingleE valueField tagInt vVar

condDefaultSkipE :: ProtoField -> Name -> Q Exp -> Q Exp
condDefaultSkipE pf v body =
  case pfType pf of
    PFSubmessage -> body
    PFScalar sc  ->
      [| if $(defaultPredFieldE pf sc v) then mempty else $body |]
    PFEnum       ->
      [| if fromEnum $(varE v) == 0 then mempty else $body |]

condDefaultSkipS :: ProtoField -> Name -> Q Exp -> Q Exp
condDefaultSkipS pf v body =
  case pfType pf of
    PFSubmessage -> body
    PFScalar sc  ->
      [| if $(defaultPredFieldE pf sc v) then (0 :: Int) else $body |]
    PFEnum       ->
      [| if fromEnum $(varE v) == 0 then (0 :: Int) else $body |]

-- | Default predicate, dispatched by string / bytes representation
-- when applicable. For non-string/non-bytes scalars the
-- representation field is ignored.
defaultPredFieldE :: ProtoField -> Scalar -> Name -> Q Exp
defaultPredFieldE pf sc v = case sc of
  SString -> stringNullE (pfStringRep pf) v
  SBytes  -> bytesNullE (pfBytesRep pf) v
  _       -> defaultPredE sc v

defaultPredE :: Scalar -> Name -> Q Exp
defaultPredE sc v = case sc of
  SBool   -> [| not $(varE v) |]
  SString -> [| T.null $(varE v) |]
  SBytes  -> [| BS.null $(varE v) |]
  SFloat  -> [| ($(varE v) :: Float)  == 0 |]
  SDouble -> [| ($(varE v) :: Double) == 0 |]
  _       -> [| $(varE v) == 0 |]

-- | Emptiness predicate per 'StringRep'.
stringNullE :: StringRep -> Name -> Q Exp
stringNullE rep v = case rep of
  StrictTextRep -> [| T.null $(varE v) |]
  LazyTextRep   -> [| TL.null $(varE v) |]
  ShortTextRep  -> [| SBS.null $(varE v) |]
  HsStringRep   -> [| null $(varE v) |]

-- | Emptiness predicate per 'BytesRep'.
bytesNullE :: BytesRep -> Name -> Q Exp
bytesNullE rep v = case rep of
  StrictBytesRep -> [| BS.null $(varE v) |]
  LazyBytesRep   -> [| BL.null $(varE v) |]
  ShortBytesRep  -> [| SBS.null $(varE v) |]

encodeSingleE :: ProtoField -> Int -> Name -> Q Exp
encodeSingleE pf tagInt v =
  let tagWord = litE (integerL (fromIntegral tagInt))
      var     = varE v
  in case pfType pf of
    PFScalar SInt32    -> [| PA.archVarint     $tagWord (fromIntegral ($var :: Int32)) |]
    PFScalar SInt64    -> [| PA.archVarint     $tagWord (fromIntegral ($var :: Int64)) |]
    PFScalar SUInt32   -> [| PA.archVarint     $tagWord (fromIntegral ($var :: Word32)) |]
    PFScalar SUInt64   -> [| PA.archVarint     $tagWord ($var :: Word64) |]
    PFScalar SSInt32   -> [| PA.archSVarint32  $tagWord ($var :: Int32) |]
    PFScalar SSInt64   -> [| PA.archSVarint64  $tagWord ($var :: Int64) |]
    PFScalar SFixed32  -> [| PA.archFixed32    $tagWord ($var :: Word32) |]
    PFScalar SFixed64  -> [| PA.archFixed64    $tagWord ($var :: Word64) |]
    PFScalar SSFixed32 -> [| PA.archFixed32    $tagWord (fromIntegral ($var :: Int32)) |]
    PFScalar SSFixed64 -> [| PA.archFixed64    $tagWord (fromIntegral ($var :: Int64)) |]
    PFScalar SBool     -> [| PA.archBool       $tagWord ($var :: Bool) |]
    PFScalar SFloat    -> [| PA.archFloat      $tagWord ($var :: Float) |]
    PFScalar SDouble   -> [| PA.archDouble     $tagWord ($var :: Double) |]
    PFScalar SString   -> stringEncodeE (pfStringRep pf) tagInt v
    PFScalar SBytes    -> bytesEncodeE  (pfBytesRep pf) tagInt v
    PFSubmessage       ->
      [| let !sz = PE.messageSize $var
         in PA.archSubmessage $tagWord sz (PE.buildMessage $var) |]
    PFEnum             ->
      -- 'encodeFieldEnum' takes the proto field number (it computes
      -- its own varint tag); we recover @fieldNumber = tagByte / 8@.
      [| PE.encodeFieldEnum
           $(litE (integerL (fromIntegral (tagInt `quot` 8))))
           $var |]

-- | Per-'StringRep' encoder. Tag byte is supplied as an 'Int' for
-- consistency with the strict-text path's @PA.archString@ shape;
-- the alternate reps reach for the @Proto.Repr.encode*@ helpers
-- which take a field number rather than a tag byte.
stringEncodeE :: StringRep -> Int -> Name -> Q Exp
stringEncodeE rep tagInt v =
  let tagWord = litE (integerL (fromIntegral tagInt))
      fieldN  = litE (integerL (fromIntegral (tagInt `quot` 8)))
      var     = varE v
  in case rep of
       StrictTextRep -> [| PA.archString $tagWord ($var :: Text) |]
       LazyTextRep   -> [| PR.encodeLazyText $fieldN ($var :: TL.Text) |]
       ShortTextRep  -> [| PR.encodeShortByteString $fieldN ($var :: SBS.ShortByteString) |]
       HsStringRep   -> [| PR.encodeHsString $fieldN ($var :: String) |]

-- | Per-'BytesRep' encoder.
bytesEncodeE :: BytesRep -> Int -> Name -> Q Exp
bytesEncodeE rep tagInt v =
  let tagWord = litE (integerL (fromIntegral tagInt))
      fieldN  = litE (integerL (fromIntegral (tagInt `quot` 8)))
      var     = varE v
  in case rep of
       StrictBytesRep -> [| PA.archBytes $tagWord ($var :: ByteString) |]
       LazyBytesRep   -> [| PR.encodeLazyBytes $fieldN ($var :: BL.ByteString) |]
       ShortBytesRep  -> [| PR.encodeShortBytes $fieldN ($var :: SBS.ShortByteString) |]

sizeSingleE :: ProtoField -> Name -> Q Exp
sizeSingleE pf v =
  let var = varE v
  in case pfType pf of
    PFScalar SInt32    -> [| PA.archVarintSize (fromIntegral ($var :: Int32)) |]
    PFScalar SInt64    -> [| PA.archVarintSize (fromIntegral ($var :: Int64)) |]
    PFScalar SUInt32   -> [| PA.archVarintSize (fromIntegral ($var :: Word32)) |]
    PFScalar SUInt64   -> [| PA.archVarintSize ($var :: Word64) |]
    PFScalar SSInt32   -> [| 1 + PWE.varintSize (fromIntegral (PWE.zigZag32 ($var :: Int32))) |]
    PFScalar SSInt64   -> [| 1 + PWE.varintSize (PWE.zigZag64 ($var :: Int64)) |]
    PFScalar SFixed32  -> [| PA.archFixed32Size |]
    PFScalar SFixed64  -> [| PA.archFixed64Size |]
    PFScalar SSFixed32 -> [| PA.archFixed32Size |]
    PFScalar SSFixed64 -> [| PA.archFixed64Size |]
    PFScalar SBool     -> [| PA.archBoolSize |]
    PFScalar SFloat    -> [| PA.archFixed32Size |]
    PFScalar SDouble   -> [| PA.archFixed64Size |]
    PFScalar SString   -> stringSizeE (pfStringRep pf) v
    PFScalar SBytes    -> bytesSizeE  (pfBytesRep pf)  v
    PFSubmessage       -> [| PA.archSubmessageSize (PE.messageSize $var) |]
    PFEnum             ->
      [| PA.archVarintSize (fromIntegral (fromEnum $var) :: Word64) |]

-- ---------------------------------------------------------------------------
-- Decoder internals
-- ---------------------------------------------------------------------------

initFor :: (ProtoField, Name) -> Q Exp
initFor (pf, _) = case (pfKind pf, pfType pf) of
  (FKMaybe, _)                 -> [| Nothing |]
  (FKRepeated rep, _)          -> repeatedEmptyE rep
  (FKMap _, _)                 -> [| Map.empty |]
  (FKOneof _, _)               -> [| Nothing |]
  (FKBare, PFScalar SBool)     -> [| False |]
  (FKBare, PFScalar SString)   -> stringEmptyE (pfStringRep pf)
  (FKBare, PFScalar SBytes)    -> bytesEmptyE (pfBytesRep pf)
  (FKBare, PFScalar SFloat)    -> [| 0 :: Float |]
  (FKBare, PFScalar SDouble)   -> [| 0 :: Double |]
  (FKBare, PFScalar SInt32)    -> [| 0 :: Int32 |]
  (FKBare, PFScalar SInt64)    -> [| 0 :: Int64 |]
  (FKBare, PFScalar SUInt32)   -> [| 0 :: Word32 |]
  (FKBare, PFScalar SUInt64)   -> [| 0 :: Word64 |]
  (FKBare, PFScalar SSInt32)   -> [| 0 :: Int32 |]
  (FKBare, PFScalar SSInt64)   -> [| 0 :: Int64 |]
  (FKBare, PFScalar SFixed32)  -> [| 0 :: Word32 |]
  (FKBare, PFScalar SFixed64)  -> [| 0 :: Word64 |]
  (FKBare, PFScalar SSFixed32) -> [| 0 :: Int32 |]
  (FKBare, PFScalar SSFixed64) -> [| 0 :: Int64 |]
  (FKBare, PFEnum)             -> [| toEnum 0 |]
  (FKBare, PFSubmessage)       ->
    [| error "Proto.Derive: bare submessage field has no zero value; wrap in Maybe" |]

-- | Per-'StringRep' empty value.
stringEmptyE :: StringRep -> Q Exp
stringEmptyE = \case
  StrictTextRep -> [| T.empty |]
  LazyTextRep   -> [| TL.empty |]
  ShortTextRep  -> [| SBS.empty |]
  HsStringRep   -> [| "" :: String |]

-- | Per-'BytesRep' empty value.
bytesEmptyE :: BytesRep -> Q Exp
bytesEmptyE = \case
  StrictBytesRep -> [| BS.empty |]
  LazyBytesRep   -> [| BL.empty |]
  ShortBytesRep  -> [| SBS.empty |]

-- | Per-'StringRep' size primitive.
stringSizeE :: StringRep -> Name -> Q Exp
stringSizeE rep v =
  let var = varE v
  in case rep of
       StrictTextRep -> [| PA.archStringSize ($var :: Text) |]
       LazyTextRep   -> [| 1 + (let !bs = TE.encodeUtf8 (TL.toStrict ($var :: TL.Text))
                                in PWE.varintSize (fromIntegral (BS.length bs))
                                   + BS.length bs) |]
       ShortTextRep  -> [| 1 + (let !len = SBS.length ($var :: SBS.ShortByteString)
                                in PWE.varintSize (fromIntegral len) + len) |]
       HsStringRep   -> [| PA.archStringSize (T.pack ($var :: String)) |]

-- | Per-'BytesRep' size primitive.
bytesSizeE :: BytesRep -> Name -> Q Exp
bytesSizeE rep v =
  let var = varE v
  in case rep of
       StrictBytesRep -> [| PA.archBytesSize ($var :: ByteString) |]
       LazyBytesRep   -> [| 1 + (let !bs = BL.toStrict ($var :: BL.ByteString)
                                  in PWE.varintSize (fromIntegral (BS.length bs))
                                     + BS.length bs) |]
       ShortBytesRep  -> [| 1 + (let !len = SBS.length ($var :: SBS.ShortByteString)
                                  in PWE.varintSize (fromIntegral len) + len) |]

-- | Foldl over a repeated field's container, used by the encoder
-- and sizer.
repeatedFoldlE :: RepeatedRep -> Exp
repeatedFoldlE = \case
  RepVector -> VarE 'V.foldl'
  RepList   -> VarE 'List.foldl'
  RepSeq    -> VarE 'List.foldl'

-- | Empty value for a repeated container, used by the decoder
-- accumulator initialiser.
repeatedEmptyE :: RepeatedRep -> Q Exp
repeatedEmptyE = \case
  RepVector -> [| V.empty |]
  RepList   -> [| [] |]
  RepSeq    -> [| Seq.empty |]

-- | @container `snoc` v@ in 'Q'-friendly form. Used by the decode
-- arm for repeated fields.
repeatedSnocE :: RepeatedRep -> Exp -> Name -> Exp
repeatedSnocE rep accE vName = case rep of
  RepVector -> AppE (AppE (VarE 'V.snoc) accE) (VarE vName)
  RepList   -> InfixE (Just accE) (VarE '(<>))
                 (Just (ListE [VarE vName]))
  RepSeq    -> InfixE (Just accE) (VarE '(Seq.|>))
                 (Just (VarE vName))

decodeLoopBody
  :: MessageMeta
  -> Name           -- ^ Record constructor.
  -> Name           -- ^ Loop function name.
  -> [(ProtoField, Name)]
  -> Maybe Name     -- ^ Unknown-fields accumulator name (when
                    --   'mmUnknownFieldsSel' is set).
  -> Q Exp
decodeLoopBody meta conName loopName pairs ufAccM = do
  fnVar    <- newName "fn"
  wtVar    <- newName "wt"
  mTagName <- newName "mTag"
  let declaredAssigns = map (\(pf, acc) -> (pfSelector pf, VarE acc)) pairs
      ufAssigns = case (mmUnknownFieldsSel meta, ufAccM) of
        (Just sel, Just ufAcc) ->
          [(sel, AppE (VarE 'reverse) (VarE ufAcc))]
        _ -> []
      recE = RecConE conName (declaredAssigns <> ufAssigns)

  fieldMatches <- concat <$> mapM (decodeArm meta loopName wtVar pairs ufAccM) pairs
  let allAccs = map snd pairs
  defaultBody <- case (mmUnknownFieldsSel meta, ufAccM) of
    (Just _, Just ufAcc) -> do
      ufVar <- newName "uf"
      [| do
           $(varP ufVar) <- PD.captureUnknownField $(varE fnVar) $(varE wtVar)
           $(recurseLoopWithUFE loopName allAccs ufAcc (VarE ufVar)) |]
    _ ->
      [| do
           _ <- PD.skipField $(varE wtVar)
           $(recurseLoopE loopName allAccs ufAccM) |]
  let defaultMatch = Match WildP (NormalB defaultBody) []
      dispatchE    = CaseE (VarE fnVar) (fieldMatches ++ [defaultMatch])

      eofMatch     = Match (ConP 'PD.UNothing [] [])
                       (NormalB (AppE (VarE 'pure) recE))
                       []
      tagPat       = ConP 'PD.UJust [] [ConP 'Tag [] [VarP fnVar, VarP wtVar]]
      tagMatch     = Match tagPat (NormalB dispatchE) []

  pure $ DoE Nothing
    [ BindS (VarP mTagName) (VarE 'PD.getTagOrU)
    , NoBindS (CaseE (VarE mTagName) [eofMatch, tagMatch])
    ]

-- | Build the dispatch arms for one logical field. Most kinds emit
-- a single arm; oneofs emit one per variant; repeated/map share
-- the same arm for every occurrence of the same tag.
decodeArm
  :: MessageMeta
  -> Name              -- ^ recursive loop name
  -> Name              -- ^ wire-type variable (for packed support)
  -> [(ProtoField, Name)]  -- ^ all (field, accumulator) pairs
  -> Maybe Name        -- ^ unknown-fields accumulator (when in scope)
  -> (ProtoField, Name)    -- ^ the one we're emitting arms for
  -> Q [Match]
decodeArm meta loopName _wt allPairs ufAccM (pf, accForThis) = case pfKind pf of
  FKBare -> singletonArm pf $ \vName -> do
    decoderE <- fieldDecoderE pf vName
    let recurse = updateAccsE loopName allPairs ufAccM (pfSelector pf) (VarE vName)
    [| do
         $(varP vName) <- $(pure decoderE)
         $(pure recurse) |]

  FKMaybe -> singletonArm pf $ \vName -> do
    decoderE <- fieldDecoderE pf vName
    let recurse = updateAccsE loopName allPairs ufAccM (pfSelector pf)
                    (AppE (ConE 'Just) (VarE vName))
    [| do
         $(varP vName) <- $(pure decoderE)
         $(pure recurse) |]

  FKRepeated rep -> singletonArm pf $ \vName -> do
    payload <- scalarDecoderE pf
    let snocE   = repeatedSnocE rep (VarE accForThis) vName
        recurse = updateAccsE loopName allPairs ufAccM (pfSelector pf) snocE
    [| do
         $(varP vName) <- $(pure payload)
         $(pure recurse) |]

  FKMap mks -> do
    let lit = LitP (IntegerL (fromIntegral (pfTag pf)))
    bsVar <- newName "bs"
    kVar  <- newName "k"
    vVar  <- newName "v"
    keyDec  <- mapKeyDecoderE mks
    valDec  <- scalarDecoderE pf
    keyZero <- mapKeyZeroE mks
    valZero <- mapValueZeroE pf
    let insertE = AppE (AppE (AppE (VarE 'Map.insert) (VarE kVar))
                             (VarE vVar))
                       (VarE accForThis)
        recurse = updateAccsE loopName allPairs ufAccM (pfSelector pf) insertE
        skipRecurse = updateAccsE loopName allPairs ufAccM (pfSelector pf)
                        (VarE accForThis)
    body <-
      [| do
           $(varP bsVar) <- PD.getLengthDelimited
           case PD.runDecoder
                  (PD.decodeMapEntry $(pure keyDec) $(pure valDec)
                                     $(pure keyZero) $(pure valZero))
                  $(varE bsVar) of
             Left _ -> $(pure skipRecurse)
             Right ($(varP kVar), $(varP vVar)) -> $(pure recurse) |]
    pure [Match lit (NormalB body) []]

  FKOneof variants ->
    traverse (oneofDecodeArm loopName allPairs ufAccM (pfSelector pf)) variants
  where
    singletonArm thisPf mkBody = do
      vName <- newName "v"
      body  <- mkBody vName
      pure [Match (LitP (IntegerL (fromIntegral (pfTag thisPf))))
                  (NormalB body) []]
    _unused_meta = meta  -- reserved for future per-arm decisions

oneofDecodeArm
  :: Name
  -> [(ProtoField, Name)]
  -> Maybe Name       -- ^ unknown-fields accumulator (when in scope)
  -> Name             -- ^ parent field's selector
  -> OneofVariant
  -> Q Match
oneofDecodeArm loopName allPairs ufAccM sel ov = do
  vName <- newName "v"
  payload <- variantDecoderE ov
  let conApp   = AppE (ConE (ovConstructor ov)) (VarE vName)
      newVal   = AppE (ConE 'Just) conApp
      recurse  = updateAccsE loopName allPairs ufAccM sel newVal
  body <-
    [| do
         $(varP vName) <- $(pure payload)
         $(pure recurse) |]
  pure (Match (LitP (IntegerL (fromIntegral (ovTag ov)))) (NormalB body) [])

-- | Build the recursive @loop a1 a2 ... aN [acc_unknown_]@
-- application, with the named selector's accumulator replaced by
-- the given expression. The unknown-fields accumulator is passed
-- through unchanged.
updateAccsE :: Name -> [(ProtoField, Name)] -> Maybe Name -> Name -> Exp -> Exp
updateAccsE loopName allPairs ufAccM sel newE =
  let args   = map pickArg allPairs
      pickArg (pf, acc)
        | pfSelector pf == sel = newE
        | otherwise            = VarE acc
      ufArgs = case ufAccM of
        Just ufAcc -> [VarE ufAcc]
        Nothing    -> []
  in foldl AppE (VarE loopName) (args <> ufArgs)

fieldDecoderE :: ProtoField -> Name -> Q Exp
fieldDecoderE pf _v = scalarDecoderE pf

variantDecoderE :: OneofVariant -> Q Exp
variantDecoderE ov = scalarDecoderE
  (protoField (ovConstructor ov) (ovTag ov) FKBare (ovType ov) (ovInnerTy ov))

scalarDecoderE :: ProtoField -> Q Exp
scalarDecoderE pf = case pfType pf of
  PFScalar SInt32    -> [| (fromIntegral :: Word64 -> Int32)  <$> PD.decodeFieldVarint |]
  PFScalar SInt64    -> [| (fromIntegral :: Word64 -> Int64)  <$> PD.decodeFieldVarint |]
  PFScalar SUInt32   -> [| (fromIntegral :: Word64 -> Word32) <$> PD.decodeFieldVarint |]
  PFScalar SUInt64   -> [| PD.decodeFieldVarint |]
  PFScalar SSInt32   -> [| PD.decodeFieldSVarint32 |]
  PFScalar SSInt64   -> [| PD.decodeFieldSVarint64 |]
  PFScalar SFixed32  -> [| PD.decodeFieldFixed32 |]
  PFScalar SFixed64  -> [| PD.decodeFieldFixed64 |]
  PFScalar SSFixed32 -> [| (fromIntegral :: Word32 -> Int32) <$> PD.decodeFieldFixed32 |]
  PFScalar SSFixed64 -> [| (fromIntegral :: Word64 -> Int64) <$> PD.decodeFieldFixed64 |]
  PFScalar SBool     -> [| PD.decodeFieldBool |]
  PFScalar SFloat    -> [| PD.decodeFieldFloat |]
  PFScalar SDouble   -> [| PD.decodeFieldDouble |]
  PFScalar SString   -> stringDecoderE (pfStringRep pf)
  PFScalar SBytes    -> bytesDecoderE  (pfBytesRep pf)
  PFSubmessage       -> [| PD.decodeFieldMessage |]
  PFEnum             -> [| PD.decodeFieldEnum |]

stringDecoderE :: StringRep -> Q Exp
stringDecoderE = \case
  StrictTextRep -> [| PD.decodeFieldString |]
  LazyTextRep   -> [| TL.fromStrict <$> PD.decodeFieldString |]
  ShortTextRep  -> [| (SBS.toShort . TE.encodeUtf8) <$> PD.decodeFieldString |]
  HsStringRep   -> [| T.unpack <$> PD.decodeFieldString |]

bytesDecoderE :: BytesRep -> Q Exp
bytesDecoderE = \case
  StrictBytesRep -> [| PD.decodeFieldBytes |]
  LazyBytesRep   -> [| BL.fromStrict <$> PD.decodeFieldBytes |]
  ShortBytesRep  -> [| SBS.toShort   <$> PD.decodeFieldBytes |]

-- | Decoder for one map key, picking by 'MapKeyScalar'.
mapKeyDecoderE :: MapKeyScalar -> Q Exp
mapKeyDecoderE mks = scalarDecoderE
  (protoField (mkName "_k") 1 FKBare
              (PFScalar (scalarOfMapKey mks)) (ConT ''Int))

mapKeyZeroE :: MapKeyScalar -> Q Exp
mapKeyZeroE = \case
  MapKeyInt32   -> [| 0 :: Int32 |]
  MapKeyInt64   -> [| 0 :: Int64 |]
  MapKeyUInt32  -> [| 0 :: Word32 |]
  MapKeyUInt64  -> [| 0 :: Word64 |]
  MapKeySInt32  -> [| 0 :: Int32 |]
  MapKeySInt64  -> [| 0 :: Int64 |]
  MapKeyFixed32 -> [| 0 :: Word32 |]
  MapKeyFixed64 -> [| 0 :: Word64 |]
  MapKeySFixed32 -> [| 0 :: Int32 |]
  MapKeySFixed64 -> [| 0 :: Int64 |]
  MapKeyBool    -> [| False |]
  MapKeyString  -> [| T.empty |]

mapValueZeroE :: ProtoField -> Q Exp
mapValueZeroE pf = case pfType pf of
  PFScalar SBool   -> [| False |]
  PFScalar SString -> [| T.empty |]
  PFScalar SBytes  -> [| BS.empty |]
  PFScalar SFloat  -> [| 0 :: Float |]
  PFScalar SDouble -> [| 0 :: Double |]
  PFScalar SInt32  -> [| 0 :: Int32 |]
  PFScalar SInt64  -> [| 0 :: Int64 |]
  PFScalar SUInt32 -> [| 0 :: Word32 |]
  PFScalar SUInt64 -> [| 0 :: Word64 |]
  PFScalar SSInt32 -> [| 0 :: Int32 |]
  PFScalar SSInt64 -> [| 0 :: Int64 |]
  PFScalar SFixed32 -> [| 0 :: Word32 |]
  PFScalar SFixed64 -> [| 0 :: Word64 |]
  PFScalar SSFixed32 -> [| 0 :: Int32 |]
  PFScalar SSFixed64 -> [| 0 :: Int64 |]
  PFEnum           -> [| toEnum 0 |]
  PFSubmessage     ->
    -- Map value of submessage type: zero is a nonsense default
    -- because the user's map should never contain a missing value
    -- for an existing key. 'decodeMapEntry' uses this only when
    -- the wire never delivered a value field, which is a malformed
    -- entry.
    [| error "Proto.Derive: map value missing on the wire" |]

recurseLoopE :: Name -> [Name] -> Maybe Name -> Q Exp
recurseLoopE loopName accs ufAccM =
  let ufArgs = case ufAccM of
        Just ufAcc -> [VarE ufAcc]
        Nothing    -> []
  in pure (foldl AppE (VarE loopName) (map VarE accs <> ufArgs))

-- | Recurse, replacing the unknown-fields accumulator with @uf : acc@.
-- Used by the wildcard arm of the decode loop when unknown-field
-- preservation is enabled.
recurseLoopWithUFE :: Name -> [Name] -> Name -> Exp -> Q Exp
recurseLoopWithUFE loopName accs ufAcc newUF =
  let consE = InfixE (Just newUF) (ConE '(:)) (Just (VarE ufAcc))
  in pure (foldl AppE (VarE loopName) (map VarE accs <> [consE]))
