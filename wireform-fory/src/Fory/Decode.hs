{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | Apache Fory xlang value decoder.
--
-- Mirrors 'Fory.Encode.encode'. Wire-compatible with @pyfory@
-- 0.17 for the value shapes documented on the encode side; see
-- "Fory.Encode" for the exact subset.
module Fory.Decode
  ( decode
  , decodeWith
  , decodeValueSlot
  , DecodeM
  , runDecodeM
  , runDecodeMWith
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Data.HashMap.Strict as HM

import qualified Fory.Bulk as B
import qualified Fory.Encoding as E
import qualified Fory.MetaString as MS
import qualified Fory.MetaString.Encoder as MSE
import qualified Fory.Options as Opt
import qualified Fory.Struct as ST
import qualified Fory.TypeId as T
import qualified Fory.Value as VV

-- ---------------------------------------------------------------------------
-- Decoder state monad
-- ---------------------------------------------------------------------------

data DecodeState = DecodeState
  { dsOptions      :: !Opt.DecodeOptions
  , dsStringPool   :: !(IntMap Text)
  , dsNextStringId :: {-# UNPACK #-} !Int
  , dsRefValues    :: !(IntMap VV.Value)
  , dsNextRefId    :: {-# UNPACK #-} !Int
  , dsTypeDefs     :: !(IntMap TypeDef)
  , dsNextTypeDefId :: {-# UNPACK #-} !Int
  }

data TypeDef = TypeDef
  { tdNamespace  :: !Text
  , tdTypeName   :: !Text
  , tdFieldNames :: !(Vector Text)
  } deriving (Show, Eq)

emptyDecodeState :: Opt.DecodeOptions -> DecodeState
emptyDecodeState opts =
  DecodeState opts IM.empty 0 IM.empty 0 IM.empty 0

newtype DecodeM a = DecodeM
  { runDM
      :: ByteString
      -> Int
      -> DecodeState
      -> Either String (a, Int, DecodeState)
  }

instance Functor DecodeM where
  fmap f (DecodeM g) = DecodeM $ \bs off st ->
    case g bs off st of
      Left e               -> Left e
      Right (a, off', st') -> Right (f a, off', st')
  {-# INLINE fmap #-}

instance Applicative DecodeM where
  pure x = DecodeM $ \_ off st -> Right (x, off, st)
  {-# INLINE pure #-}
  DecodeM f <*> DecodeM x = DecodeM $ \bs off st ->
    case f bs off st of
      Left e -> Left e
      Right (g, off1, st1) -> case x bs off1 st1 of
        Left e -> Left e
        Right (a, off2, st2) -> Right (g a, off2, st2)

instance Monad DecodeM where
  DecodeM m >>= k = DecodeM $ \bs off st ->
    case m bs off st of
      Left e -> Left e
      Right (a, off', st') -> runDM (k a) bs off' st'
  {-# INLINE (>>=) #-}

runDecodeM :: DecodeM a -> ByteString -> Either String a
runDecodeM = runDecodeMWith Opt.defaultDecodeOptions

runDecodeMWith :: Opt.DecodeOptions -> DecodeM a -> ByteString -> Either String a
runDecodeMWith opts (DecodeM m) bs =
  case m bs 0 (emptyDecodeState opts) of
    Left e -> Left e
    Right (a, off, _)
      | off == BS.length bs -> Right a
      | otherwise -> Left $ "Fory.Decode: " ++ show (BS.length bs - off)
                            ++ " trailing bytes"

liftEither :: (ByteString -> Int -> Either String (a, Int)) -> DecodeM a
liftEither f = DecodeM $ \bs off st ->
  case f bs off of
    Left e               -> Left e
    Right (a, off')      -> Right (a, off', st)
{-# INLINE liftEither #-}

getOff :: DecodeM Int
getOff = DecodeM $ \_ off st -> Right (off, off, st)

getState :: DecodeM DecodeState
getState = DecodeM $ \_ off st -> Right (st, off, st)

modifyState :: (DecodeState -> DecodeState) -> DecodeM ()
modifyState f = DecodeM $ \_ off st -> Right ((), off, f st)

failD :: String -> DecodeM a
failD e = DecodeM $ \_ _ _ -> Left e

readByteD :: DecodeM Word8
readByteD = liftEither E.readByte

readBytesD :: Int -> DecodeM ByteString
readBytesD n = liftEither (E.readBytes n)

readWord16D :: DecodeM Word16
readWord16D = liftEither E.readWord16LE

readWord32D :: DecodeM Word32
readWord32D = liftEither E.readWord32LE

readWord64D :: DecodeM Word64
readWord64D = liftEither E.readWord64LE

readInt16D :: DecodeM Int16
readInt16D = liftEither E.readInt16LE

readInt32D :: DecodeM Int32
readInt32D = liftEither E.readInt32LE

readInt64D :: DecodeM Int64
readInt64D = liftEither E.readInt64LE

readFloat32D :: DecodeM Float
readFloat32D = liftEither E.readFloat32LE

readFloat64D :: DecodeM Double
readFloat64D = liftEither E.readFloat64LE

readVaruint32D :: DecodeM Word32
readVaruint32D = liftEither E.readVaruint32

readVaruint64D :: DecodeM Word64
readVaruint64D = liftEither E.readVaruint64

readVarint32D :: DecodeM Int32
readVarint32D = liftEither E.readVarint32

readVarint64D :: DecodeM Int64
readVarint64D = liftEither E.readVarint64

-- ---------------------------------------------------------------------------
-- Slot flag bytes
-- ---------------------------------------------------------------------------

slotNotNullValue, slotNull, slotRefValue, slotRef :: Word8
slotNotNullValue = 0xFF
slotNull         = 0xFD
slotRefValue     = 0x00
slotRef          = 0xFE

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Parse a fory-encoded byte string back to a 'Value' under the
-- default 'Opt.defaultDecodeOptions' (no reference tracking).
decode :: ByteString -> Either String VV.Value
decode = decodeWith Opt.defaultDecodeOptions

-- | Decode under explicit options. Use
-- @'Opt.DecodeOptions' { 'Opt.doRefTracking' = True }@ to read
-- bytes produced by an encoder with 'Opt.eoRefTracking' on (or
-- by @pyfory.Fory(xlang=True, ref=True)@).
decodeWith :: Opt.DecodeOptions -> ByteString -> Either String VV.Value
decodeWith opts bs = runDecodeMWith opts go bs
  where
    go = do
      hdr <- readByteD
      if hdr .&. 0x02 == 0
        then failD ("Fory.Decode.decode: missing xlang flag in header byte "
                    ++ show hdr)
        else decodeValueSlot

decodeValueSlot :: DecodeM VV.Value
decodeValueSlot = do
  flag <- readByteD
  case flag of
    f | f == slotNull       -> pure VV.NoneVal
      | f == slotRef        -> decodeRefBack
      | f == slotRefValue   -> decodeRefValue
      | f == slotNotNullValue -> decodeTypedPayload
      | otherwise -> failD $
          "Fory.Decode: unexpected slot flag byte " ++ show flag

decodeRefBack :: DecodeM VV.Value
decodeRefBack = do
  wid <- fromIntegral <$> readVaruint32D
  st  <- getState
  case IM.lookup wid (dsRefValues st) of
    Nothing -> failD $
      "Fory.Decode: REF flag references unknown ref_id " ++ show wid
    Just v  -> pure (VV.RefVal wid v)

decodeRefValue :: DecodeM VV.Value
decodeRefValue = do
  st <- getState
  let !wid = dsNextRefId st
  modifyState $ \s -> s { dsNextRefId = wid + 1 }
  inner <- decodeTypedPayload
  modifyState $ \s -> s { dsRefValues = IM.insert wid inner (dsRefValues s) }
  pure (VV.RefVal wid inner)

-- | Read a type tag (varuint32 in pyfory's wire format) plus
-- payload.
decodeTypedPayload :: DecodeM VV.Value
decodeTypedPayload = do
  tagW <- readVaruint32D
  decodePayloadFor (T.TypeId (fromIntegral tagW))

decodePayloadFor :: T.TypeId -> DecodeM VV.Value
decodePayloadFor tag = case tag of
  T.NONE     -> pure VV.NoneVal
  T.BOOL     -> VV.BoolVal . (/= 0) <$> readByteD
  T.INT8     -> VV.Int8Val . fromIntegral <$> readByteD
  T.INT16    -> VV.Int16Val <$> readInt16D
  T.INT32    -> VV.Int32Val <$> readInt32D
  T.VARINT32 -> VV.VarInt32Val <$> readVarint32D
  T.INT64    -> VV.Int64Val <$> readInt64D
  T.VARINT64 -> VV.VarInt64Val <$> readVarint64D
  T.UINT8    -> VV.Uint8Val <$> readByteD
  T.UINT16   -> VV.Uint16Val <$> readWord16D
  T.UINT32   -> VV.Uint32Val <$> readWord32D
  T.VAR_UINT32 -> VV.VarUint32Val <$> readVaruint32D
  T.UINT64   -> VV.Uint64Val <$> readWord64D
  T.VAR_UINT64 -> VV.VarUint64Val <$> readVaruint64D
  T.FLOAT32  -> VV.Float32Val <$> readFloat32D
  T.FLOAT64  -> VV.Float64Val <$> readFloat64D
  T.STRING   -> VV.StringVal <$> readForyString
  T.BINARY   -> do
    n   <- fromIntegral <$> readVaruint32D
    raw <- readBytesD n
    pure (VV.BinaryVal raw)
  T.LIST     -> VV.ListVal <$> decodeCollection
  T.SET      -> VV.SetVal  <$> decodeCollection
  T.MAP      -> decodeMapChunks
  T.NAMED_STRUCT -> do
    ns     <- decodeMetaStringWith MSE.namespaceSpecialChars
    typeNm <- decodeMetaStringWith MSE.typenameSpecialChars
    -- Disambiguate the pyfory-compatible wire layout from our
    -- in-package self-describing layout by checking the
    -- struct registry. If the schema is present we parse
    -- (4-byte hash + canonical-order fields); otherwise we
    -- parse (varuint32 num_fields + per-field meta-string +
    -- value).
    st <- getState
    case HM.lookup (ns, typeNm) (Opt.doStructRegistry (dsOptions st)) of
      Just sch -> decodeRegisteredStruct ns typeNm sch
      Nothing  -> do
        fields <- decodeStructFields
        pure (VV.StructVal ns typeNm fields)
  T.NAMED_COMPATIBLE_STRUCT -> decodeCompatibleStruct
  T.BOOL_ARRAY    -> VV.BoolArrayVal    <$> decodeBoolArray
  T.INT8_ARRAY    -> VV.Int8ArrayVal    <$> decodeInt8Array
  T.INT16_ARRAY   -> VV.Int16ArrayVal   <$> decodeInt16Array
  T.INT32_ARRAY   -> VV.Int32ArrayVal   <$> decodeInt32Array
  T.INT64_ARRAY   -> VV.Int64ArrayVal   <$> decodeInt64Array
  T.UINT8_ARRAY   -> VV.Uint8ArrayVal   <$> decodeUint8Array
  T.UINT16_ARRAY  -> VV.Uint16ArrayVal  <$> decodeUint16Array
  T.UINT32_ARRAY  -> VV.Uint32ArrayVal  <$> decodeUint32Array
  T.UINT64_ARRAY  -> VV.Uint64ArrayVal  <$> decodeUint64Array
  T.FLOAT32_ARRAY -> VV.Float32ArrayVal <$> decodeFloat32Array
  T.FLOAT64_ARRAY -> VV.Float64ArrayVal <$> decodeFloat64Array
  T.TypeId tw -> failD $
    "Fory.Decode.decodePayloadFor: unsupported type tag " ++ show tw

-- ---------------------------------------------------------------------------
-- Strings
-- ---------------------------------------------------------------------------

readForyString :: DecodeM Text
readForyString = do
  hdr <- readVaruint64D
  let !enc = hdr .&. 0x03
      !len = fromIntegral (hdr `shiftR` 2) :: Int
  raw <- readBytesD len
  case enc of
    0 -> pure (TE.decodeLatin1 raw)  -- single-allocation pure-Haskell decode
    1 -> case decodeUtf16LE raw of
           Right t -> pure t
           Left e  -> failD ("Fory.Decode: invalid UTF-16: " ++ e)
    2 -> case TE.decodeUtf8' raw of
           Right t -> pure t
           Left e  -> failD ("Fory.Decode: invalid UTF-8: " ++ show e)
    _ -> failD ("Fory.Decode: reserved string encoding " ++ show enc)

decodeUtf16LE :: ByteString -> Either String Text
decodeUtf16LE bs
  | BS.length bs `mod` 2 /= 0 = Left "odd-length UTF-16 byte string"
  | otherwise = Right (TE.decodeUtf16LE bs)

-- ---------------------------------------------------------------------------
-- Collections
-- ---------------------------------------------------------------------------

collFlagTrackingRef, collFlagHasNull, collFlagDeclElementType, collFlagIsSameType :: Word8
collFlagTrackingRef     = 0b0001
collFlagHasNull         = 0b0010
collFlagDeclElementType = 0b0100
collFlagIsSameType      = 0b1000

decodeCollection :: DecodeM (Vector VV.Value)
decodeCollection = do
  count <- fromIntegral <$> readVaruint32D
  if count == 0
    then pure V.empty
    else do
      flag <- readByteD
      let !sameType    = flag .&. collFlagIsSameType /= 0
          !hasNull     = flag .&. collFlagHasNull    /= 0
          !trackingRef = flag .&. collFlagTrackingRef /= 0
          !decl        = flag .&. collFlagDeclElementType /= 0
      if sameType
        then do
          elemTag <-
            if decl
              then pure Nothing
              else do
                tw <- readVaruint32D
                pure (Just (T.TypeId (fromIntegral tw)))
          case elemTag of
            Nothing ->
              failD "Fory.Decode: same-type collection without element type"
            Just tg -> do
              -- For NAMED_STRUCT-like elements pyfory writes the
              -- ns + tn once at the element-type position; pre-read
              -- them and feed the per-element decoder a payload-only
              -- reader.
              elemReader <- elementReaderForSameType tg
              if trackingRef
                then V.replicateM count (readSameTypeRefSlotE elemReader)
                else if hasNull
                  then V.replicateM count $ do
                    f <- readByteD
                    if f == slotNull
                      then pure VV.NoneVal
                      else if f == slotNotNullValue
                        then elemReader
                        else failD ("Fory.Decode: unexpected element flag "
                                    ++ show f)
                  else V.replicateM count elemReader
        else
          if hasNull
            then V.replicateM count $ do
              f <- readByteD
              case f of
                _ | f == slotNull -> pure VV.NoneVal
                  | f == slotRef -> decodeRefBack
                  | f == slotRefValue -> decodeRefValue
                  | f == slotNotNullValue -> decodeTypedPayload
                  | otherwise -> failD
                      ("Fory.Decode: unexpected element flag " ++ show f)
            else V.replicateM count decodeTypedPayload

-- | Build a per-element payload reader for a same-type
-- collection. For the named-struct flavours we pre-read the
-- namespace + type-name meta-strings (and the corresponding
-- registry lookup) right after the element type tag; the
-- returned action then reads only the per-element payload
-- (hash + fields, etc.).
elementReaderForSameType :: T.TypeId -> DecodeM (DecodeM VV.Value)
elementReaderForSameType tg = case tg of
  T.NAMED_STRUCT -> do
    ns     <- decodeMetaStringWith MSE.namespaceSpecialChars
    typeNm <- decodeMetaStringWith MSE.typenameSpecialChars
    st <- getState
    case HM.lookup (ns, typeNm) (Opt.doStructRegistry (dsOptions st)) of
      Just sch ->
        pure (decodeRegisteredStruct ns typeNm sch)
      Nothing -> do
        -- Fallback to the in-package self-describing layout.
        pure $ do
          fields <- decodeStructFields
          pure (VV.StructVal ns typeNm fields)
  T.NAMED_COMPATIBLE_STRUCT ->
    -- Compatible struct doesn't pre-share ns+tn at the
    -- element-type position because each instance carries the
    -- meta-share marker; fall through to per-element payload
    -- handling.
    pure (decodePayloadFor tg)
  _ ->
    pure (decodePayloadFor tg)

-- | Element of a same-type ref-tracked homogeneous collection.
readSameTypeRefSlotE :: DecodeM VV.Value -> DecodeM VV.Value
readSameTypeRefSlotE inner = do
  f <- readByteD
  case f of
    _ | f == slotNull -> pure VV.NoneVal
      | f == slotRef -> decodeRefBack
      | f == slotRefValue -> do
          st <- getState
          let !wid = dsNextRefId st
          modifyState $ \s -> s { dsNextRefId = wid + 1 }
          v <- inner
          modifyState $ \s -> s
            { dsRefValues = IM.insert wid v (dsRefValues s) }
          pure (VV.RefVal wid v)
      | otherwise -> failD
          ("Fory.Decode: unexpected ref-tracked element flag " ++ show f)

-- ---------------------------------------------------------------------------
-- Maps
-- ---------------------------------------------------------------------------

mapKeyHasNull, mapValueHasNull :: Word8
mapKeyHasNull       = 0b0000_0010
mapValueHasNull     = 0b0001_0000

decodeMapChunks :: DecodeM VV.Value
decodeMapChunks = do
  total <- fromIntegral <$> readVaruint32D
  if total == 0
    then pure (VV.MapVal V.empty)
    else do
      pairs <- collectChunks total []
      pure (VV.MapVal (V.fromList (reverse pairs)))
  where
    collectChunks 0 acc = pure acc
    collectChunks remaining acc = do
      header <- readByteD
      let !keyNull = header .&. mapKeyHasNull /= 0
          !valNull = header .&. mapValueHasNull /= 0
      case (keyNull, valNull) of
        -- Both null: implied chunk_size 1, no payload.
        (True, True) ->
          collectChunks (remaining - 1) ((VV.NoneVal, VV.NoneVal) : acc)
        -- Key null only: implied chunk_size 1, slot flag +
        -- value type + value payload.
        (True, False) -> do
          flag <- readByteD
          if flag /= slotNotNullValue
            then failD ("Fury.Decode: expected NOT_NULL_VALUE for partial-"
                        ++ "null map value, got " ++ show flag)
            else do
              tw <- readVaruint32D
              v  <- decodePayloadFor (T.TypeId (fromIntegral tw))
              collectChunks (remaining - 1) ((VV.NoneVal, v) : acc)
        -- Value null only: implied chunk_size 1, slot flag +
        -- key type + key payload.
        (False, True) -> do
          flag <- readByteD
          if flag /= slotNotNullValue
            then failD ("Fury.Decode: expected NOT_NULL_VALUE for partial-"
                        ++ "null map key, got " ++ show flag)
            else do
              tw <- readVaruint32D
              k  <- decodePayloadFor (T.TypeId (fromIntegral tw))
              collectChunks (remaining - 1) ((k, VV.NoneVal) : acc)
        -- Neither null: chunk_size + key_type + value_type +
        -- (key_payload, value_payload) * chunk_size.
        (False, False) -> do
          chunkSize <- fromIntegral <$> readByteD
          twk <- readVaruint32D
          twv <- readVaruint32D
          let keyTag = T.TypeId (fromIntegral twk)
              valTag = T.TypeId (fromIntegral twv)
          entries <- replicateMList chunkSize $ do
            k <- decodePayloadFor keyTag
            v <- decodePayloadFor valTag
            pure (k, v)
          collectChunks (remaining - chunkSize)
                        (foldl (flip (:)) acc entries)

replicateMList :: Int -> DecodeM a -> DecodeM [a]
replicateMList n act
  | n <= 0    = pure []
  | otherwise = do
      x  <- act
      xs <- replicateMList (n - 1) act
      pure (x : xs)

-- ---------------------------------------------------------------------------
-- Struct (NAMED_STRUCT)
-- ---------------------------------------------------------------------------

decodeStructFields :: DecodeM VV.StructFields
decodeStructFields = do
  n <- fromIntegral <$> readVaruint32D
  V.replicateM n $ do
    name <- decodeMetaStringWith MSE.namespaceSpecialChars
    val  <- decodeValueSlot
    pure (name, val)

-- | Read the pyfory-compatible @NAMED_STRUCT@ payload, given the
-- already-consumed namespace + type name and the matching
-- 'ST.StructSchema'. Layout:
--
-- @
-- | int32 hash | (per-field payload, in canonical order) |
-- @
--
-- The decoder validates the hash against the registered schema
-- so a schema-version mismatch fails fast.
decodeRegisteredStruct
  :: Text -> Text -> ST.StructSchema -> DecodeM VV.Value
decodeRegisteredStruct ns typeNm sch = do
  wireHash <- readInt32D
  let expected = ST.computeStructHash sch
  if wireHash /= expected
    then failD $
      "Fory.Decode: struct schema hash mismatch for "
        ++ T.unpack ns ++ "." ++ T.unpack typeNm
        ++ ": wire " ++ show wireHash
        ++ " /= local " ++ show expected
    else do
      let canonical = ST.fieldOrder sch
      values <- V.mapM readField canonical
      let resultPairs = V.zip (V.map ST.fsName canonical) values
      pure (VV.RegisteredStructVal ns typeNm resultPairs)
  where
    readField :: ST.FieldSpec -> DecodeM VV.Value
    readField spec
      | ST.isBasicTypeId (ST.fsTypeId spec) =
          if ST.fsNullable spec
            then do
              flag <- readByteD
              if flag == slotNull
                then pure VV.NoneVal
                else if flag == slotNotNullValue
                  then decodePayloadFor (ST.fsTypeId spec)
                  else failD ("Fory.Decode: bad nullable basic field flag "
                              ++ show flag)
            else decodePayloadFor (ST.fsTypeId spec)
      | otherwise = do
          flag <- readByteD
          if flag == slotNull
            then pure VV.NoneVal
            else if flag == slotNotNullValue
              then decodePayloadFor (ST.fsTypeId spec)
              else failD ("Fory.Decode: bad non-basic field flag "
                          ++ show flag)

-- ---------------------------------------------------------------------------
-- Meta-string deduplication
-- ---------------------------------------------------------------------------

decodeMetaStringWith :: MSE.SpecialChars -> DecodeM Text
decodeMetaStringWith sc = do
  hdr <- liftEither MS.readMetaStringHeader
  case hdr of
    MS.MetaStringRef rid -> do
      st <- getState
      case IM.lookup rid (dsStringPool st) of
        Nothing -> failD $
          "Fory.Decode.decodeMetaString: ref to unknown id " ++ show rid
        Just t  -> pure t
    MS.MetaStringFresh len -> do
      t  <- liftEither (MS.readFreshMetaStringPayload sc len)
      st <- getState
      let !nid = dsNextStringId st
      modifyState $ \s -> s
        { dsStringPool   = IM.insert nid t (dsStringPool s)
        , dsNextStringId = nid + 1
        }
      pure t

-- ---------------------------------------------------------------------------
-- TypeDef + NAMED_COMPATIBLE_STRUCT
-- ---------------------------------------------------------------------------

decodeCompatibleStruct :: DecodeM VV.Value
decodeCompatibleStruct = do
  marker <- readVaruint64D
  td <- if marker .&. 1 /= 0
    then do
      let !idx = fromIntegral (marker `shiftR` 1) :: Int
      st <- getState
      case IM.lookup idx (dsTypeDefs st) of
        Nothing -> failD $
          "Fory.Decode: TypeDef ref to unknown index " ++ show idx
        Just td -> pure td
    else do
      let !idx = fromIntegral (marker `shiftR` 1) :: Int
      td <- decodeTypeDefBytes
      modifyState $ \s -> s
        { dsTypeDefs      = IM.insert idx td (dsTypeDefs s)
        , dsNextTypeDefId = idx + 1
        }
      pure td
  values <- V.mapM (const decodeValueSlot) (tdFieldNames td)
  let fields = V.zip (tdFieldNames td) values
  pure (VV.CompatibleStructVal (tdNamespace td) (tdTypeName td) fields)

decodeTypeDefBytes :: DecodeM TypeDef
decodeTypeDefBytes = do
  hdr <- readWord64D
  let !rawSize = hdr .&. 0xFF
  bodyLen <-
    if rawSize == 0xFF
      then do
        ext <- readVaruint32D
        pure (0xFF + fromIntegral ext :: Int)
      else
        pure (fromIntegral rawSize :: Int)
  off0 <- getOff
  td <- decodeTypeDefBody
  off1 <- getOff
  let !consumed = off1 - off0
  if consumed /= bodyLen
    then failD $ "Fory.Decode: TypeDef body size mismatch (header said "
                  ++ show bodyLen ++ ", consumed " ++ show consumed ++ ")"
    else pure td

decodeTypeDefBody :: DecodeM TypeDef
decodeTypeDefBody = do
  metaHeader <- readByteD
  let !rawNumFields = fromIntegral (metaHeader .&. 0x1F) :: Int
      !registered = (metaHeader .&. 0x20) /= 0
  numFields <- if rawNumFields >= 31
    then do
      ext <- readVaruint32D
      pure (rawNumFields + fromIntegral ext)
    else
      pure rawNumFields
  if not registered
    then failD "Fory.Decode: TypeDef without REGISTER_BY_NAME flag"
    else do
      ns     <- decodeMetaStringWith MSE.namespaceSpecialChars
      typeNm <- decodeMetaStringWith MSE.typenameSpecialChars
      fieldNames <- V.replicateM numFields $ do
        fname <- decodeMetaStringWith MSE.namespaceSpecialChars
        _typeId <- readVaruint32D
        pure fname
      pure (TypeDef ns typeNm fieldNames)

-- ---------------------------------------------------------------------------
-- Primitive 1-D arrays
-- ---------------------------------------------------------------------------
--
-- The wire layout is | varuint32 byte_length | raw bytes |, so
-- we read the whole slice in one shot and let 'Fory.Bulk' do
-- the cast in a tight @V.generate@ loop. This drops the
-- 1k-element decode path from a per-element state-monad bind
-- (~16 us at 1024 elements) to a single bytestring slice +
-- O(n) generate.

-- | Read the @varuint32 byteLen + bytes@ payload for a
-- primitive array, validate alignment, and reinterpret the
-- raw bytes as a 'VS.Vector' of the appropriate element type.
-- Zero-copy on little-endian platforms.
bulkArray
  :: Int                              -- ^ element size in bytes
  -> (ByteString -> VS.Vector a)      -- ^ zero-copy reinterpret
  -> DecodeM (VS.Vector a)
bulkArray elemBytes f = do
  byteLen <- fromIntegral <$> readVaruint32D
  let (_, r) = byteLen `quotRem` elemBytes
  if r /= 0
    then failD $ "Fory.Decode: array byte length " ++ show byteLen
                  ++ " not a multiple of element size " ++ show elemBytes
    else f <$> readBytesD byteLen

decodeBoolArray    = bulkArray 1 B.bytesToBoolArray
decodeInt8Array    = bulkArray 1 B.bytesToInt8Array
decodeInt16Array   = bulkArray 2 B.bytesToInt16Array
decodeInt32Array   = bulkArray 4 B.bytesToInt32Array
decodeInt64Array   = bulkArray 8 B.bytesToInt64Array
decodeUint8Array   = bulkArray 1 B.bytesToUint8Array
decodeUint16Array  = bulkArray 2 B.bytesToUint16Array
decodeUint32Array  = bulkArray 4 B.bytesToUint32Array
decodeUint64Array  = bulkArray 8 B.bytesToUint64Array
decodeFloat32Array = bulkArray 4 B.bytesToFloat32Array
decodeFloat64Array = bulkArray 8 B.bytesToFloat64Array

decodeBoolArray    :: DecodeM (VS.Vector Word8)
decodeInt8Array    :: DecodeM (VS.Vector Int8)
decodeInt16Array   :: DecodeM (VS.Vector Int16)
decodeInt32Array   :: DecodeM (VS.Vector Int32)
decodeInt64Array   :: DecodeM (VS.Vector Int64)
decodeUint8Array   :: DecodeM (VS.Vector Word8)
decodeUint16Array  :: DecodeM (VS.Vector Word16)
decodeUint32Array  :: DecodeM (VS.Vector Word32)
decodeUint64Array  :: DecodeM (VS.Vector Word64)
decodeFloat32Array :: DecodeM (VS.Vector Float)
decodeFloat64Array :: DecodeM (VS.Vector Double)

