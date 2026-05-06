{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | Apache Fory xlang value decoder.
--
-- Mirrors 'Fury.Encode.encode'. Wire-compatible with @pyfory@
-- 0.17 for the value shapes documented on the encode side; see
-- "Fury.Encode" for the exact subset.
module Fury.Decode
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
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Fury.Encoding as E
import qualified Fury.MetaString as MS
import qualified Fury.MetaString.Encoder as MSE
import qualified Fury.Options as Opt
import qualified Fury.TypeId as T
import qualified Fury.Value as VV

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
      | otherwise -> Left $ "Fury.Decode: " ++ show (BS.length bs - off)
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
        then failD ("Fury.Decode.decode: missing xlang flag in header byte "
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
          "Fury.Decode: unexpected slot flag byte " ++ show flag

decodeRefBack :: DecodeM VV.Value
decodeRefBack = do
  wid <- fromIntegral <$> readVaruint32D
  st  <- getState
  case IM.lookup wid (dsRefValues st) of
    Nothing -> failD $
      "Fury.Decode: REF flag references unknown ref_id " ++ show wid
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
    "Fury.Decode.decodePayloadFor: unsupported type tag " ++ show tw

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
    0 -> pure (decodeLatin1 raw)
    1 -> case decodeUtf16LE raw of
           Right t -> pure t
           Left e  -> failD ("Fury.Decode: invalid UTF-16: " ++ e)
    2 -> case TE.decodeUtf8' raw of
           Right t -> pure t
           Left e  -> failD ("Fury.Decode: invalid UTF-8: " ++ show e)
    _ -> failD ("Fury.Decode: reserved string encoding " ++ show enc)
  where

decodeLatin1 :: ByteString -> Text
decodeLatin1 = T.pack . map (toEnum . fromIntegral) . BS.unpack

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
              failD "Fury.Decode: same-type collection without element type"
            Just tg ->
              if trackingRef
                then V.replicateM count (readSameTypeRefSlot tg)
                else if hasNull
                  then V.replicateM count $ do
                    f <- readByteD
                    if f == slotNull
                      then pure VV.NoneVal
                      else if f == slotNotNullValue
                        then decodePayloadFor tg
                        else failD ("Fury.Decode: unexpected element flag "
                                    ++ show f)
                  else V.replicateM count (decodePayloadFor tg)
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
                      ("Fury.Decode: unexpected element flag " ++ show f)
            else V.replicateM count decodeTypedPayload

-- | Element of a same-type ref-tracked homogeneous collection:
-- the wire layout is @REF@ \/ @REF_VALUE@ \/ @NULL@ slot flag,
-- followed by the payload (no inner type tag — we already know
-- the element type).
readSameTypeRefSlot :: T.TypeId -> DecodeM VV.Value
readSameTypeRefSlot tg = do
  f <- readByteD
  case f of
    _ | f == slotNull -> pure VV.NoneVal
      | f == slotRef -> decodeRefBack
      | f == slotRefValue -> do
          st <- getState
          let !wid = dsNextRefId st
          modifyState $ \s -> s { dsNextRefId = wid + 1 }
          inner <- decodePayloadFor tg
          modifyState $ \s -> s
            { dsRefValues = IM.insert wid inner (dsRefValues s) }
          pure (VV.RefVal wid inner)
      | otherwise -> failD
          ("Fury.Decode: unexpected ref-tracked element flag " ++ show f)

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
      if keyNull && valNull
        then collectChunks (remaining - 1) ((VV.NoneVal, VV.NoneVal) : acc)
        else do
          chunkSize <- fromIntegral <$> readByteD  -- pyfory: uint8
          (keyTag, valTag) <- case (keyNull, valNull) of
            (True, False) -> do
              tw <- readVaruint32D
              pure (Nothing, Just (T.TypeId (fromIntegral tw)))
            (False, True) -> do
              tw <- readVaruint32D
              pure (Just (T.TypeId (fromIntegral tw)), Nothing)
            (False, False) -> do
              twk <- readVaruint32D
              twv <- readVaruint32D
              pure ( Just (T.TypeId (fromIntegral twk))
                   , Just (T.TypeId (fromIntegral twv)))
            (True, True) -> pure (Nothing, Nothing)
          entries <- replicateMList chunkSize $ do
            k <- case keyTag of
              Nothing -> pure VV.NoneVal
              Just tg -> decodePayloadFor tg
            v <- case valTag of
              Nothing -> pure VV.NoneVal
              Just tg -> decodePayloadFor tg
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

-- ---------------------------------------------------------------------------
-- Meta-string deduplication
-- ---------------------------------------------------------------------------

decodeMetaString :: DecodeM Text
decodeMetaString = decodeMetaStringWith MSE.namespaceSpecialChars

decodeMetaStringWith :: MSE.SpecialChars -> DecodeM Text
decodeMetaStringWith sc = do
  hdr <- liftEither MS.readMetaStringHeader
  case hdr of
    MS.MetaStringRef rid -> do
      st <- getState
      case IM.lookup rid (dsStringPool st) of
        Nothing -> failD $
          "Fury.Decode.decodeMetaString: ref to unknown id " ++ show rid
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
          "Fury.Decode: TypeDef ref to unknown index " ++ show idx
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
    then failD $ "Fury.Decode: TypeDef body size mismatch (header said "
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
    then failD "Fury.Decode: TypeDef without REGISTER_BY_NAME flag"
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

-- | Read the byte length, divide by the per-element size, and
-- ensure the result is a whole number of elements.
readArrayElemCount :: Int -> DecodeM Int
readArrayElemCount elemBytes = do
  byteLen <- fromIntegral <$> readVaruint32D
  let (q, r) = byteLen `quotRem` elemBytes
  if r /= 0
    then failD $ "Fury.Decode: array byte length " ++ show byteLen
                  ++ " not a multiple of element size " ++ show elemBytes
    else pure q

decodeBoolArray :: DecodeM (Vector Bool)
decodeBoolArray = do
  n <- readArrayElemCount 1
  V.replicateM n ((/= 0) <$> readByteD)

decodeInt8Array :: DecodeM (Vector Int8)
decodeInt8Array = do
  n <- readArrayElemCount 1
  V.replicateM n (fromIntegral <$> readByteD)

decodeInt16Array :: DecodeM (Vector Int16)
decodeInt16Array = do
  n <- readArrayElemCount 2
  V.replicateM n readInt16D

decodeInt32Array :: DecodeM (Vector Int32)
decodeInt32Array = do
  n <- readArrayElemCount 4
  V.replicateM n readInt32D

decodeInt64Array :: DecodeM (Vector Int64)
decodeInt64Array = do
  n <- readArrayElemCount 8
  V.replicateM n readInt64D

decodeUint8Array :: DecodeM (Vector Word8)
decodeUint8Array = do
  n <- readArrayElemCount 1
  V.replicateM n readByteD

decodeUint16Array :: DecodeM (Vector Word16)
decodeUint16Array = do
  n <- readArrayElemCount 2
  V.replicateM n readWord16D

decodeUint32Array :: DecodeM (Vector Word32)
decodeUint32Array = do
  n <- readArrayElemCount 4
  V.replicateM n readWord32D

decodeUint64Array :: DecodeM (Vector Word64)
decodeUint64Array = do
  n <- readArrayElemCount 8
  V.replicateM n readWord64D

decodeFloat32Array :: DecodeM (Vector Float)
decodeFloat32Array = do
  n <- readArrayElemCount 4
  V.replicateM n readFloat32D

decodeFloat64Array :: DecodeM (Vector Double)
decodeFloat64Array = do
  n <- readArrayElemCount 8
  V.replicateM n readFloat64D

