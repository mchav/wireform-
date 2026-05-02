{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Arrow ↔ ORC column-data bridge.
--
-- Lets callers keep a single in-memory representation
-- ('Arrow.Column.ColumnArray') and serialise it as ORC instead.
-- Mirrors "Parquet.Arrow" for the same flat-primitive subset
-- ORC's writer natively encodes today.
--
-- @
-- -- Arrow → ORC
-- let !(types, stripes) = 'arrowToORC' arrowSchema arrowBatches
--     bytes             = 'ORC.HighLevel.encodeORC'
--                            'ORC.HighLevel.defaultWriteOptions'
--                            types stripes
--
-- -- ORC → Arrow (one stripe at a time)
-- footer <- 'ORC.HighLevel.decodeORC' bytes
-- batch  <- 'orcStripeToArrow' arrowSchema bytes footer 0
-- @
--
-- Coverage today: flat-primitive Arrow columns only — 'AInt'
-- (8/16/32/64), 'ABool', 'AFloatingPoint' (Single + Double),
-- 'AUtf8', 'ABinary', and their nullable variants. Nested types
-- (struct / list / map / union / dictionary / view / REE)
-- aren't yet routed through ORC's nested writer; they fall
-- through to a clean 'Left' at translation time.
module ORC.Arrow
  ( -- * Arrow → ORC
    arrowToORC
  , columnArrayToORCStreams
    -- * ORC → Arrow
  , orcStripeToArrow
    -- * Legacy / deprecated variants
  , arrowToORCWithRows
  , arrowToORCWithoutRows
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word (Word32, Word64)

import qualified Arrow.Column as AC
import qualified Arrow.Types as AT

import qualified ORC.Read   as OR
import qualified ORC.Stripe as OSt
import qualified ORC.Types  as OT
import qualified ORC.Write  as OW

-- ============================================================
-- Stream-kind constants (per ORC's Stream.proto)
-- ============================================================

-- | @Kind = PRESENT@.
streamPresent :: Word64
streamPresent = 0

-- | @Kind = DATA@.
streamData :: Word64
streamData = 1

-- | @Kind = LENGTH@.
streamLength :: Word64
streamLength = 2

-- | @Kind = SECONDARY@ — used for ORC timestamp nanoseconds (not
-- exercised in this bridge yet).
_streamSecondary :: Word64
_streamSecondary = 7

-- ============================================================
-- Arrow → ORC
-- ============================================================

-- | Lower an Arrow schema + a sequence of column-major batches
-- to the inputs 'ORC.HighLevel.encodeORC' expects.
--
-- Each Arrow batch becomes one ORC stripe. Returns 'Left' if any
-- column type isn't representable in ORC's flat data plane yet.
-- | Lower an Arrow schema + a sequence of column-major batches
-- to the inputs 'ORC.HighLevel.encodeORC' expects.
--
-- Each Arrow batch becomes one ORC stripe. The output pairs each
-- stripe's stream tuples with its row count (derived from the
-- first column's length) so 'ORC.HighLevel.encodeORC' can stamp
-- @siNumberOfRows@ directly into the stripe information.
--
-- Returns 'Left' if any column type isn't representable in ORC's
-- data plane yet (see 'columnArrayToORCStreams' for the
-- supported shapes).
arrowToORC
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String ( V.Vector OT.ORCType
                   , [(V.Vector (Word64, Word64, ByteString), Word64)]
                   )
arrowToORC sch batches = do
  let !leafFields = V.filter (V.null . AT.fieldChildren) (AT.arrowFields sch)
      !rootType = OT.ORCType
        { OT.otKind       = OT.TKStruct
        , OT.otSubtypes   = V.generate
                              (V.length leafFields)
                              (\i -> fromIntegral (i + 1) :: Word32)
        , OT.otFieldNames = V.map AT.fieldName leafFields
        }
  childTypes <- V.mapM arrowFieldToORCType leafFields
  let !types = V.cons rootType childTypes
  stripes <- mapM
    (\cols -> do
        streams <- encodeStripe leafFields cols
        let !rowCount = if V.null cols
                           then 0
                           else fromIntegral (AC.columnLength (V.head cols))
        Right (streams, rowCount))
    batches
  Right (types, stripes)

-- | Build one stripe from a single batch of columns by encoding
-- each leaf column to its ORC stream tuples and concatenating.
encodeStripe
  :: V.Vector AT.Field
  -> V.Vector AC.ColumnArray
  -> Either String (V.Vector (Word64, Word64, ByteString))
encodeStripe leafFields cols
  | V.length leafFields /= V.length cols =
      Left $ "ORC.Arrow.arrowToORC: schema has "
              ++ show (V.length leafFields)
              ++ " leaf fields but batch has "
              ++ show (V.length cols)
              ++ " columns"
  | otherwise = do
      let !idxedCols = V.zip3 (V.enumFromN (1 :: Int) (V.length cols))
                              leafFields
                              cols
      streamLists <- V.mapM
        (\(colIdx, _fld, col) ->
            columnArrayToORCStreams (fromIntegral colIdx) col)
        idxedCols
      Right (V.concat (V.toList streamLists))

-- | Map an Arrow leaf type onto its ORC type counterpart.
arrowFieldToORCType :: AT.Field -> Either String OT.ORCType
arrowFieldToORCType f = do
  let !ty = AT.fieldType f
  kind <- case ty of
    AT.AInt 8  True  -> Right OT.TKByte
    AT.AInt 16 True  -> Right OT.TKShort
    AT.AInt 32 True  -> Right OT.TKInt
    AT.AInt 64 True  -> Right OT.TKLong
    AT.AInt 8  False -> Right OT.TKByte
    AT.AInt 16 False -> Right OT.TKShort
    AT.AInt 32 False -> Right OT.TKInt
    AT.AInt 64 False -> Right OT.TKLong
    AT.ABool         -> Right OT.TKBoolean
    AT.AFloatingPoint AT.Single          -> Right OT.TKFloat
    AT.AFloatingPoint AT.DoublePrecision -> Right OT.TKDouble
    AT.AUtf8         -> Right OT.TKString
    AT.ABinary       -> Right OT.TKBinary
    AT.ALargeUtf8    -> Right OT.TKString
    AT.ALargeBinary  -> Right OT.TKBinary
    -- Temporal types map to ORC's native kinds. ORC doesn't
    -- distinguish time-of-day from timestamp, so ATime32 / ATime64
    -- get flattened to TKLong (caller records the semantics via
    -- Arrow's Field metadata) rather than TKTimestamp.
    AT.ADate _      -> Right OT.TKDate
    AT.ATimestamp _ _ -> Right OT.TKTimestamp
    AT.ADuration _  -> Right OT.TKLong
    AT.ATime _ _    -> Right OT.TKLong
    AT.ADecimal _ _ -> Right OT.TKDecimal
    other ->
      Left $ "ORC.Arrow: Arrow type "
             ++ show other
             ++ " has no flat ORC equivalent"
  Right OT.ORCType
    { OT.otKind       = kind
    , OT.otSubtypes   = V.empty
    , OT.otFieldNames = V.empty
    }

-- | Encode one Arrow column at the given ORC column id into its
-- ORC stream tuples. Returns @[(streamKind, columnId, payload)]@
-- in emission order. Nullable columns now emit a PRESENT stream
-- so round-trips recover the nulls at the right positions.
columnArrayToORCStreams
  :: Word64
  -> AC.ColumnArray
  -> Either String (V.Vector (Word64, Word64, ByteString))
columnArrayToORCStreams !cid = go
  where
    go col = case col of
      AC.ColInt8  v -> Right (intStreams Nothing cid (signedI8 v))
      AC.ColInt16 v -> Right (intStreams Nothing cid (signedI16 v))
      AC.ColInt32 v -> Right (intStreams Nothing cid (signedI32 v))
      AC.ColInt64 v -> Right (intStreams Nothing cid v)
      AC.ColUInt8  v -> Right (intStreams Nothing cid (VP.map fromIntegral v))
      AC.ColUInt16 v -> Right (intStreams Nothing cid (VP.map fromIntegral v))
      AC.ColUInt32 v -> Right (intStreams Nothing cid (VP.map fromIntegral v))
      AC.ColUInt64 v -> Right (intStreams Nothing cid (VP.map fromIntegral v))

      AC.ColBool   v -> Right (boolStreams Nothing cid v)
      AC.ColFloat  v -> Right (floatStreams Nothing cid v)
      AC.ColDouble v -> Right (doubleStreams Nothing cid v)

      AC.ColUtf8 v -> Right (stringStreams Nothing cid (V.map TE.encodeUtf8 v))
      AC.ColLargeUtf8 v -> Right (stringStreams Nothing cid (V.map TE.encodeUtf8 v))
      AC.ColBinary v -> Right (stringStreams Nothing cid v)
      AC.ColLargeBinary v -> Right (stringStreams Nothing cid v)

      -- Temporal types: map to an integer stream at the natural
      -- width. Date = days-since-epoch, Time/Duration/Timestamp
      -- use the Int32/Int64 payload as-is.
      AC.ColDate32 v -> Right (intStreams Nothing cid (signedI32 v))
      AC.ColDate64 v -> Right (intStreams Nothing cid v)
      AC.ColTime32 v -> Right (intStreams Nothing cid (signedI32 v))
      AC.ColTime64 v -> Right (intStreams Nothing cid v)
      AC.ColTimestamp v -> Right (intStreams Nothing cid v)
      AC.ColDuration  v -> Right (intStreams Nothing cid v)

      -- Nullable variants: emit PRESENT + present-only data.
      AC.ColInt8Maybe   v -> Right (intMaybe v cid signedI8')
      AC.ColInt16Maybe  v -> Right (intMaybe v cid signedI16')
      AC.ColInt32Maybe  v -> Right (intMaybe v cid signedI32')
      AC.ColInt64Maybe  v -> Right (intMaybe v cid id)
      AC.ColUInt8Maybe  v -> Right (intMaybe v cid fromIntegral)
      AC.ColUInt16Maybe v -> Right (intMaybe v cid fromIntegral)
      AC.ColUInt32Maybe v -> Right (intMaybe v cid fromIntegral)
      AC.ColUInt64Maybe v -> Right (intMaybe v cid fromIntegral)

      AC.ColBoolMaybe   v ->
        let (pres, present) = presentBits v
        in Right (boolStreams (Just pres) cid present)
      AC.ColFloatMaybe  v ->
        let (pres, present) = presentFloat v
        in Right (floatStreams (Just pres) cid present)
      AC.ColDoubleMaybe v ->
        let (pres, present) = presentDouble v
        in Right (doubleStreams (Just pres) cid present)

      AC.ColUtf8Maybe   v ->
        let (pres, present) = presentBytes (V.map (fmap TE.encodeUtf8) v)
        in Right (stringStreams (Just pres) cid present)
      AC.ColLargeUtf8Maybe v ->
        let (pres, present) = presentBytes (V.map (fmap TE.encodeUtf8) v)
        in Right (stringStreams (Just pres) cid present)
      AC.ColBinaryMaybe v ->
        let (pres, present) = presentBytes v
        in Right (stringStreams (Just pres) cid present)
      AC.ColLargeBinaryMaybe v ->
        let (pres, present) = presentBytes v
        in Right (stringStreams (Just pres) cid present)

      -- Nullable temporals: reuse intMaybe with the matching
      -- width-preserving cast. Narrow Int32 payloads get
      -- signedI32'; native-Int64 Timestamps / Date64 / Time64 /
      -- Duration use id.
      AC.ColDate32Maybe    v -> Right (intMaybe v cid signedI32')
      AC.ColDate64Maybe    v -> Right (intMaybe v cid id)
      AC.ColTime32Maybe    v -> Right (intMaybe v cid signedI32')
      AC.ColTime64Maybe    v -> Right (intMaybe v cid id)
      AC.ColTimestampMaybe v -> Right (intMaybe v cid id)
      AC.ColDurationMaybe  v -> Right (intMaybe v cid id)

      other -> Left $ "ORC.Arrow: column shape "
                       ++ show other
                       ++ " not supported by the bridge yet "
                       ++ "(nested types, dictionary, view, REE)"

    -- Build PRESENT stream + present-only Int64 payload for a
    -- nullable integer column. We materialise the entire payload
    -- at the requested signed width.
    intMaybe
      :: V.Vector (Maybe a)
      -> Word64
      -> (a -> Int64)
      -> V.Vector (Word64, Word64, ByteString)
    intMaybe vmb c cast =
      let (pres, xs) = presentPayload vmb cast
      in  intStreams (Just pres) c (VP.fromList xs)

    -- Boolean-RLE-encoded PRESENT bits + present-only payload.
    presentPayload
      :: V.Vector (Maybe a) -> (a -> b) -> (ByteString, [b])
    presentPayload vmb cast =
      let !pres   = OW.encodeBooleanRLE (V.map maybeToBool vmb)
          !xs     = [cast x | Just x <- V.toList vmb]
      in (pres, xs)

    maybeToBool :: Maybe a -> Bool
    maybeToBool (Just _) = True
    maybeToBool Nothing  = False

    presentBits v =
      let !pres = OW.encodeBooleanRLE (V.map maybeToBool v)
          !vs   = V.fromList [b | Just b <- V.toList v]
      in (pres, vs)
    presentFloat v =
      let !pres = OW.encodeBooleanRLE (V.map maybeToBool v)
          !vs   = VP.fromList [f | Just f <- V.toList v]
      in (pres, vs)
    presentDouble v =
      let !pres = OW.encodeBooleanRLE (V.map maybeToBool v)
          !vs   = VP.fromList [d | Just d <- V.toList v]
      in (pres, vs)
    presentBytes v =
      let !pres = OW.encodeBooleanRLE (V.map maybeToBool v)
          !vs   = V.fromList [b | Just b <- V.toList v]
      in (pres, vs)

    -- ORC's RLE-v2 integer encoders take (Int64 vector, signed?).
    -- Each emitter optionally prepends a PRESENT stream.
    intStreams mPres !c xs =
      presentPrefix mPres c <>
        V.singleton (streamData, c, OW.encodeIntColumn xs True)
    boolStreams mPres !c xs =
      presentPrefix mPres c <>
        V.singleton (streamData, c, OW.encodeBooleanRLE xs)
    floatStreams mPres !c xs =
      presentPrefix mPres c <>
        V.singleton (streamData, c, OW.encodeFloatColumn xs)
    doubleStreams mPres !c xs =
      presentPrefix mPres c <>
        V.singleton (streamData, c, OW.encodeDoubleColumn xs)
    stringStreams mPres !c bytesVec =
      let !(dataBs, lengthBs) = OW.encodeStringDirectColumn (V.map decodeBytesAsText bytesVec)
      in presentPrefix mPres c <>
         V.fromList
           [ (streamData,   c, dataBs)
           , (streamLength, c, lengthBs)
           ]

    presentPrefix Nothing  _ = V.empty
    presentPrefix (Just p) c = V.singleton (streamPresent, c, p)

    -- Feed the writer's text-encoder by re-tagging the raw bytes
    -- as Text. ORC's DIRECT_V2 string writer treats the Text
    -- payload as a UTF-8 bytestring under the hood; decodeUtf8
    -- with replacement keeps non-text payloads valid for the
    -- binary case.
    decodeBytesAsText bs = case TE.decodeUtf8' bs of
      Right t -> t
      Left  _ -> T.pack (map (toEnum . fromIntegral) (BS.unpack bs))

-- Type-directed signed-cast helpers (for signed Arrow integers).
signedI8 :: VP.Vector Int8 -> VP.Vector Int64
signedI8 = VP.map fromIntegral
signedI16 :: VP.Vector Int16 -> VP.Vector Int64
signedI16 = VP.map fromIntegral
signedI32 :: VP.Vector Int32 -> VP.Vector Int64
signedI32 = VP.map fromIntegral
signedI8' :: Int8 -> Int64
signedI8' = fromIntegral
signedI16' :: Int16 -> Int64
signedI16' = fromIntegral
signedI32' :: Int32 -> Int64
signedI32' = fromIntegral

-- ============================================================
-- ORC → Arrow
-- ============================================================

-- | Read a single stripe from an ORC file and lift each leaf
-- column to its Arrow shape. Requires both the parsed footer
-- (from 'ORC.HighLevel.decodeORC') and the original file bytes
-- so we can slice the stripe payload.
--
-- The Arrow schema is consulted to resolve the per-column
-- target nullability (UTF-8 vs raw binary likewise).
orcStripeToArrow
  :: AT.Schema
  -> ByteString             -- ^ the full ORC file bytes
  -> OT.ORCFooter           -- ^ pre-parsed footer (from 'ORC.HighLevel.decodeORC')
  -> Int                    -- ^ stripe index
  -> Either String (V.Vector AC.ColumnArray)
orcStripeToArrow sch fileBs footer stripeIdx = do
  -- Re-load the file via the public reader so we get the
  -- compression kind alongside the footer (the bridge doesn't
  -- decompress today, but stripeSlice / loadStripeFooter need an
  -- ORCFile handle).
  ofile <- OR.loadORCFile fileBs
  let !si = OT.orcStripes (OR.ofFooter ofile) V.! stripeIdx
      !leafFields = V.filter (V.null . AT.fieldChildren) (AT.arrowFields sch)
  stripeBytes <- OR.stripeSlice ofile stripeIdx
  stFooter    <- OR.loadStripeFooter ofile stripeIdx
  let !numRows = fromIntegral (OT.siNumberOfRows si) :: Int
      !streams = OSt.sfStreams stFooter
      _ = footer  -- kept for API stability; we re-derive the
                  -- compression-aware view via loadORCFile
  V.imapM
    (\i fld ->
        let !cid = fromIntegral (i + 1) :: Word64
        in  decodeOneColumn cid fld numRows stripeBytes streams)
    leafFields

-- | Decode one ORC column from its (kind, columnId, length)
-- stream descriptors plus the stripe data section. Walks the
-- stream descriptors to locate the @DATA@ (and @LENGTH@) byte
-- ranges for this column id, then dispatches to the appropriate
-- decoder in "ORC.Read".
decodeOneColumn
  :: Word64                      -- ^ column id
  -> AT.Field                    -- ^ Arrow target field
  -> Int                         -- ^ stripe row count
  -> ByteString                  -- ^ stripe bytes
  -> V.Vector OSt.Stream         -- ^ stream descriptors
  -> Either String AC.ColumnArray
decodeOneColumn cid fld numRows stripeBs streams = do
  -- Optionally pick up the PRESENT stream; pass to each decoder
  -- so nulls round-trip correctly.
  let mPresentBs = either (const Nothing) Just (sliceFor streamPresent)
  case AT.fieldType fld of
    AT.AInt _ True -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeIntColumn True numRows dataBs mPresentBs
      intToArrow (AT.fieldType fld) (AT.fieldNullable fld) xs
    AT.AInt _ False -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeIntColumn False numRows dataBs mPresentBs
      intToArrow (AT.fieldType fld) (AT.fieldNullable fld) xs
    AT.ABool -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeBoolColumn numRows dataBs mPresentBs
      if AT.fieldNullable fld
        then Right (AC.ColBoolMaybe xs)
        else Right (AC.ColBool (V.map (maybe False id) xs))
    AT.AFloatingPoint AT.Single -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeFloatColumn numRows dataBs mPresentBs
      if AT.fieldNullable fld
        then Right (AC.ColFloatMaybe xs)
        else Right (AC.ColFloat (VP.fromList (map (maybe 0 id) (V.toList xs))))
    AT.AFloatingPoint AT.DoublePrecision -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeDoubleColumn numRows dataBs mPresentBs
      if AT.fieldNullable fld
        then Right (AC.ColDoubleMaybe xs)
        else Right (AC.ColDouble (VP.fromList (map (maybe 0 id) (V.toList xs))))
    AT.AUtf8        -> stringColumn mPresentBs AT.AUtf8
    AT.ALargeUtf8   -> stringColumn mPresentBs AT.ALargeUtf8
    AT.ABinary      -> stringColumn mPresentBs AT.ABinary
    AT.ALargeBinary -> stringColumn mPresentBs AT.ALargeBinary
    -- Temporal types: recover the int stream at the right Arrow
    -- flavour. Date32 uses i32 days, Date64 i64, Time i32/i64,
    -- Timestamp / Duration i64.
    AT.ADate _ -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeIntColumn True numRows dataBs mPresentBs
      temporalToArrow (AT.fieldType fld) (AT.fieldNullable fld) xs
    AT.ATime _ _ -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeIntColumn True numRows dataBs mPresentBs
      temporalToArrow (AT.fieldType fld) (AT.fieldNullable fld) xs
    AT.ATimestamp _ _ -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeIntColumn True numRows dataBs mPresentBs
      temporalToArrow (AT.fieldType fld) (AT.fieldNullable fld) xs
    AT.ADuration _ -> do
      dataBs <- sliceFor streamData
      xs <- OR.decodeIntColumn True numRows dataBs mPresentBs
      temporalToArrow (AT.fieldType fld) (AT.fieldNullable fld) xs
    other ->
      Left $ "ORC.Arrow: column type " ++ show other
              ++ " not yet supported by the read bridge"
  where
    -- Lazy stream-byte helper. Accumulates stream lengths in the
    -- declared stripe order and returns the first chunk that
    -- matches (column, kind).
    sliceFor k = case V.foldl'
                   (\(off, found) s ->
                       case found of
                         Just _ -> (off, found)
                         Nothing
                           | OSt.stColumn s == cid && OSt.stKind s == k ->
                               (off, Just (off, OSt.stLength s))
                           | otherwise ->
                               (off + OSt.stLength s, Nothing))
                   (0 :: Word64, Nothing) streams of
                   (_, Just (off, len)) ->
                     Right (BS.take (fromIntegral len)
                                    (BS.drop (fromIntegral off) stripeBs))
                   (_, Nothing) ->
                     Left $ "ORC.Arrow: column " ++ show cid
                              ++ " missing stream kind " ++ show k

    stringColumn mPresentBs ty = do
      dataBs   <- sliceFor streamData
      lengthBs <- sliceFor streamLength
      xs <- OR.decodeStringColumn numRows dataBs lengthBs BS.empty mPresentBs
      let !decoded = case ty of
            AT.ABinary       -> AC.ColBinary
                                  (V.map (maybe BS.empty TE.encodeUtf8) xs)
            AT.ALargeBinary  -> AC.ColLargeBinary
                                  (V.map (maybe BS.empty TE.encodeUtf8) xs)
            AT.ALargeUtf8    -> AC.ColLargeUtf8
                                  (V.map (maybe T.empty id) xs)
            _                -> AC.ColUtf8
                                  (V.map (maybe T.empty id) xs)
      if AT.fieldNullable fld
        then Right $ case ty of
               AT.ABinary       -> AC.ColBinaryMaybe (V.map (fmap TE.encodeUtf8) xs)
               AT.ALargeBinary  -> AC.ColLargeBinaryMaybe (V.map (fmap TE.encodeUtf8) xs)
               AT.ALargeUtf8    -> AC.ColLargeUtf8Maybe xs
               _                -> AC.ColUtf8Maybe xs
        else Right decoded

-- | Cast a @V.Vector (Maybe Int64)@ stream to the right Arrow
-- column flavour. ORC ints use a single Int64-backed RLE-v2
-- representation; we narrow back to the requested Arrow width
-- here.
intToArrow
  :: AT.ArrowType -> Bool -> V.Vector (Maybe Int64)
  -> Either String AC.ColumnArray
intToArrow ty nullable xs = case (ty, nullable) of
  (AT.AInt 8  True,  False) -> Right $! AC.ColInt8   (VP.fromList (map narrow8  (presentValues xs)))
  (AT.AInt 16 True,  False) -> Right $! AC.ColInt16  (VP.fromList (map narrow16 (presentValues xs)))
  (AT.AInt 32 True,  False) -> Right $! AC.ColInt32  (VP.fromList (map narrow32 (presentValues xs)))
  (AT.AInt 64 True,  False) -> Right $! AC.ColInt64  (VP.fromList (presentValues xs))
  (AT.AInt 8  False, False) -> Right $! AC.ColUInt8  (VP.fromList (map fromIntegral (presentValues xs)))
  (AT.AInt 16 False, False) -> Right $! AC.ColUInt16 (VP.fromList (map fromIntegral (presentValues xs)))
  (AT.AInt 32 False, False) -> Right $! AC.ColUInt32 (VP.fromList (map fromIntegral (presentValues xs)))
  (AT.AInt 64 False, False) -> Right $! AC.ColUInt64 (VP.fromList (map fromIntegral (presentValues xs)))
  (AT.AInt 8  True,  True)  -> Right $! AC.ColInt8Maybe   (V.map (fmap narrow8)  xs)
  (AT.AInt 16 True,  True)  -> Right $! AC.ColInt16Maybe  (V.map (fmap narrow16) xs)
  (AT.AInt 32 True,  True)  -> Right $! AC.ColInt32Maybe  (V.map (fmap narrow32) xs)
  (AT.AInt 64 True,  True)  -> Right $! AC.ColInt64Maybe  xs
  (AT.AInt 8  False, True)  -> Right $! AC.ColUInt8Maybe  (V.map (fmap fromIntegral) xs)
  (AT.AInt 16 False, True)  -> Right $! AC.ColUInt16Maybe (V.map (fmap fromIntegral) xs)
  (AT.AInt 32 False, True)  -> Right $! AC.ColUInt32Maybe (V.map (fmap fromIntegral) xs)
  (AT.AInt 64 False, True)  -> Right $! AC.ColUInt64Maybe (V.map (fmap fromIntegral) xs)
  _ -> Left $ "ORC.Arrow: unexpected type/null combo " ++ show (ty, nullable)
  where
    narrow8 :: Int64 -> Int8
    narrow8 = fromIntegral
    narrow16 :: Int64 -> Int16
    narrow16 = fromIntegral
    narrow32 :: Int64 -> Int32
    narrow32 = fromIntegral

presentValues :: V.Vector (Maybe a) -> [a]
presentValues v = [x | Just x <- V.toList v]

-- | Lift a decoded ORC integer stream into one of Arrow's temporal
-- column shapes. Width narrowing happens here (Date32 / Time32
-- use Int32 under the hood).
temporalToArrow
  :: AT.ArrowType -> Bool -> V.Vector (Maybe Int64)
  -> Either String AC.ColumnArray
temporalToArrow ty nullable xs = case (ty, nullable) of
  (AT.ADate AT.DateDay, False) ->
    Right $! AC.ColDate32 (VP.fromList (map narrow32 (presentValues xs)))
  (AT.ADate AT.DateMillisecond, False) ->
    Right $! AC.ColDate64 (VP.fromList (presentValues xs))
  (AT.ATime _ 32, False) ->
    Right $! AC.ColTime32 (VP.fromList (map narrow32 (presentValues xs)))
  (AT.ATime _ 64, False) ->
    Right $! AC.ColTime64 (VP.fromList (presentValues xs))
  (AT.ATimestamp _ _, False) ->
    Right $! AC.ColTimestamp (VP.fromList (presentValues xs))
  (AT.ADuration _, False) ->
    Right $! AC.ColDuration (VP.fromList (presentValues xs))

  (AT.ADate AT.DateDay, True) ->
    Right $! AC.ColDate32Maybe (V.map (fmap narrow32) xs)
  (AT.ADate AT.DateMillisecond, True) ->
    Right $! AC.ColDate64Maybe xs
  (AT.ATime _ 32, True) ->
    Right $! AC.ColTime32Maybe (V.map (fmap narrow32) xs)
  (AT.ATime _ 64, True) ->
    Right $! AC.ColTime64Maybe xs
  (AT.ATimestamp _ _, True) ->
    Right $! AC.ColTimestampMaybe xs
  (AT.ADuration _, True) ->
    Right $! AC.ColDurationMaybe xs

  _ -> Left $ "ORC.Arrow.temporalToArrow: unexpected type/null combo "
                ++ show (ty, nullable)
  where
    narrow32 :: Int64 -> Int32
    narrow32 = fromIntegral

-- ============================================================
-- Legacy / deprecated variants
-- ============================================================
--
-- Before the consolidation, there were two Arrow → ORC entry
-- points: 'arrowToORC' (row-count-free) and 'arrowToORCWithRows'
-- (row-count-paired). The row-count-paired shape is now the
-- canonical one; these aliases keep old call sites compiling.

{-# DEPRECATED arrowToORCWithRows
    "Use 'arrowToORC' — it now returns per-stripe row counts as the canonical shape." #-}
arrowToORCWithRows
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String ( V.Vector OT.ORCType
                   , [(V.Vector (Word64, Word64, ByteString), Word64)]
                   )
arrowToORCWithRows = arrowToORC

{-# DEPRECATED arrowToORCWithoutRows
    "Use 'arrowToORC' then 'map fst' if you really only want the stream tuples." #-}
arrowToORCWithoutRows
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String ( V.Vector OT.ORCType
                   , [V.Vector (Word64, Word64, ByteString)]
                   )
arrowToORCWithoutRows sch batches = do
  (types, withRows) <- arrowToORC sch batches
  Right (types, map fst withRows)
