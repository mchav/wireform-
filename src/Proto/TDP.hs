{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | Table-driven protobuf parser, inspired by hyperpb's TDP VM.
--
-- Instead of generating Haskell source code specialized per message type,
-- this module compiles a 'ProtoMessage' schema into a flat array of
-- 'FieldParser' entries that a small interpreter loop evaluates against
-- incoming wire data.
--
-- Key properties (matching hyperpb's design):
--
-- * No per-type instruction cache pressure: the interpreter is a single
--   small function shared across all message types.
-- * Field-order scheduling: 'fpNextOk' / 'fpNextErr' form a linked
--   list through the array, predicting the next field in declaration
--   order.
-- * TagLUT: single-byte tags (field numbers 1-15 with common wire types)
--   index directly into a 128-byte lookup table, bypassing the hash
--   table entirely.
-- * Thunk dispatch: each field has a function pointer ('fpParse') for
--   its specific decode operation. The CPU's indirect branch predictor
--   handles this well because most messages use only a handful of
--   distinct field archetypes.
module Proto.TDP
  ( -- * Parse table types
    ParseTable (..)
  , FieldParser (..)
  , FieldThunk

    -- * Compilation
  , compileParseTable

    -- * Execution
  , runParseTable

    -- * Dynamic message representation
  , TDPMessage (..)
  , TDPValue (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Text (Text)
import Data.Int (Int64)
import Data.Word (Word8, Word32, Word64)
import qualified Data.Vector as V
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.Encoding as TE

import Proto.Wire (WireType(..), fieldTag)
import Proto.Wire.Decode
  ( Decoder, runDecoder, runDecoder'
  , DecodeResult(..), DecodeError(..)
  , getVarint, getFixed32, getFixed64, getText, getLengthDelimited
  , skipField, validateUtf8
  )
import Proto.Schema
  ( ProtoMessage(..), SomeFieldDescriptor(..), FieldDescriptor(..)
  , FieldTypeDescriptor(..), ScalarFieldType(..), FieldLabel'(..)
  )
import Data.Proxy (Proxy(..))

-- | A decoded value from the TDP parser.
data TDPValue
  = TVVarint   {-# UNPACK #-} !Word64
  | TVSVarint  {-# UNPACK #-} !Int64
  | TVFixed32  {-# UNPACK #-} !Word32
  | TVFixed64  {-# UNPACK #-} !Word64
  | TVFloat    {-# UNPACK #-} !Float
  | TVDouble   {-# UNPACK #-} !Double
  | TVBool     !Bool
  | TVString   !Text
  | TVBytes    !ByteString
  | TVMessage  !TDPMessage
  | TVEnum     {-# UNPACK #-} !Int
  | TVRepeated !(V.Vector TDPValue)
  deriving stock (Show, Eq)

-- | Dynamic message decoded by the TDP VM.
data TDPMessage = TDPMessage
  { tdpFields  :: !(IntMap TDPValue)
  , tdpUnknown :: !ByteString
  } deriving stock (Show, Eq)

-- | A thunk for parsing a single field. Takes a ByteString and offset,
-- returns the parsed value and new offset.
type FieldThunk = ByteString -> Int -> IO (TDPValue, Int)

-- | A single field's parse configuration in the table.
data FieldParser = FieldParser
  { fpTag       :: {-# UNPACK #-} !Word64
    -- ^ The wire tag (field number << 3 | wire type) this entry matches.
  , fpFieldNum  :: {-# UNPACK #-} !Int
    -- ^ Proto field number.
  , fpNextOk    :: {-# UNPACK #-} !Int
    -- ^ Index of next FieldParser to try on successful match (field scheduling).
  , fpNextErr   :: {-# UNPACK #-} !Int
    -- ^ Index of next FieldParser to try on mismatch.
  , fpParse     :: !FieldThunk
    -- ^ The thunk that actually decodes this field's value.
  , fpLabel     :: !FieldLabel'
    -- ^ Whether this field is repeated (affects accumulation).
  , fpSubmsg    :: !(Maybe ParseTable)
    -- ^ Nested parse table for submessage fields.
  }

-- | Compiled parse table for a message type.
data ParseTable = ParseTable
  { ptFields   :: !(V.Vector FieldParser)
    -- ^ Field parsers in scheduled order.
  , ptTagLUT   :: !ByteString
    -- ^ 128-byte LUT: tag byte -> index in ptFields (0xFF = miss).
    -- For tags < 128 (field numbers 1-15), this is a direct O(1) lookup.
  , ptTagMap   :: !(IntMap Int)
    -- ^ Fallback map: wire tag -> index in ptFields, for tags >= 128.
  , ptMaxMiss  :: {-# UNPACK #-} !Int
    -- ^ Max consecutive misses before hitting the hash table.
  }

-- | Compile a 'ProtoMessage' schema into a 'ParseTable'.
compileParseTable :: forall a. ProtoMessage a => Proxy a -> ParseTable
compileParseTable proxy =
  let descriptors = protoFieldDescriptors proxy
      fieldList' = Map.toAscList descriptors
      nFields = length fieldList'

      parsers = V.fromList
        [ makeFieldParser i nFields fd
        | (i, (_, SomeField fd)) <- zip [0..] fieldList'
        ]

      tagLUTBytes = BS.pack (fmap tagLUTEntry [0..127])
      tagMap = IntMap.fromList
        [ (fromIntegral (fpTag fp), i)
        | (i, fp) <- zip [0..] (V.toList parsers)
        ]
  in ParseTable
    { ptFields  = parsers
    , ptTagLUT  = tagLUTBytes
    , ptTagMap  = tagMap
    , ptMaxMiss = min 4 nFields
    }
  where
    fieldList = Map.toAscList (protoFieldDescriptors proxy)
    nFields = length fieldList

    fpList :: [(Int, FieldParser)]
    fpList =
      [ (i, makeFieldParser i nFields fd)
      | (i, (_, SomeField fd)) <- zip [0..] fieldList
      ]

    tagLUTEntry :: Word8 -> Word8
    tagLUTEntry tag =
      case IntMap.lookup (fromIntegral tag) tagIdxMap of
        Just idx | idx < 256 -> fromIntegral idx
        _ -> 0xFF

    tagIdxMap :: IntMap Int
    tagIdxMap = IntMap.fromList
      [ (fromIntegral (fpTag (snd fp)), fst fp)
      | fp <- fpList
      ]

-- | Build a FieldParser from a schema FieldDescriptor.
makeFieldParser :: Int -> Int -> FieldDescriptor msg a -> FieldParser
makeFieldParser idx nFields fd =
  let fn = fdNumber fd
      wt = fieldWireType (fdTypeDesc fd)
      tag = fieldTag fn wt
      nextOk = (idx + 1) `mod` nFields
      nextErr = (idx + 1) `mod` nFields
  in FieldParser
    { fpTag      = tag
    , fpFieldNum = fn
    , fpNextOk   = nextOk
    , fpNextErr  = nextErr
    , fpParse    = makeThunk (fdTypeDesc fd)
    , fpLabel    = fdLabel fd
    , fpSubmsg   = Nothing
    }

fieldWireType :: FieldTypeDescriptor -> WireType
fieldWireType = \case
  ScalarType DoubleField   -> Wire64Bit
  ScalarType FloatField    -> Wire32Bit
  ScalarType Int32Field    -> WireVarint
  ScalarType Int64Field    -> WireVarint
  ScalarType UInt32Field   -> WireVarint
  ScalarType UInt64Field   -> WireVarint
  ScalarType SInt32Field   -> WireVarint
  ScalarType SInt64Field   -> WireVarint
  ScalarType Fixed32Field  -> Wire32Bit
  ScalarType Fixed64Field  -> Wire64Bit
  ScalarType SFixed32Field -> Wire32Bit
  ScalarType SFixed64Field -> Wire64Bit
  ScalarType BoolField     -> WireVarint
  ScalarType StringField   -> WireLengthDelimited
  ScalarType BytesField    -> WireLengthDelimited
  MessageType _            -> WireLengthDelimited
  EnumType _               -> WireVarint
  MapType _ _              -> WireLengthDelimited

-- | Make a decode thunk for a field type.
makeThunk :: FieldTypeDescriptor -> FieldThunk
makeThunk = \case
  ScalarType DoubleField   -> thunkFixed64 (TVDouble . castWord64ToDouble)
  ScalarType FloatField    -> thunkFixed32 (TVFloat . castWord32ToFloat)
  ScalarType Int32Field    -> thunkVarint (TVVarint)
  ScalarType Int64Field    -> thunkVarint (TVVarint)
  ScalarType UInt32Field   -> thunkVarint (TVVarint)
  ScalarType UInt64Field   -> thunkVarint (TVVarint)
  ScalarType SInt32Field   -> thunkVarint (TVVarint)
  ScalarType SInt64Field   -> thunkVarint (TVVarint)
  ScalarType Fixed32Field  -> thunkFixed32 (TVFixed32)
  ScalarType Fixed64Field  -> thunkFixed64 (TVFixed64)
  ScalarType SFixed32Field -> thunkFixed32 (TVFixed32)
  ScalarType SFixed64Field -> thunkFixed64 (TVFixed64)
  ScalarType BoolField     -> thunkVarint (\v -> TVBool (v /= 0))
  ScalarType StringField   -> thunkLenDelim (\bs -> TVString <$> decodeText bs)
  ScalarType BytesField    -> thunkLenDelim (\bs -> Right (TVBytes bs))
  MessageType _            -> thunkLenDelim (\bs -> TVMessage <$> decodeTDPMessage bs)
  EnumType _               -> thunkVarint (TVEnum . fromIntegral)
  MapType _ _              -> thunkLenDelim (\bs -> Right (TVBytes bs))

decodeText :: ByteString -> Either DecodeError Text
decodeText bs
  | validateUtf8 bs = Right (TE.decodeUtf8Lenient bs)
  | otherwise = Left InvalidUtf8

-- Thunk builders: decode a wire value at a given offset in a ByteString.

thunkVarint :: (Word64 -> TDPValue) -> FieldThunk
thunkVarint f bs off =
  case runDecoder' getVarint bs off of
    DecodeOK v off' -> pure (f v, off')
    DecodeFail e    -> error ("TDP varint decode: " <> show e)

thunkFixed32 :: (Word32 -> TDPValue) -> FieldThunk
thunkFixed32 f bs off =
  case runDecoder' getFixed32 bs off of
    DecodeOK v off' -> pure (f v, off')
    DecodeFail e    -> error ("TDP fixed32 decode: " <> show e)

thunkFixed64 :: (Word64 -> TDPValue) -> FieldThunk
thunkFixed64 f bs off =
  case runDecoder' getFixed64 bs off of
    DecodeOK v off' -> pure (f v, off')
    DecodeFail e    -> error ("TDP fixed64 decode: " <> show e)

thunkLenDelim :: (ByteString -> Either DecodeError TDPValue) -> FieldThunk
thunkLenDelim f bs off =
  case runDecoder' getLengthDelimited bs off of
    DecodeOK bytes off' -> case f bytes of
      Right v  -> pure (v, off')
      Left e   -> error ("TDP len-delim decode: " <> show e)
    DecodeFail e -> error ("TDP len-delim decode: " <> show e)

-- | Decode a submessage using the TDP VM (recursive).
decodeTDPMessage :: ByteString -> Either DecodeError TDPMessage
decodeTDPMessage bs = Right $ runParseTableRaw bs

-- | Run a parse table on raw bytes (without a compiled table — wire-type inference).
runParseTableRaw :: ByteString -> TDPMessage
runParseTableRaw bs = unsafePerformIO $ do
  fieldsRef <- newIORef IntMap.empty
  let len = BS.length bs
      go !off
        | off >= len = pure ()
        | otherwise = do
            case runDecoder' getVarint bs off of
              DecodeOK tagW off1 -> do
                let !fn = fromIntegral (tagW `div` 8) :: Int
                    !wt = fromIntegral (tagW `mod` 8) :: Int
                case wt of
                  0 -> case runDecoder' getVarint bs off1 of
                    DecodeOK v off2 -> do
                      modifyIORef' fieldsRef (IntMap.insert fn (TVVarint v))
                      go off2
                    DecodeFail _ -> pure ()
                  1 -> case runDecoder' getFixed64 bs off1 of
                    DecodeOK v off2 -> do
                      modifyIORef' fieldsRef (IntMap.insert fn (TVFixed64 v))
                      go off2
                    DecodeFail _ -> pure ()
                  2 -> case runDecoder' getLengthDelimited bs off1 of
                    DecodeOK v off2 -> do
                      modifyIORef' fieldsRef (IntMap.insert fn (TVBytes v))
                      go off2
                    DecodeFail _ -> pure ()
                  5 -> case runDecoder' getFixed32 bs off1 of
                    DecodeOK v off2 -> do
                      modifyIORef' fieldsRef (IntMap.insert fn (TVFixed32 v))
                      go off2
                    DecodeFail _ -> pure ()
                  _ -> pure ()
              DecodeFail _ -> pure ()
  go 0
  fields <- readIORef fieldsRef
  pure (TDPMessage fields BS.empty)

-- | The core TDP interpreter loop.
--
-- This is the Haskell equivalent of hyperpb's @loop@ function in @vm/run.go@.
-- It evaluates a compiled 'ParseTable' against wire data:
--
-- 1. Decode a tag varint.
-- 2. If tag < 128, use the TagLUT for O(1) field lookup.
-- 3. Otherwise, try the predicted next field ('fpNextOk').
-- 4. On mismatch, walk 'fpNextErr' up to 'ptMaxMiss' times.
-- 5. Fall back to the tag hash map.
-- 6. Call the matched field's thunk ('fpParse') to decode the value.
-- 7. Store the value in the accumulator.
-- 8. Repeat until end of input.
runParseTable :: ParseTable -> ByteString -> Either DecodeError TDPMessage
runParseTable pt bs
  | BS.null bs = Right (TDPMessage IntMap.empty BS.empty)
  | V.null (ptFields pt) = Right (runParseTableRaw bs)
  | otherwise = unsafePerformIO $ do
      fieldsRef <- newIORef IntMap.empty
      let len = BS.length bs
          nFields = V.length (ptFields pt)
          go !off !curIdx
            | off >= len = pure ()
            | otherwise = do
                case runDecoder' getVarint bs off of
                  DecodeOK tagW off1 -> do
                    let !tagInt = fromIntegral tagW :: Int
                    mIdx <- findField pt tagW tagInt curIdx
                    case mIdx of
                      Just idx -> do
                        let !fp = ptFields pt V.! idx
                        (val, off2) <- fpParse fp bs off1
                        case fpLabel fp of
                          LabelRepeated -> do
                            fields <- readIORef fieldsRef
                            let fn = fpFieldNum fp
                                val' = case IntMap.lookup fn fields of
                                  Just (TVRepeated vs) -> TVRepeated (V.snoc vs val)
                                  Just existing -> TVRepeated (V.fromList [existing, val])
                                  Nothing -> val
                            writeIORef fieldsRef (IntMap.insert fn val' fields)
                          _ ->
                            modifyIORef' fieldsRef (IntMap.insert (fpFieldNum fp) val)
                        go off2 (fpNextOk fp)
                      Nothing -> do
                        -- Unknown field: skip it
                        let !wt = fromIntegral (tagW `mod` 8) :: Int
                        case skipWireValue wt bs off1 of
                          Just off2 -> go off2 curIdx
                          Nothing   -> pure ()
                  DecodeFail _ -> pure ()
      go 0 0
      fields <- readIORef fieldsRef
      pure (Right (TDPMessage fields BS.empty))

-- | Find the matching field parser for a given tag.
findField :: ParseTable -> Word64 -> Int -> Int -> IO (Maybe Int)
findField pt tagW tagInt curIdx
  -- TagLUT fast path: single-byte tags (field numbers 1-15)
  | tagInt >= 0, tagInt < 128 =
      let !lutVal = BSU.unsafeIndex (ptTagLUT pt) tagInt
      in if lutVal /= 0xFF
         then pure (Just (fromIntegral lutVal))
         else pure Nothing
  -- Predicted next field
  | curIdx < V.length (ptFields pt)
  , let fp = ptFields pt V.! curIdx
  , fpTag fp == tagW =
      pure (Just curIdx)
  -- Walk NextErr chain
  | otherwise = walkErr pt tagW curIdx (ptMaxMiss pt)

walkErr :: ParseTable -> Word64 -> Int -> Int -> IO (Maybe Int)
walkErr pt tagW curIdx !tries
  | tries <= 0 = pure (IntMap.lookup (fromIntegral tagW) (ptTagMap pt))
  | curIdx >= V.length (ptFields pt) = pure (IntMap.lookup (fromIntegral tagW) (ptTagMap pt))
  | otherwise =
      let !fp = ptFields pt V.! curIdx
      in if fpTag fp == tagW
         then pure (Just curIdx)
         else walkErr pt tagW (fpNextErr fp) (tries - 1)

-- | Skip a wire value based on wire type. Returns new offset or Nothing.
skipWireValue :: Int -> ByteString -> Int -> Maybe Int
skipWireValue wt bs off = case wt of
  0 -> case runDecoder' getVarint bs off of
    DecodeOK _ off' -> Just off'
    DecodeFail _    -> Nothing
  1 -> let off' = off + 8 in if off' <= BS.length bs then Just off' else Nothing
  2 -> case runDecoder' getVarint bs off of
    DecodeOK lenW off' ->
      let off'' = off' + fromIntegral lenW
      in if off'' <= BS.length bs then Just off'' else Nothing
    DecodeFail _ -> Nothing
  5 -> let off' = off + 4 in if off' <= BS.length bs then Just off' else Nothing
  _ -> Nothing
