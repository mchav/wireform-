{-# LANGUAGE OverloadedStrings #-}

-- | Common functionality for working with Protobuf
module Network.GRPC.Common.Protobuf (
    Protobuf
  , Proto(..)
  , getProto

    -- * Exceptions
  , ProtobufError(..)
  , throwProtobufError
  , throwProtobufErrorHom
  , toProtobufError
  , toProtobufErrorHom

    -- * Re-exports
    -- ** "Data.Function"
  , (&)
    -- ** "Proto.Lens"
  , (.~)
  , (^.)
  , (%~)
    -- ** "Proto.Schema" / "Proto.Registry"
  , StreamingType(..)
  , Proto.Schema.HasField(..)
  , IsMessage
  , ProtoMessage(..)
  ) where

import Network.GRPC.Util.Imports

import Control.Monad ((<=<))
import Control.Monad.Except (throwError)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Int (Int32)
import Data.Text qualified as T

import Proto.Lens ((.~), (^.), (%~))
import Proto.Schema (ProtoMessage(..))
import Proto.Schema qualified
import Proto.Registry (IsMessage)
import Proto.Encode
import Proto.Decode
import Proto.Internal.Wire (Tag(..), WireType(..))
import Proto.Internal.Wire.Encode (putTag, putVarint, varintSize, tagSize, fieldTextSize, fieldBytesSize, fieldMessageSize)

import Network.GRPC.Common.Protobuf.Any (Any)
import Network.GRPC.Common.Protobuf.Any qualified as Any

import Proto.Google.Protobuf.Any qualified as PbAny

{-------------------------------------------------------------------------------
  Wire-compatible google.rpc.Status using wireform encoding/decoding.

  google.rpc.Status has:
    int32 code = 1;
    string message = 2;
    repeated google.protobuf.Any details = 3;
-------------------------------------------------------------------------------}

data RpcStatus = RpcStatus
  { rsCode    :: !Int32
  , rsMessage :: !Text
  , rsDetails :: ![PbAny.Any]
  }

instance MessageEncode RpcStatus where
  buildMessage msg =
       (if msg_code == 0 then mempty
        else encodeFieldVarint 1 (fromIntegral msg_code))
    <> (if T.null msg_message then mempty
        else encodeFieldString 2 msg_message)
    <> mconcat [ encodeFieldMessage 3 d | d <- msg_details ]
    where
      msg_code    = rsCode msg
      msg_message = rsMessage msg
      msg_details = rsDetails msg

instance MessageSize RpcStatus where
  messageSize msg =
       (if msg_code == 0 then 0
        else tagSize 1 + varintSize (fromIntegral msg_code))
    + (if T.null msg_message then 0
        else fieldTextSize 2 msg_message)
    + sum [ fieldMessageSize 3 (messageSize d) | d <- msg_details ]
    where
      msg_code    = rsCode msg
      msg_message = rsMessage msg
      msg_details = rsDetails msg

instance MessageDecode RpcStatus where
  {-# INLINE messageDecoder #-}
  messageDecoder = loop 0 "" []
    where
      loop !acc_code !acc_msg !acc_details = do
        mTag <- getTagOrU
        case mTag of
          UNothing -> pure (RpcStatus acc_code acc_msg (reverse acc_details))
          UJust (Tag fn wt) -> case fn of
            1 -> do
              v <- decodeFieldVarint
              loop (fromIntegral v) acc_msg acc_details
            2 -> do
              v <- decodeFieldString
              loop acc_code v acc_details
            3 -> do
              v <- decodeFieldMessage
              loop acc_code acc_msg (v : acc_details)
            _ -> do
              _uf <- captureUnknownField fn wt
              loop acc_code acc_msg acc_details

encodeRpcStatus :: RpcStatus -> BS.ByteString
encodeRpcStatus = encodeMessage

decodeRpcStatus :: BS.ByteString -> Either String RpcStatus
decodeRpcStatus bs = case decodeMessage bs of
  Left err -> Left (show err)
  Right s  -> Right s

{-------------------------------------------------------------------------------
  Protobuf-specific errors
-------------------------------------------------------------------------------}

-- | gRPC exception with protobuf-specific error details
--
-- See also @google.rpc.Status@.
data ProtobufError a = ProtobufError {
      protobufErrorCode     :: GrpcError
    , protobufErrorMessage  :: Maybe Text
    , protobufErrorDetails  :: [a]
    }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Throw 'GrpcException' with Protobuf-specific details
throwProtobufError :: ProtobufError Any -> IO x
throwProtobufError ProtobufError{
                       protobufErrorCode
                     , protobufErrorMessage
                     , protobufErrorDetails
                     } = throwIO $ GrpcException {
      grpcError         = protobufErrorCode
    , grpcErrorMessage  = protobufErrorMessage
    , grpcErrorDetails  = Just $ encodeRpcStatus status
    , grpcErrorMetadata = []
    }
  where
    status :: RpcStatus
    status = RpcStatus
      { rsCode    = fromIntegral (fromGrpcError protobufErrorCode)
      , rsMessage = fromMaybe "" protobufErrorMessage
      , rsDetails = protobufErrorDetails
      }

-- | Variation of 'throwProtobufError' for a homogenous list of details
--
-- The @google.rpc.Status@ message uses the Protobuf 'Any' type to store a
-- heterogenous list of details. In case that all elements in this list are
-- actually of the /same/ type, we can provide a simpler API.
throwProtobufErrorHom :: IsMessage a => ProtobufError a -> IO x
throwProtobufErrorHom = throwProtobufError . fmap Any.pack

-- | Construct 'ProtobufError' by parsing 'grpcErrorDetails' as 'Status'
--
-- See also 'throwProtobufError'.
toProtobufError :: GrpcException -> Either String (ProtobufError Any)
toProtobufError err =
    case grpcErrorDetails err of
      Nothing ->
        return ProtobufError{
            protobufErrorCode    = grpcError err
          , protobufErrorMessage = grpcErrorMessage err
          , protobufErrorDetails = []
          }
      Just statusEnc -> do
        status <- decodeRpcStatus statusEnc
        protobufErrorCode <- checkErrorCode (rsCode status)
        return ProtobufError{
            protobufErrorCode
          , protobufErrorMessage = constructErrorMessage (rsMessage status)
          , protobufErrorDetails = rsDetails status
          }
  where
    checkErrorCode :: Int32 -> Either String GrpcError
    checkErrorCode statusCode
      | statusCode == 0
      = return $ grpcError err

      | fromGrpcError (grpcError err) == fromIntegral statusCode
      = return $ grpcError err

      | otherwise
      = throwError $ "'Status.code' does not match 'grpc-status'"

    constructErrorMessage :: Text -> Maybe Text
    constructErrorMessage msg =
        if T.null msg
          then grpcErrorMessage err
          else Just msg

-- | Variation of 'toProtobufError' for a homogenous list of details
toProtobufErrorHom :: forall a.
     IsMessage a
  => GrpcException -> Either String (ProtobufError a)
toProtobufErrorHom = traverse aux <=< toProtobufError
  where
    aux :: Any -> Either String a
    aux = first show . Any.unpack
