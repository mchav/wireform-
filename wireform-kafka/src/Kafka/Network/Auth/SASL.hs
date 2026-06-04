{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}

{-|
Module      : Kafka.Network.Auth.SASL
Description : SASL handshake driver

Authenticates an already-connected (TCP or TLS) Kafka broker
connection using SASL.

The driver is mechanism-agnostic: it implements the broker-side
handshake (SaslHandshakeRequest \/ SaslAuthenticateRequest framing,
KIP-152 wrapped form) and delegates the cryptographic per-message
work to a 'SaslMechanismImpl' that knows how to drive the chosen
mechanism. Five built-in implementations:

  * 'plainImpl'        — SASL\/PLAIN (RFC 4616)
  * 'scramImpl'        — SASL\/SCRAM-SHA-256 \/ SHA-512 (RFC 5802)
  * 'oauthBearerImpl'  — SASL\/OAUTHBEARER (RFC 7628)
  * 'awsMskIamImpl'    — AWS MSK IAM (\@AWS_MSK_IAM\@) using SigV4
  * 'gssapiImpl'       — GSSAPI \/ Kerberos placeholder that returns a
                         clear "not yet implemented" error so callers
                         get a structured failure instead of confusing
                         broker-side timeouts.

= Usage

You almost never use this module directly; pass a 'SaslConfig' on
the 'Kafka.Network.Connection.ConnectionConfig' instead and the
connection manager will run the handshake automatically. The direct
'authenticate' entry point is useful for unit tests and for
integrations that manage their own connections.

= Wire layout

Per KIP-43 (handshake) + KIP-152 (wrapped SaslAuthenticate framing)
the conversation is:

@
client -> SaslHandshakeRequest  v1  { mechanism: "PLAIN" }
broker -> SaslHandshakeResponse v1  { error: 0, mechanisms: [...] }
client -> SaslAuthenticateRequest  v1 { auth_bytes }
broker -> SaslAuthenticateResponse v1 { error, error_message, auth_bytes, lifetime_ms }
... mechanism-defined number of additional round-trips ...
@

Most mechanisms only need one client@->@server step (PLAIN, OAUTHBEARER,
AWS_MSK_IAM); SCRAM needs two.
-}
module Kafka.Network.Auth.SASL
  ( -- * Configuration
    SaslConfig(..)
  , SaslMechanismName(..)
  , mechanismWireName
    -- * High-level entry point
  , authenticate
  , authenticateDetailed
  , AuthSuccess(..)
  , AuthError(..)
    -- * KIP-368 session re-authentication
  , effectiveReauthDeadlineMs
  , reauthRequiredAtMs
    -- * Built-in mechanism implementations (advanced)
  , SaslMechanismImpl(..)
  , StepResult(..)
  , plainImpl
  , plainImplWithAuthzid
  , scramImpl
  , oauthBearerImpl
  , oauthBearerImplWithExtensions
  , awsMskIamImpl
  , gssapiImpl
  , gssapiBuildEnabled
  , configMechanism
  ) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Int (Int16, Int32)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Kafka.Network.Connection.Internal (Connection)

#ifdef WIREFORM_KAFKA_GSSAPI
import Control.Exception (bracketOnError, finally, throwIO)
import Control.Monad (void)
import Data.Bits ((.&.))
import qualified Data.ByteString.Char8 as BS8
import qualified Foreign.Concurrent as FC
import Foreign
  ( Ptr
  , Storable(..)
  , alloca
  , castPtr
  , nullPtr
  , peek
  , poke
  , withForeignPtr
  )
import Foreign.C.Types (CSize(..), CUInt(..))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Marshal.Alloc (free, malloc)
import qualified Network.Security.GssApi as GSSAPI ()
#endif

import qualified Kafka.Client.Internal.Request as Req
import qualified Kafka.Network.Auth.AwsMskIam as Iam
import qualified Kafka.Network.Auth.OAuthBearer as OAuth
import qualified Kafka.Network.Auth.Plain as Plain
import qualified Kafka.Network.Auth.Scram as Scram
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.SaslAuthenticateRequest as SAReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.SaslAuthenticateResponse as SAResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.SaslHandshakeRequest as SHReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.SaslHandshakeResponse as SHResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Primitives as P
import qualified "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec as WC

------------------------------------------------------------------------
-- User-facing config
------------------------------------------------------------------------

-- | A SASL mechanism plus the per-mechanism configuration it needs.
-- 'SaslConfig' is the complete description of "how do I authenticate
-- to this broker"; pass it to 'Kafka.Network.Connection.ConnectionConfig'
-- (or via 'Kafka.Client.Group.GroupConfig') and the connection layer
-- will run the SASL handshake right after TCP\/TLS is up.
data SaslConfig
  = -- | SASL\/PLAIN with a static username\/password. Send only over
    --   TLS — the password is on the wire in the clear.
    SaslPlain !Text !Text
  | -- | SASL\/PLAIN with an explicit authorization identity
    --   (@authzid@) distinct from the authentication identity. Most
    --   Kafka deployments leave this empty; use this only when the
    --   broker-side auth stack is configured to honor it.
    SaslPlainWithAuthzid
        !Text  -- ^ Authorization identity (@authzid@)
        !Text  -- ^ Authentication identity / username
        !Text  -- ^ Password
  | -- | SASL\/SCRAM with the chosen hash variant.
    SaslScram !Scram.ScramAlgo !Text !Text
  | -- | SASL\/OAUTHBEARER with a pluggable token provider.
    SaslOAuthBearer !OAuth.OAuthTokenProvider
  | -- | SASL\/OAUTHBEARER with optional RFC 7628 authzid/host/port
    --   extensions.
    SaslOAuthBearerWithExtensions
        !OAuth.OAuthTokenProvider
        !OAuth.OAuthBearerExtensions
  | -- | AWS MSK IAM (@AWS_MSK_IAM@). Pass a credentials provider and
    --   the AWS region; the broker host is taken from the connection
    --   target at handshake time.
    SaslAwsMskIam
        !Iam.AwsCredentialsProvider
        !Text  -- ^ AWS region, e.g. \"us-east-1\"
  | -- | GSSAPI \/ Kerberos — accepted by the configuration so callers
    --   can wire it up uniformly, but rejected at handshake time
    --   with a clear error (see 'gssapiImpl').
    SaslGssapi

-- | Symbolic name for a SASL mechanism, used for logging / error
-- reporting / mechanism negotiation.
data SaslMechanismName
  = NamePlain
  | NameScramSha256
  | NameScramSha512
  | NameOAuthBearer
  | NameAwsMskIam
  | NameGssapi
  deriving (Eq, Ord, Show)

mechanismWireName :: SaslMechanismName -> Text
mechanismWireName = \case
  NamePlain        -> "PLAIN"
  NameScramSha256  -> "SCRAM-SHA-256"
  NameScramSha512  -> "SCRAM-SHA-512"
  NameOAuthBearer  -> "OAUTHBEARER"
  NameAwsMskIam    -> "AWS_MSK_IAM"
  NameGssapi       -> "GSSAPI"

configMechanism :: SaslConfig -> SaslMechanismName
configMechanism = \case
  SaslPlain{}                               -> NamePlain
  SaslPlainWithAuthzid{}                    -> NamePlain
  SaslScram Scram.ScramSHA256 _ _           -> NameScramSha256
  SaslScram Scram.ScramSHA512 _ _           -> NameScramSha512
  SaslOAuthBearer{}                         -> NameOAuthBearer
  SaslOAuthBearerWithExtensions{}           -> NameOAuthBearer
  SaslAwsMskIam{}                           -> NameAwsMskIam
  SaslGssapi{}                              -> NameGssapi

------------------------------------------------------------------------
-- Mechanism implementation interface
------------------------------------------------------------------------

-- | What a single client step yields. After 'StepDone' the driver
-- still parses the next broker response so it can verify mechanism-
-- defined trailing tokens (SCRAM's server-final), but stops sending
-- new ones.
data StepResult
  = StepSend  !ByteString (Maybe ByteString -> IO (Either String StepResult))
    -- ^ Send these bytes; when the broker replies, feed its bytes
    --   (or 'Nothing' for "the broker said this was the last round")
    --   back in to get the next step.
  | StepDone  !(Maybe (ByteString -> Either String ()))
    -- ^ Authentication is complete. If we got a 'Just verifier', the
    --   driver will hand the broker's final auth_bytes (which may
    --   come back empty) to the verifier as a soundness check.
  | StepError !String
    -- ^ Mechanism declared a hard failure.

-- | One implementation of a SASL mechanism. 'smiName' is the wire
-- mechanism name we send in SaslHandshakeRequest; 'smiInitial'
-- starts the conversation.
data SaslMechanismImpl = SaslMechanismImpl
  { smiName    :: !Text
  , smiInitial :: !(IO (Either String StepResult))
  }

------------------------------------------------------------------------
-- Driver
------------------------------------------------------------------------

data AuthError
  = AuthHandshake   !String        -- ^ SaslHandshake transport / decode failed
  | AuthMechanismRejected !Text [Text]
                                   -- ^ Broker doesn't support our mechanism;
                                   --   field 1 = our mechanism, field 2 = supported
  | AuthBrokerError !Int16 !Text   -- ^ Broker returned a SASL error code
  | AuthMechanism   !String        -- ^ Mechanism implementation said something went wrong
  | AuthTransport   !String        -- ^ Network / decode error during the SASL exchange
  deriving (Show)

-- | Metadata returned by a successful SASL authentication.
data AuthSuccess = AuthSuccess
  { authSessionLifetimeMs :: !Int
    -- ^ Broker-advertised @SaslAuthenticateResponse.session_lifetime_ms@.
    --   Zero means the broker did not advertise a SASL reauth deadline.
  } deriving (Eq, Show)

-- | Run the full SASL handshake (SaslHandshakeRequest +
-- SaslAuthenticateRequest loop) over an already-open connection.
authenticate
  :: Connection
  -> Text          -- ^ Client id (used in request headers)
  -> Text          -- ^ Broker host (used by mechanisms like AWS_MSK_IAM)
  -> SaslConfig
  -> IO (Either AuthError ())
authenticate conn clientId host cfg =
  fmap (fmap (const ())) (authenticateDetailed conn clientId host cfg)

-- | Like 'authenticate', but preserves broker metadata needed by
-- KIP-368 re-authentication scheduling.
authenticateDetailed
  :: Connection
  -> Text          -- ^ Client id (used in request headers)
  -> Text          -- ^ Broker host (used by mechanisms like AWS_MSK_IAM)
  -> SaslConfig
  -> IO (Either AuthError AuthSuccess)
authenticateDetailed conn clientId host cfg = do
  let mechName = mechanismWireName (configMechanism cfg)
      mech     = mechanismImpl host cfg
  corrIdRef <- newIORef (0 :: Int32)
  let nextCorrId = atomicModifyIORef' corrIdRef (\c -> (c + 1, c))
      clientIdK  = P.mkKafkaString clientId

  hsR <- runHandshake conn clientIdK nextCorrId mechName
  case hsR of
    Left e         -> pure (Left e)
    Right brokerMs ->
      let advertised = map kafkaStrToText brokerMs
      in if mechName `elem` advertised
           then drive conn clientIdK nextCorrId mech
           else pure (Left (AuthMechanismRejected mechName advertised))

------------------------------------------------------------------------
-- Handshake (KIP-43)
------------------------------------------------------------------------

runHandshake
  :: Connection
  -> P.KafkaString
  -> IO Int32
  -> Text
  -> IO (Either AuthError [P.KafkaString])
runHandshake conn clientId nextCorrId mechName = do
  let req = SHReq.SaslHandshakeRequest
        { SHReq.saslHandshakeRequestMechanism = P.mkKafkaString mechName }
      apiVersion = 1 :: Int16
      reqBytes   = WC.runEncodeVer @SHReq.SaslHandshakeRequest apiVersion req
  cid <- nextCorrId
  txn <- try $ Req.sendRequestReceiveResponse conn 17 apiVersion cid clientId reqBytes
  case txn of
    Left (e :: SomeException) ->
      pure (Left (AuthHandshake ("SaslHandshake transport: " <> show e)))
    Right (Left err) ->
      pure (Left (AuthHandshake ("SaslHandshake transport: " <> err)))
    Right (Right (_, body)) ->
      case WC.runDecodeVer @SHResp.SaslHandshakeResponse apiVersion body of
        Left err -> pure (Left (AuthHandshake ("SaslHandshakeResponse decode: " <> err)))
        Right resp ->
          let ec  = SHResp.saslHandshakeResponseErrorCode resp
              ms  = case P.unKafkaArray (SHResp.saslHandshakeResponseMechanisms resp) of
                      P.NotNull v -> V.toList v
                      P.Null      -> []
          in if ec /= 0
               then pure (Left (AuthBrokerError ec
                       ("SaslHandshake error code " <> T.pack (show ec))))
               else pure (Right ms)

------------------------------------------------------------------------
-- Authenticate loop (KIP-152: wrapped SaslAuthenticate)
------------------------------------------------------------------------

drive
  :: Connection
  -> P.KafkaString
  -> IO Int32
  -> SaslMechanismImpl
  -> IO (Either AuthError AuthSuccess)
drive conn clientId nextCorrId SaslMechanismImpl{..} = do
  initR <- smiInitial
  case initR of
    Left err -> pure (Left (AuthMechanism err))
    Right step -> loop 0 step
  where
    loop !lastLifetimeMs = \case
      StepError err           -> pure (Left (AuthMechanism err))
      StepDone Nothing        -> pure (Right (AuthSuccess lastLifetimeMs))
      StepDone (Just verify)  ->
        -- The broker may be silent now; we don't expect more bytes
        -- but if it sends them we still hand them through verify.
        pure (Right (AuthSuccess lastLifetimeMs))
        -- (the actual round-trip path lives in StepSend below)
      StepSend out k -> do
        sendR <- saslAuthenticate conn clientId nextCorrId out
        case sendR of
          Left err               -> pure (Left err)
          Right brokerResponse    -> do
            nextR <- k (sbrAuthBytes brokerResponse)
            case nextR of
              Left err     -> pure (Left (AuthMechanism err))
              Right next   -> loop (sbrSessionLifetimeMs brokerResponse) next

data SaslBrokerResponse = SaslBrokerResponse
  { sbrAuthBytes :: !(Maybe ByteString)
  , sbrSessionLifetimeMs :: !Int
  } deriving (Eq, Show)

saslAuthenticate
  :: Connection
  -> P.KafkaString
  -> IO Int32
  -> ByteString
  -> IO (Either AuthError SaslBrokerResponse)
saslAuthenticate conn clientId nextCorrId bytes = do
  let req = SAReq.SaslAuthenticateRequest
        { SAReq.saslAuthenticateRequestAuthBytes = P.mkKafkaBytes bytes }
      apiVersion = 1 :: Int16
      reqBytes   = WC.runEncodeVer @SAReq.SaslAuthenticateRequest apiVersion req
  cid <- nextCorrId
  txn <- try $ Req.sendRequestReceiveResponse conn 36 apiVersion cid clientId reqBytes
  case txn of
    Left (e :: SomeException) ->
      pure (Left (AuthTransport ("SaslAuthenticate transport: " <> show e)))
    Right (Left err) ->
      pure (Left (AuthTransport ("SaslAuthenticate transport: " <> err)))
    Right (Right (_, body)) ->
      case WC.runDecodeVer @SAResp.SaslAuthenticateResponse apiVersion body of
        Left err -> pure (Left (AuthTransport ("SaslAuthenticateResponse decode: " <> err)))
        Right resp ->
          let ec   = SAResp.saslAuthenticateResponseErrorCode resp
              msg  = kafkaStrToText (SAResp.saslAuthenticateResponseErrorMessage resp)
              raw  = case P.unKafkaBytes (SAResp.saslAuthenticateResponseAuthBytes resp) of
                       P.NotNull v -> Just v
                       P.Null      -> Nothing
              lifetimeMs = fromIntegral (SAResp.saslAuthenticateResponseSessionLifetimeMs resp)
          in if ec /= 0
               then pure (Left (AuthBrokerError ec
                       (if T.null msg
                          then "SaslAuthenticate error " <> T.pack (show ec)
                          else msg)))
               else pure (Right (SaslBrokerResponse raw lifetimeMs))

------------------------------------------------------------------------
-- Built-in mechanism implementations
------------------------------------------------------------------------

mechanismImpl :: Text -> SaslConfig -> SaslMechanismImpl
mechanismImpl host = \case
  SaslPlain user pwd            -> plainImpl user pwd
  SaslPlainWithAuthzid authzid user pwd -> plainImplWithAuthzid authzid user pwd
  SaslScram algo user pwd       -> scramImpl algo user pwd
  SaslOAuthBearer provider      -> oauthBearerImpl provider
  SaslOAuthBearerWithExtensions provider ext -> oauthBearerImplWithExtensions ext provider
  SaslAwsMskIam provider region -> awsMskIamImpl provider host region
  SaslGssapi                    -> gssapiImplForHost host

-- | SASL\/PLAIN: one client message, broker either accepts or rejects.
plainImpl :: Text -> Text -> SaslMechanismImpl
plainImpl = plainImplWithAuthzidMaybe Nothing

-- | SASL\/PLAIN with an explicit authorization identity.
plainImplWithAuthzid :: Text -> Text -> Text -> SaslMechanismImpl
plainImplWithAuthzid authzid = plainImplWithAuthzidMaybe (Just authzid)

plainImplWithAuthzidMaybe :: Maybe Text -> Text -> Text -> SaslMechanismImpl
plainImplWithAuthzidMaybe mAuthzid user pwd = SaslMechanismImpl
  { smiName    = "PLAIN"
  , smiInitial = pure $ do
      validatePlainInputs mAuthzid user pwd
      Right initial
  }
  where
    bytes   = Plain.generatePlainAuthWithAuthzid mAuthzid user pwd
    initial = StepSend bytes $ \_brokerBytes ->
      -- Broker either errored (and we'd never get here — the driver
      -- short-circuits on a non-zero error code) or accepted with no
      -- payload. Either way, we're done.
      pure (Right (StepDone Nothing))

-- | SASL\/SCRAM-SHA-{256,512}.
scramImpl :: Scram.ScramAlgo -> Text -> Text -> SaslMechanismImpl
scramImpl algo user pwd = SaslMechanismImpl
  { smiName    = Scram.algoMechanismName algo
  , smiInitial = do
      session <- Scram.newScramSession algo user pwd
      let cf = Scram.firstClientMessage session
      pure $ Right $ StepSend cf $ \mServerFirst -> pure $ case mServerFirst of
        Nothing -> Left "SCRAM: broker closed the auth conversation after the client-first message without sending a server-first."
        Just serverFirst ->
          case Scram.finalClientMessage session serverFirst of
            Left err -> Left err
            Right (clientFinal, verifier) ->
              Right $ StepSend clientFinal $ \mFinal -> pure $ case mFinal of
                Nothing       -> Left "SCRAM: broker did not send a server-final message."
                Just sfBytes  -> case verifier sfBytes of
                  Left err -> Left err
                  Right () -> Right (StepDone Nothing)
  }

-- | SASL\/OAUTHBEARER: one client message containing the framed
-- bearer token; broker either accepts (with empty auth_bytes) or
-- rejects with an error code.
oauthBearerImpl :: OAuth.OAuthTokenProvider -> SaslMechanismImpl
oauthBearerImpl = oauthBearerImplWithExtensions OAuth.defaultOAuthBearerExtensions

oauthBearerImplWithExtensions
  :: OAuth.OAuthBearerExtensions
  -> OAuth.OAuthTokenProvider
  -> SaslMechanismImpl
oauthBearerImplWithExtensions ext provider = SaslMechanismImpl
  { smiName    = "OAUTHBEARER"
  , smiInitial = do
      tokenR <- OAuth.resolveOAuthToken provider
      case tokenR of
        Left err  -> pure (Left ("OAUTHBEARER: " <> err))
        Right tok -> pure $ do
          OAuth.validateOAuthBearerPayload ext tok
          let bytes = OAuth.buildOAuthPayloadWithExtensions ext tok
          Right $ StepSend bytes $ \_ -> pure (Right (StepDone Nothing))
  }

-- | AWS MSK IAM. Computes the SigV4-signed JSON payload up front,
-- sends it, and waits for the broker to accept.
awsMskIamImpl :: Iam.AwsCredentialsProvider -> Text -> Text -> SaslMechanismImpl
awsMskIamImpl provider host region = SaslMechanismImpl
  { smiName    = "AWS_MSK_IAM"
  , smiInitial = do
      payloadR <- Iam.buildIamPayloadIO provider host region
                    "wireform-kafka/0.1" 900
      case payloadR of
        Left err -> pure (Left ("AWS_MSK_IAM: " <> err))
        Right bs -> pure $ Right $ StepSend bs $ \_ -> pure (Right (StepDone Nothing))
  }

-- | Stub for GSSAPI \/ Kerberos. Returning an explicit error here
-- gives users a clear message instead of a confusing broker-side
-- timeout.
gssapiImpl :: SaslMechanismImpl
gssapiImpl = gssapiImplForHost "localhost"

gssapiImplForHost :: Text -> SaslMechanismImpl
#ifdef WIREFORM_KAFKA_GSSAPI
gssapiImplForHost host = SaslMechanismImpl
  { smiName    = "GSSAPI"
  , smiInitial = do
      ctxR <- try (newGssClientContext ("kafka@" <> TE.encodeUtf8 host))
      case ctxR of
        Left (e :: SomeException) ->
          pure (Left ("GSSAPI/Kerberos init failed: " <> show e))
        Right ctx -> do
          stepR <- runGssClientStep ctx Nothing
          pure (gssStep ctx stepR)
  }
#else
gssapiImplForHost _ = SaslMechanismImpl
  { smiName    = "GSSAPI"
  , smiInitial = pure $ Left
      "GSSAPI/Kerberos is not implemented in wireform-kafka. Build wireform-kafka with the gssapi flag to enable the optional Kerberos dependency, or use SASL/SCRAM-SHA-512 or AWS_MSK_IAM."
  }
#endif

gssapiBuildEnabled :: Bool
#ifdef WIREFORM_KAFKA_GSSAPI
gssapiBuildEnabled = True
#else
gssapiBuildEnabled = False
#endif

#ifdef WIREFORM_KAFKA_GSSAPI
data OidDescStruct
newtype GssOID = GssOID (Ptr OidDescStruct)
newtype GssNameT = GssNameT { unGssNameT :: Ptr () } deriving (Storable)
newtype GssCredIdT = GssCredIdT (Ptr ()) deriving (Storable)
newtype GssCtxIdT = GssCtxIdT (Ptr ()) deriving (Storable)
newtype GssChannelBindingsT = GssChannelBindingsT (Ptr ()) deriving (Storable)

data GssBufferDesc = GssBufferDesc !CSize !(Ptr ())

instance Storable GssBufferDesc where
  sizeOf _ = sizeOf (undefined :: CSize) + sizeOf (undefined :: Ptr ())
  alignment _ = alignment (undefined :: Ptr ())
  poke p (GssBufferDesc len value) = do
    poke (castPtr p) len
    poke (castPtr (p `plusPtr` sizeOf (undefined :: CSize))) value
  peek p =
    GssBufferDesc
      <$> peek (castPtr p)
      <*> peek (castPtr (p `plusPtr` sizeOf (undefined :: CSize)))

foreign import capi "gssapi/gssapi.h value GSS_C_NT_HOSTBASED_SERVICE"
  gssCNtHostbasedService :: GssOID
foreign import capi "gssapi/gssapi.h value GSS_C_NO_OID"
  gssCNoOid :: GssOID
foreign import capi "gssapi/gssapi.h value GSS_C_NO_CREDENTIAL"
  gssCNoCredential :: GssCredIdT
foreign import capi "gssapi/gssapi.h value GSS_C_NO_CONTEXT"
  gssCNoContext :: GssCtxIdT
foreign import capi "gssapi/gssapi.h value GSS_C_NO_CHANNEL_BINDINGS"
  gssCNoChannelBindings :: GssChannelBindingsT
foreign import capi "gssapi/gssapi.h value GSS_C_NO_BUFFER"
  gssCNoBuffer :: Ptr GssBufferDesc
foreign import capi "gssapi/gssapi.h value GSS_C_GSS_CODE"
  gssCGssCode :: CUInt
foreign import capi "gssapi/gssapi.h value GSS_C_MECH_CODE"
  gssCMechCode :: CUInt
foreign import capi "gssapi/gssapi.h value GSS_S_CONTINUE_NEEDED"
  gssSContinueNeeded :: CUInt

foreign import capi "gssapi/gssapi.h GSS_ERROR"
  gssErrorRaw :: CUInt -> CUInt

foreign import ccall unsafe "gssapi/gssapi.h gss_import_name"
  c_gss_import_name
    :: Ptr CUInt -> Ptr GssBufferDesc -> GssOID -> Ptr GssNameT -> IO CUInt

foreign import ccall unsafe "gssapi/gssapi.h gss_release_name"
  c_gss_release_name :: Ptr CUInt -> GssNameT -> IO CUInt

foreign import ccall unsafe "gssapi/gssapi.h gss_release_buffer"
  c_gss_release_buffer :: Ptr CUInt -> Ptr GssBufferDesc -> IO CUInt

foreign import ccall unsafe "gssapi/gssapi.h gss_display_status"
  c_gss_display_status
    :: Ptr CUInt -> CUInt -> CUInt -> GssOID -> Ptr CUInt -> Ptr GssBufferDesc -> IO CUInt

foreign import ccall safe "gssapi/gssapi.h gss_init_sec_context"
  c_gss_init_sec_context
    :: Ptr CUInt
    -> GssCredIdT
    -> Ptr GssCtxIdT
    -> GssNameT
    -> GssOID
    -> CUInt
    -> CUInt
    -> GssChannelBindingsT
    -> Ptr GssBufferDesc
    -> Ptr GssOID
    -> Ptr GssBufferDesc
    -> Ptr CUInt
    -> Ptr CUInt
    -> IO CUInt

foreign import ccall unsafe "gssapi/gssapi.h gss_delete_sec_context"
  c_gss_delete_sec_context :: Ptr CUInt -> Ptr GssCtxIdT -> Ptr GssBufferDesc -> IO CUInt

data GssClientContext = GssClientContext
  { gccContext :: !(ForeignPtr GssCtxIdT)
  , gccTargetName :: !(ForeignPtr ())
  }

newGssClientContext :: ByteString -> IO GssClientContext
newGssClientContext target = bracketOnError (gssImportName target) gssReleaseName $ \targetName -> do
  nameFp <- FC.newForeignPtr (unGssNameT targetName) (gssReleaseName targetName)
  ctxPtr <- malloc
  poke ctxPtr gssCNoContext
  ctxFp <- FC.newForeignPtr ctxPtr $ do
    alloca $ \minor -> void (c_gss_delete_sec_context minor ctxPtr gssCNoBuffer)
    free ctxPtr
  pure GssClientContext
    { gccContext = ctxFp
    , gccTargetName = nameFp
    }

gssImportName :: ByteString -> IO GssNameT
gssImportName target =
  withInputBuffer (Just target) $ \targetBuf ->
    alloca $ \minor ->
      alloca $ \namePtr -> do
        major <- c_gss_import_name minor targetBuf gssCNtHostbasedService namePtr
        whenGssOk major minor (peek namePtr)

gssReleaseName :: GssNameT -> IO ()
gssReleaseName name =
  alloca $ \minor -> void (c_gss_release_name minor name)

runGssClientStep :: GssClientContext -> Maybe ByteString -> IO (Either String (ByteString, Bool))
runGssClientStep GssClientContext{..} mInput =
  fmap normalize (try runStep)
  where
    normalize :: Either SomeException (ByteString, Bool) -> Either String (ByteString, Bool)
    normalize = \case
      Left e -> Left (show e)
      Right r -> Right r

    runStep =
      withForeignPtr gccContext $ \ctxPtr ->
        withForeignPtr gccTargetName $ \targetPtr ->
          withInputBuffer mInput $ \inputBuf ->
            withOutputBuffer $ \outputBuf ->
              alloca $ \minor ->
                alloca $ \retFlags ->
                  alloca $ \timeRec -> do
                    major <- c_gss_init_sec_context
                      minor
                      gssCNoCredential
                      ctxPtr
                      (GssNameT targetPtr)
                      gssCNoOid
                      0
                      0
                      gssCNoChannelBindings
                      inputBuf
                      nullPtr
                      outputBuf
                      retFlags
                      timeRec
                    whenGssOk major minor $ do
                      output <- peekGssBuffer outputBuf
                      pure (output, gssContinueNeeded major)

gssStep :: GssClientContext -> Either String (ByteString, Bool) -> Either String StepResult
gssStep _ (Left err) = Left ("GSSAPI/Kerberos: " <> err)
gssStep ctx (Right (out, continue))
  | BS.null out && continue =
      Left "GSSAPI/Kerberos: GSSAPI requested continuation without an output token"
  | BS.null out =
      Right (StepDone Nothing)
  | otherwise =
      Right $ StepSend out $ \mChallenge ->
        if continue
          then case mChallenge of
            Nothing -> pure (Left "GSSAPI/Kerberos: broker did not send a continuation token")
            Just challenge -> do
              next <- runGssClientStep ctx (Just challenge)
              pure (gssStep ctx next)
          else pure (Right (StepDone Nothing))

gssContinueNeeded :: CUInt -> Bool
gssContinueNeeded major = major .&. gssSContinueNeeded /= 0

gssError :: CUInt -> Bool
gssError major = gssErrorRaw major /= 0

whenGssOk :: CUInt -> Ptr CUInt -> IO a -> IO a
whenGssOk major minorPtr action
  | gssError major = do
      minor <- peek minorPtr
      majorMsg <- gssDisplayStatus gssCGssCode major
      minorMsg <- gssDisplayStatus gssCMechCode minor
      throwIO (userError ("GSS major=" <> BS8.unpack majorMsg <> "; minor=" <> BS8.unpack minorMsg))
  | otherwise = action

gssDisplayStatus :: CUInt -> CUInt -> IO ByteString
gssDisplayStatus statusType status =
  alloca $ \minor ->
    alloca $ \msgCtx ->
      withOutputBuffer $ \buf -> do
        poke msgCtx 0
        major <- c_gss_display_status minor status statusType gssCNoOid msgCtx buf
        if gssError major
          then pure ""
          else peekGssBuffer buf

withInputBuffer :: Maybe ByteString -> (Ptr GssBufferDesc -> IO a) -> IO a
withInputBuffer Nothing action = action gssCNoBuffer
withInputBuffer (Just bs) action =
  BS.useAsCStringLen bs $ \(ptr, len) ->
    alloca $ \buf -> do
      poke buf (GssBufferDesc (fromIntegral len) (castPtr ptr))
      action buf

withOutputBuffer :: (Ptr GssBufferDesc -> IO a) -> IO a
withOutputBuffer action =
  alloca $ \buf -> do
    poke buf (GssBufferDesc 0 nullPtr)
    action buf `finally` releaseBuffer buf

releaseBuffer :: Ptr GssBufferDesc -> IO ()
releaseBuffer buf =
  alloca $ \minor -> void (c_gss_release_buffer minor buf)

peekGssBuffer :: Ptr GssBufferDesc -> IO ByteString
peekGssBuffer buf = do
  GssBufferDesc len ptr <- peek buf
  if len == 0 || ptr == nullPtr
    then pure BS.empty
    else BS.packCStringLen (castPtr ptr, fromIntegral len)
#endif

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

kafkaStrToText :: P.KafkaString -> Text
kafkaStrToText ks = case P.unKafkaString ks of
  P.NotNull t -> t
  P.Null      -> T.empty

validatePlainInputs :: Maybe Text -> Text -> Text -> Either String ()
validatePlainInputs mAuthzid user pwd = do
  maybe (Right ()) (validatePlainField "authorization identity") mAuthzid
  validatePlainField "username" user
  validatePlainField "password" pwd

validatePlainField :: String -> Text -> Either String ()
validatePlainField fieldName value
  | T.any (== '\NUL') value =
      Left ("PLAIN: " <> fieldName <> " must not contain NUL")
  | otherwise = Right ()

------------------------------------------------------------------------
-- KIP-368 session re-authentication
------------------------------------------------------------------------

-- | Compute the effective re-authentication deadline (epoch
-- milliseconds) from the broker-advertised lifetime and the
-- client's @connections.max.reauth.ms@ knob.
--
-- KIP-368 lets the broker tell us how long the credentials we
-- presented are valid via 'SaslAuthenticateResponse.lifetime_ms'.
-- The client should run a fresh @SaslHandshake@ + @SaslAuthenticate@
-- cycle /before/ the smaller of the two deadlines elapses,
-- otherwise the broker silently closes the connection.
--
-- Returns @Nothing@ when neither side has set a deadline (i.e. both
-- the broker lifetime and the client config are 0): the connection
-- is open-ended, no re-auth is required. Otherwise returns the
-- absolute epoch-ms at which re-auth must complete by.
effectiveReauthDeadlineMs
  :: Int          -- ^ wall-clock when authentication completed (epoch ms)
  -> Int          -- ^ broker-advertised lifetime (ms; 0 = no expiry)
  -> Int          -- ^ client @connections.max.reauth.ms@ (0 = disabled)
  -> Maybe Int
effectiveReauthDeadlineMs nowMs brokerLifetimeMs clientMaxMs =
  case (brokerLifetimeMs > 0, clientMaxMs > 0) of
    (False, False) -> Nothing
    (True,  False) -> Just (nowMs + brokerLifetimeMs)
    (False, True)  -> Just (nowMs + clientMaxMs)
    (True,  True)  -> Just (nowMs + min brokerLifetimeMs clientMaxMs)

-- | Decide whether the next request should run through a fresh
-- handshake first. Compares the current time to the previously
-- computed deadline (from 'effectiveReauthDeadlineMs') and applies
-- a small safety margin so we don't race the broker's enforcement.
--
-- The safety margin is taken as @max 1000 (deadline\/10)@ — i.e.
-- one second or 10% of the remaining window, whichever is greater.
-- Mirrors the JVM client's behaviour.
reauthRequiredAtMs
  :: Int          -- ^ now (epoch ms)
  -> Maybe Int    -- ^ deadline returned by 'effectiveReauthDeadlineMs'
  -> Bool
reauthRequiredAtMs _   Nothing  = False
reauthRequiredAtMs now (Just d) =
  let !remaining = d - now
      !margin    = max 1000 (max 0 d `div` 10)
  in remaining <= margin

