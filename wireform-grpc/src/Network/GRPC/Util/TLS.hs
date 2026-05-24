{-# LANGUAGE OverloadedStrings #-}

-- | TLS utilities (OpenSSL-backed).
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Util.TLS (ServerValidation(..))
-- > import Network.GRPC.Util.TLS qualified as Util.TLS
--
-- == Migration note
--
-- This module was originally a thin wrapper over
-- @Data.X509.CertificateStore@ + the system trust store provided
-- by @System.X509@.  After the OpenSSL migration, OpenSSL itself
-- owns the certificate-store machinery: the system trust store
-- comes from @SSL_CTX_set_default_verify_paths@, and additional
-- PEM bundles are loaded into the per-context store via
-- @SSL_CTX_load_verify_locations@ (see
-- 'Wireform.Network.TLS.OpenSSL.loadCaBundle').
--
-- The 'CertificateStoreSpec' ADT is therefore re-expressed in
-- OpenSSL terms: it composes a set of PEM bundle paths + a flag
-- for whether the system store should be layered in.
-- 'loadCertificateStore' resolves the spec to that flat shape; the
-- caller plugs the paths into a 'SslCtx' via
-- 'loadCaBundle'.  In-memory cert lists are no longer accepted
-- (OpenSSL's @SSL_CTX_load_verify_locations@ takes paths, not
-- @X509@ values); construct an on-disk PEM file if you need that.
module Network.GRPC.Util.TLS (
    -- * Certificate store
    CertificateStoreSpec(..)
  , certStoreFromSystem
  , certStoreFromPath
  , loadCertificateStore
  , ResolvedCertificateStore (..)
  , applyResolvedToCtx
    -- * Configuration
    -- ** Parameters
  , ServerValidation(..)
  , validationCAStore
    -- ** Common to server and client
  , SslKeyLog(..)
  , keyLogger
  ) where

import Control.Exception
import Data.Default
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.Environment

import Wireform.Network.TLS.OpenSSL (SslCtx, loadCaBundle)

{-------------------------------------------------------------------------------
  Certificate store
-------------------------------------------------------------------------------}

-- | Certificate store specification (for certificate validation).
--
-- A deep embedding that describes how to assemble OpenSSL's
-- verification material for a connection.  Composable through the
-- 'Monoid' instance.
--
-- Three primitive forms:
--
--   * 'CertStoreFromSystem' — use the system trust store
--     (OpenSSL's @SSL_CTX_set_default_verify_paths@).
--   * 'CertStoreFromPath' — additional PEM CA bundle file or
--     directory.  Passed to OpenSSL's
--     @SSL_CTX_load_verify_locations@ via
--     'Wireform.Network.TLS.OpenSSL.loadCaBundle'.
--   * 'CertStoreAppend' — combine two specs.
--
-- (The pre-rewrite @CertStoreFromCerts [X509.SignedCertificate]@
-- variant is gone; OpenSSL takes PEM paths, not in-memory
-- @X509@ values.  If you have certificate bytes in memory, write
-- them to a temp PEM file and use 'CertStoreFromPath'.)
data CertificateStoreSpec =
    CertStoreEmpty
  | CertStoreAppend CertificateStoreSpec CertificateStoreSpec
  | CertStoreFromSystem
  | CertStoreFromPath FilePath
  deriving (Show)

instance Semigroup CertificateStoreSpec where
  (<>) = CertStoreAppend

instance Monoid CertificateStoreSpec where
  mempty = CertStoreEmpty

-- | Use the system's certificate store.
certStoreFromSystem :: CertificateStoreSpec
certStoreFromSystem = CertStoreFromSystem

-- | Load certificate store from disk.  The path may point to a
-- single PEM file (multiple PEM-formatted certificates
-- concatenated) or a directory (one certificate per file, file
-- names are hashes from certificate).
certStoreFromPath :: FilePath -> CertificateStoreSpec
certStoreFromPath = CertStoreFromPath

-- | A resolved 'CertificateStoreSpec': the system-store flag plus
-- the list of extra PEM bundle paths the caller wants layered on
-- top.
data ResolvedCertificateStore = ResolvedCertificateStore
  { rcsUseSystem    :: !Bool
  , rcsExtraBundles :: ![FilePath]
  } deriving stock (Show, Eq)

instance Semigroup ResolvedCertificateStore where
  a <> b = ResolvedCertificateStore
    { rcsUseSystem    = rcsUseSystem a || rcsUseSystem b
    , rcsExtraBundles = rcsExtraBundles a <> rcsExtraBundles b
    }

instance Monoid ResolvedCertificateStore where
  mempty = ResolvedCertificateStore False []

-- | Resolve a 'CertificateStoreSpec' into the system-store flag +
-- the list of extra bundle paths.  Throws 'NoCertificatesAtPath'
-- if any 'CertStoreFromPath' refers to a missing file.
--
-- The returned record can be applied to a 'SslCtx' via
-- 'applyResolvedToCtx'.
loadCertificateStore :: CertificateStoreSpec -> IO ResolvedCertificateStore
loadCertificateStore = go
  where
    go CertStoreEmpty         = pure mempty
    go (CertStoreAppend a b)  = (<>) <$> go a <*> go b
    go CertStoreFromSystem    = pure (ResolvedCertificateStore True [])
    go (CertStoreFromPath fp) = do
      ok <- doesFileExist fp
      if ok
        then pure (ResolvedCertificateStore False [fp])
        else throwIO (NoCertificatesAtPath fp)

-- | Apply a resolved certificate store to a 'SslCtx': layers each
-- extra PEM bundle in via 'loadCaBundle'.  (The system store flag
-- is informational; OpenSSL contexts produced by
-- 'Wireform.Network.TLS.OpenSSL.newClientCtx' with @verifyPeer=True@
-- already pick up the system store.)
applyResolvedToCtx :: SslCtx -> ResolvedCertificateStore -> IO ()
applyResolvedToCtx ctx ResolvedCertificateStore{rcsExtraBundles} =
  mapM_ (loadCaBundle ctx) rcsExtraBundles

data LoadCertificateStoreException =
    NoCertificatesAtPath FilePath
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Parameters
-------------------------------------------------------------------------------}

-- | How does the client want to validate the server?
data ServerValidation =
    -- | Validate the server.  The accompanying 'CertificateStoreSpec'
    --   describes the trust roots to use (in addition to whatever
    --   the OpenSSL system store provides).
    ValidateServer CertificateStoreSpec

    -- | Skip server validation.
    --
    -- WARNING: This is dangerous.  Although communication with the
    -- server will still be encrypted, you cannot be sure that the
    -- server is who they claim to be.
  | NoServerValidation
  deriving (Show)

-- | Resolve the 'ServerValidation' to a 'ResolvedCertificateStore'.
-- 'NoServerValidation' returns 'mempty'.
validationCAStore :: ServerValidation -> IO ResolvedCertificateStore
validationCAStore (ValidateServer storeSpec) = loadCertificateStore storeSpec
validationCAStore NoServerValidation         = pure mempty

{-------------------------------------------------------------------------------
  Configuration common to server and client
-------------------------------------------------------------------------------}

-- | SSL key log file.
--
-- An SSL key log file can be used by tools such as Wireshark to
-- decode TLS network traffic.  It is used for debugging only.
--
-- /Note:/ the OpenSSL FFI in 'Wireform.Network.TLS.OpenSSL' does
-- not yet wire @SSL_CTX_set_keylog_callback@; the returned logger
-- function is honored at the @Settings@ level but does not yet
-- propagate to OpenSSL.  Adding the callback is a follow-up.
data SslKeyLog =
    SslKeyLogNone
  | SslKeyLogPath FilePath
  | SslKeyLogFromEnv
  deriving stock (Show, Eq, Generic)

instance Default SslKeyLog where
  def = SslKeyLogFromEnv

keyLogger :: SslKeyLog -> IO (String -> IO ())
keyLogger sslKeyLog = do
    keyLogFile <- case sslKeyLog of
                    SslKeyLogNone    -> pure Nothing
                    SslKeyLogPath fp -> pure (Just fp)
                    SslKeyLogFromEnv -> lookupEnv "SSLKEYLOGFILE"
    pure $
      case keyLogFile of
        Nothing -> \_   -> pure ()
        Just fp -> \str -> appendFile fp (str ++ "\n")
