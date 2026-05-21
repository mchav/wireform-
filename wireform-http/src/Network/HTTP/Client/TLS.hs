{- | Pluggable TLS backend for the wireform HTTP client.

The 'TLSBackend' record is the spec's abstraction over the
underlying TLS implementation: handshake, byte read \/ write,
close, with ALPN advertised through 'TLSSettings'. Backends ship
as values, so swapping from the default Haskell 'tls' library to
OpenSSL (in a separate @wireform-http-openssl@ package) is a
configuration change rather than a code change.

== Status

The 'defaultTLSBackend' value built in to this package routes
through the existing "Network.HTTP.TLS" handshake (the @tls@
package + ALPN). It satisfies the API contract for callers, but
the actual byte read \/ write hooks are stubs ('tlsRead' \/
'tlsWrite' point at the bracketed
'Network.HTTP.Connection.withConnection' path internally; the
backend's per-byte hooks aren't yet threaded through the low-level
HTTP\/1 \/ HTTP\/2 transport layers).

In practice that means:

* Specifying 'ccTlsBackend' selects which value 'withClient'
  records on the 'ClientConfig'. Application code that wants to
  swap implementations (e.g. to @opensslBackend@ from a
  hypothetical 'wireform-http-openssl' package) writes the swap
  there.
* The current connection layer still uses the @tls@ package
  directly. Switching that to call through the 'TLSBackend' hooks
  is a follow-up that touches 'Network.HTTP.TLS',
  'Network.HTTP1.Transport', and 'Network.HTTP2.TLS' — the
  abstraction is in place to make that refactor mechanical.

== Why ship the API now

So callers can start writing against it, and so we can land
@wireform-http-openssl@ as a separate package that drops in for
testing or compliance without changing application code.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.TLS
  ( -- * Backend interface
    TLSBackend (..)
  , TLSConnection
  , defaultTLSBackend
  , noTLSBackend
    -- * Settings
  , TLSSettings (..)
  , defaultTLSSettings
  , CertVerifyMode (..)
  , ClientCert (..)
  , TLSVersionRange (..)
  ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Network.Socket (Socket)

-- ---------------------------------------------------------------------------
-- Settings
-- ---------------------------------------------------------------------------

-- | The parameters a TLS handshake needs.
data TLSSettings = TLSSettings
  { alpnProtocols     :: ![ByteString]
    -- ^ ALPN identifiers to advertise, in preference order.
    --   E.g. @["h2", "http\/1.1"]@.
  , certificateVerify :: !CertVerifyMode
  , clientCertificate :: !(Maybe ClientCert)
  , versionRange      :: !TLSVersionRange
  }
  deriving stock (Show)

data CertVerifyMode
  = VerifySystemTrust
    -- ^ Verify against the OS\'s trust store. The default.
  | VerifyNone
    -- ^ Skip verification entirely. Test-only.
  | VerifyWithRoots ![FilePath]
    -- ^ Verify against the union of these PEM-encoded root files.
  deriving stock (Eq, Show)

-- | Mutual-TLS client cert. Either a PEM file pair (cert + key) or
-- an opaque caller-supplied identifier the backend knows how to
-- resolve.
data ClientCert
  = ClientCertFiles !FilePath !FilePath  -- cert.pem, key.pem
  | ClientCertOpaque !ByteString
  deriving stock (Show)

-- | Minimum and maximum TLS protocol versions the handshake will
-- agree to. The spec uses a 'TLSVersion' newtype; we mirror that
-- with a small wrapper so callers can write @tls12To13@ without
-- pulling in @Network.TLS@ themselves.
data TLSVersionRange = TLSVersionRange
  { minVersion :: !TLSProtoVersion
  , maxVersion :: !TLSProtoVersion
  }
  deriving stock (Eq, Show)

data TLSProtoVersion
  = TLS_1_2
  | TLS_1_3
  deriving stock (Eq, Ord, Show)

defaultTLSSettings :: TLSSettings
defaultTLSSettings = TLSSettings
  { alpnProtocols     = ["h2", "http/1.1"]
  , certificateVerify = VerifySystemTrust
  , clientCertificate = Nothing
  , versionRange      = TLSVersionRange TLS_1_2 TLS_1_3
  }

-- ---------------------------------------------------------------------------
-- Backend interface
-- ---------------------------------------------------------------------------

-- | An opaque per-backend connection handle. Implementations cast
-- through @unsafeCoerce@ or stash a 'Dynamic'; the backend interface
-- only obliges callers to round-trip the handle through 'tlsRead' /
-- 'tlsWrite' / 'tlsClose'.
data TLSConnection = TLSConnection
  { tlsConnImpl :: !Impl
  }

data Impl = ImplNoOp | ImplHaskellTLS  -- placeholder discriminator

-- | A pluggable TLS backend.
data TLSBackend = TLSBackend
  { tlsBackendName :: !String
    -- ^ Human-readable identifier — e.g. @"haskell-tls"@ or
    --   @"openssl"@. Used in tracing\/log messages and for error
    --   reporting; carries no semantics.
  , tlsHandshake   :: !(Socket -> TLSSettings -> IO TLSConnection)
  , tlsClose       :: !(TLSConnection -> IO ())
  , tlsRead        :: !(TLSConnection -> Int -> IO ByteString)
  , tlsWrite       :: !(TLSConnection -> ByteString -> IO ())
  }

-- | The default backend, currently a thin wrapper over the
-- existing "Network.HTTP.TLS" code (the @tls@ package). See the
-- module header for the limitations of the current routing.
defaultTLSBackend :: TLSBackend
defaultTLSBackend = TLSBackend
  { tlsBackendName = "haskell-tls"
  , tlsHandshake   = \_sock _opts ->
      pure (TLSConnection ImplHaskellTLS)
      -- The actual handshake is performed by Network.HTTP.TLS
      -- in the connection layer; this hook is exposed so backends
      -- that /do/ go through the abstraction (e.g. opensslBackend)
      -- can plug in. Wiring our own connection layer through this
      -- hook is tracked as a follow-up.
  , tlsClose = \_ -> pure ()
  , tlsRead  = \_ _ -> throwIO TLSBackendNotWiredThrough
  , tlsWrite = \_ _ -> throwIO TLSBackendNotWiredThrough
  }

-- | A backend that refuses to do TLS. Useful for testing
-- transports that should never have to perform a handshake (e.g.
-- mock transports that already shouldn't see HTTPS URIs).
noTLSBackend :: TLSBackend
noTLSBackend = TLSBackend
  { tlsBackendName = "none"
  , tlsHandshake   = \_ _ -> throwIO TLSBackendDisabled
  , tlsClose       = \_   -> pure ()
  , tlsRead        = \_ _ -> throwIO TLSBackendDisabled
  , tlsWrite       = \_ _ -> throwIO TLSBackendDisabled
  }

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data TLSBackendError
  = TLSBackendDisabled
    -- ^ Thrown by 'noTLSBackend'.
  | TLSBackendNotWiredThrough
    -- ^ Thrown when the per-byte hooks of 'defaultTLSBackend' are
    --   called, which only happens if a future revision of the
    --   connection layer routes through them. Until then, the
    --   default backend's handshake hook produces a placeholder
    --   handle and the actual TLS work goes through the existing
    --   "Network.HTTP.TLS" code.
  deriving stock (Show)

instance Exception TLSBackendError
