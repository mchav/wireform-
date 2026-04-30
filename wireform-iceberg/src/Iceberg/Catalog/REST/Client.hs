{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Minimal HTTP client for the Iceberg REST catalog spec.
--
-- The transport is intentionally tiny: every operation is a pure function
-- from a 'CatalogClient' (which carries the base URL, auth header, and an
-- 'HTTP.Manager') to an @IO@ action that returns either the typed response
-- payload or a 'CatalogError'. Authentication is captured as a single
-- 'AuthHeader'; users that need OAuth refresh-token flows can layer their
-- own logic above 'mkClient'.
--
-- Endpoints implemented (matches the open-api spec at
-- <https://iceberg.apache.org/rest-catalog-spec/>):
--
-- - @GET    /v1/config@                                              — 'getConfig'
-- - @GET    /v1/{prefix}/namespaces@                                 — 'listNamespaces'
-- - @POST   /v1/{prefix}/namespaces@                                 — 'createNamespace'
-- - @GET    /v1/{prefix}/namespaces/{ns}@                            — 'loadNamespace'
-- - @DELETE /v1/{prefix}/namespaces/{ns}@                            — 'dropNamespace'
-- - @GET    /v1/{prefix}/namespaces/{ns}/tables@                     — 'listTables'
-- - @POST   /v1/{prefix}/namespaces/{ns}/tables@                     — 'createTable'
-- - @GET    /v1/{prefix}/namespaces/{ns}/tables/{name}@              — 'loadTable'
-- - @POST   /v1/{prefix}/namespaces/{ns}/tables/{name}@              — 'commitTable'
-- - @DELETE /v1/{prefix}/namespaces/{ns}/tables/{name}@              — 'dropTable'
-- - @GET    /v1/{prefix}/namespaces/{ns}/views@                      — 'listViews'
module Iceberg.Catalog.REST.Client
  ( -- * Client
    CatalogClient(..)
  , AuthHeader(..)
  , mkClient
  , mkClientWithManager
    -- * Configuration
  , getConfig
    -- * Namespaces
  , listNamespaces
  , createNamespace
  , loadNamespace
  , dropNamespace
    -- * Tables
  , listTables
  , createTable
  , loadTable
  , commitTable
  , dropTable
    -- * Views
  , listViews
    -- * Errors
  , CatalogClientError(..)
  ) where

import Control.Exception (Exception, throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HT

import qualified Iceberg.Catalog.REST as REST
import Iceberg.Catalog.REST
  ( CatalogConfig
  , CatalogError (..)
  , CommitTableRequest
  , CommitTableResponse
  , CreateNamespaceRequest
  , CreateNamespaceResponse
  , CreateTableRequest
  , GetNamespaceResponse
  , ListNamespacesResponse (..)
  , ListTablesResponse
  , ListViewsResponse
  , LoadTableResult
  , Namespace
  , TableIdentifier (..)
  )

-- ============================================================
-- Client + auth
-- ============================================================

data AuthHeader
  = NoAuth
  | BearerToken !ByteString
    -- ^ Sent as @Authorization: Bearer <token>@.
  | RawHeader !ByteString !ByteString
    -- ^ Arbitrary header name + value pair (e.g. @Authorization: Basic …@).
  deriving (Show, Eq)

data CatalogClient = CatalogClient
  { ccBaseUrl :: !ByteString
    -- ^ Catalog base URL with no trailing slash, e.g. @https://catalog.example.com@.
  , ccPrefix  :: !(Maybe Text)
    -- ^ Optional @prefix@ component returned by @GET /v1/config@'s
    -- @overrides@ map under the @prefix@ key. Inserted between @/v1/@ and
    -- the resource path on every request.
  , ccAuth    :: !AuthHeader
  , ccManager :: !HTTP.Manager
  }

-- | Build a 'CatalogClient' with a freshly-created TLS-aware HTTP manager.
mkClient :: ByteString -> Maybe Text -> AuthHeader -> IO CatalogClient
mkClient base prefix auth = do
  mgr <- HTTP.newManager TLS.tlsManagerSettings
  pure (mkClientWithManager mgr base prefix auth)

-- | Build a 'CatalogClient' that reuses an existing 'HTTP.Manager'. Use
-- this when you want to share connection pools across catalog clients.
mkClientWithManager
  :: HTTP.Manager -> ByteString -> Maybe Text -> AuthHeader -> CatalogClient
mkClientWithManager mgr base prefix auth = CatalogClient
  { ccBaseUrl = stripTrailingSlash base
  , ccPrefix  = prefix
  , ccAuth    = auth
  , ccManager = mgr
  }

stripTrailingSlash :: ByteString -> ByteString
stripTrailingSlash bs
  | BS.null bs           = bs
  | BS.last bs == 0x2F   = BS.init bs
  | otherwise            = bs

-- ============================================================
-- Errors
-- ============================================================

data CatalogClientError
  = ClientHttpError !HTTP.HttpException
  | ClientCatalogError !CatalogError
  | ClientDecodeError !String !ByteString
  deriving (Show)

instance Exception CatalogClientError

-- ============================================================
-- Endpoints
-- ============================================================

getConfig :: CatalogClient -> IO CatalogConfig
getConfig cc = doRequest cc HT.methodGet "/v1/config" Nothing

listNamespaces :: CatalogClient -> IO (V.Vector Namespace)
listNamespaces cc = do
  resp <- doRequest cc HT.methodGet (catalogPath cc "/namespaces") Nothing
  pure (lnrNamespaces (resp :: ListNamespacesResponse))

createNamespace
  :: CatalogClient -> CreateNamespaceRequest -> IO CreateNamespaceResponse
createNamespace cc req =
  doRequest cc HT.methodPost (catalogPath cc "/namespaces") (Just (Aeson.encode req))

loadNamespace :: CatalogClient -> Namespace -> IO GetNamespaceResponse
loadNamespace cc ns =
  doRequest cc HT.methodGet
    (catalogPath cc ("/namespaces/" <> namespaceSegment ns))
    Nothing

dropNamespace :: CatalogClient -> Namespace -> IO ()
dropNamespace cc ns =
  doRequest_ cc HT.methodDelete
    (catalogPath cc ("/namespaces/" <> namespaceSegment ns))
    Nothing

listTables :: CatalogClient -> Namespace -> IO ListTablesResponse
listTables cc ns =
  doRequest cc HT.methodGet
    (catalogPath cc ("/namespaces/" <> namespaceSegment ns <> "/tables"))
    Nothing

createTable
  :: CatalogClient -> Namespace -> CreateTableRequest -> IO LoadTableResult
createTable cc ns req =
  doRequest cc HT.methodPost
    (catalogPath cc ("/namespaces/" <> namespaceSegment ns <> "/tables"))
    (Just (Aeson.encode req))

loadTable :: CatalogClient -> TableIdentifier -> IO LoadTableResult
loadTable cc ti =
  doRequest cc HT.methodGet
    (tablePath cc ti)
    Nothing

commitTable
  :: CatalogClient -> CommitTableRequest -> IO CommitTableResponse
commitTable cc req =
  doRequest cc HT.methodPost
    (tablePath cc (REST.ctReqIdentifier req))
    (Just (Aeson.encode req))

dropTable :: CatalogClient -> TableIdentifier -> IO ()
dropTable cc ti =
  doRequest_ cc HT.methodDelete (tablePath cc ti) Nothing

listViews :: CatalogClient -> Namespace -> IO ListViewsResponse
listViews cc ns =
  doRequest cc HT.methodGet
    (catalogPath cc ("/namespaces/" <> namespaceSegment ns <> "/views"))
    Nothing

-- ============================================================
-- Internals
-- ============================================================

catalogPath :: CatalogClient -> ByteString -> ByteString
catalogPath cc rest = case ccPrefix cc of
  Just p  -> "/v1/" <> TE.encodeUtf8 p <> rest
  Nothing -> "/v1" <> rest

tablePath :: CatalogClient -> TableIdentifier -> ByteString
tablePath cc ti =
  catalogPath cc
    ( "/namespaces/" <> namespaceSegment (tiNamespace ti)
        <> "/tables/" <> TE.encodeUtf8 (tiName ti) )

-- | Iceberg uses the unit separator (@\\u001F@) to delimit namespace
-- components in URL paths.
namespaceSegment :: Namespace -> ByteString
namespaceSegment ns =
  TE.encodeUtf8 (T.intercalate "\x1F" (V.toList ns))

doRequest
  :: forall a. Aeson.FromJSON a
  => CatalogClient -> HT.Method -> ByteString -> Maybe BL.ByteString -> IO a
doRequest cc method path body = do
  resp <- runRequest cc method path body
  case Aeson.eitherDecode' (HTTP.responseBody resp) of
    Right v  -> pure v
    Left err -> throwIO (ClientDecodeError err (BL.toStrict (HTTP.responseBody resp)))

doRequest_
  :: CatalogClient -> HT.Method -> ByteString -> Maybe BL.ByteString -> IO ()
doRequest_ cc method path body = do
  _ <- runRequest cc method path body
  pure ()

runRequest
  :: CatalogClient -> HT.Method -> ByteString -> Maybe BL.ByteString
  -> IO (HTTP.Response BL.ByteString)
runRequest cc method path body = do
  req0 <- HTTP.parseRequest (BSC.unpack (ccBaseUrl cc <> path))
  let req = applyAuth (ccAuth cc) req0
              { HTTP.method = method
              , HTTP.requestHeaders =
                  ("Accept", "application/json")
                  : maybe [] (const [("Content-Type", "application/json")]) body
                  ++ HTTP.requestHeaders req0
              , HTTP.requestBody = case body of
                  Just b  -> HTTP.RequestBodyLBS b
                  Nothing -> HTTP.RequestBodyBS BS.empty
              , HTTP.checkResponse = \_ _ -> pure ()
              }
  resp <- HTTP.httpLbs req (ccManager cc)
  let status = HT.statusCode (HTTP.responseStatus resp)
  if status >= 400
    then case Aeson.eitherDecode' (HTTP.responseBody resp) of
      Right (e :: CatalogError) -> throwIO (ClientCatalogError e)
      Left _ ->
        throwIO . ClientCatalogError $ CatalogError
          { ceMessage = "HTTP " <> T.pack (show status) <> " from catalog"
          , ceType    = "RESTException"
          , ceCode    = status
          }
    else pure resp

applyAuth :: AuthHeader -> HTTP.Request -> HTTP.Request
applyAuth NoAuth r = r
applyAuth (BearerToken tok) r =
  r { HTTP.requestHeaders =
        ("Authorization", "Bearer " <> tok)
        : HTTP.requestHeaders r }
applyAuth (RawHeader name val) r =
  r { HTTP.requestHeaders =
        (CI.mk name, val) : HTTP.requestHeaders r }

-- We don't actually export 'Map.empty' but importing the module is convenient
-- for callers that want to build their own JSON requests; tickle the
-- redundancy warning.
_unusedMap :: Map.Map () ()
_unusedMap = Map.empty
