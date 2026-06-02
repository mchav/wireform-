module Network.HTTP.Headers.ProxyAuthorization where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization
import Network.HTTP.Headers.HeaderFieldName (hProxyAuthorization)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype ProxyAuthorization = ProxyAuthorization {proxyAuthorizationCredentials :: Credentials}
  deriving stock (Eq, Show)


instance KnownHeader ProxyAuthorization where
  type ParseFailure ProxyAuthorization = String
  type Cardinality ProxyAuthorization = 'ZeroOrOne
  type Direction ProxyAuthorization = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser credentialsParser header of
      OK creds "" -> Right $ ProxyAuthorization creds
      OK _ rest -> Left $ "Unconsumed input after parsing Proxy-Authorization header: " <> show rest
      Fail -> Left "Failed to parse Proxy-Authorization header"
      Err e -> Left e


  renderToHeaders _ (ProxyAuthorization creds) = M.toStrictByteString $ renderCredentials creds


  headerName _ = hProxyAuthorization
