{- |
RFC 9110 §11.7.1 @Proxy-Authenticate@.

The grammar is identical to @WWW-Authenticate@ (RFC 9110
§11.6.1); only the field name and HTTP-semantic role differ
(it's the proxy-tier challenge that pairs with @407 Proxy
Authentication Required@ instead of the origin-tier @401@).
We reuse the parser and renderer from
"Network.HTTP.Headers.WWWAuthenticate" wholesale and just wrap
the result in a distinct newtype so the 'KnownHeader' instance
can pin the right 'HeaderFieldName'.
-}
module Network.HTTP.Headers.ProxyAuthenticate
  ( ProxyAuthenticate (..)
  , proxyAuthenticateParser
  , renderProxyAuthenticate
    -- * Re-exports
  , AuthChallenge (..)
  , ChallengeContents (..)
  , AuthScheme (..)
  , CredentialParam (..)
  ) where

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProxyAuthenticate)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.WWWAuthenticate
  ( AuthChallenge (..)
  , ChallengeContents (..)
  , AuthScheme (..)
  , CredentialParam (..)
  , challengesParser
  , renderAuthChallenge
  )

-- | Challenge list for the proxy auth tier.
newtype ProxyAuthenticate = ProxyAuthenticate
  { proxyAuthChallenges :: [AuthChallenge]
  }
  deriving stock (Eq, Show)

instance KnownHeader ProxyAuthenticate where
  type ParseFailure ProxyAuthenticate = String
  type Cardinality ProxyAuthenticate = 'ZeroOrMore
  type Direction ProxyAuthenticate = 'Response

  parseFromHeaders _ headers = do
    challenges <- traverse parseOne (NE.toList headers)
    pure (ProxyAuthenticate (concat challenges))
    where
      parseOne hdr = case runParser challengesParser hdr of
        OK cs leftover
          | B.null (dropOws leftover) -> Right cs
          | otherwise ->
              Left ("Unconsumed input after parsing Proxy-Authenticate: " <> show leftover)
        Fail    -> Left "Failed to parse Proxy-Authenticate header"
        Err err -> Left err
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)

  renderToHeaders _ (ProxyAuthenticate cs) =
    [M.toStrictByteString (renderProxyAuthenticate (ProxyAuthenticate cs))]

  headerName _ = hProxyAuthenticate

proxyAuthenticateParser :: ParserT st String ProxyAuthenticate
proxyAuthenticateParser = ProxyAuthenticate <$> challengesParser

renderProxyAuthenticate :: ProxyAuthenticate -> M.Builder
renderProxyAuthenticate (ProxyAuthenticate cs) =
  M.intersperse ", " (map renderAuthChallenge cs)
