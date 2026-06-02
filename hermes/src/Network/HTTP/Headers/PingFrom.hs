module Network.HTTP.Headers.PingFrom where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPingFrom)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype PingFrom = PingFrom {pingFrom :: String}
  deriving stock (Eq, Show)


instance KnownHeader PingFrom where
  type ParseFailure PingFrom = String
  type Cardinality PingFrom = 'ZeroOrOne
  type Direction PingFrom = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser pingFromParser header of
      OK pf "" -> Right pf
      OK _ rest -> Left $ "Unconsumed input after parsing Ping-From header: " <> show rest
      Fail -> Left "Failed to parse Ping-From header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderPingFrom


  headerName _ = hPingFrom


pingFromParser :: ParserT st String PingFrom
pingFromParser = PingFrom <$> takeRestString


renderPingFrom :: PingFrom -> M.Builder
renderPingFrom (PingFrom str) = M.string8 str
