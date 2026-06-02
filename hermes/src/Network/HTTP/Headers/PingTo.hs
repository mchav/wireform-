module Network.HTTP.Headers.PingTo where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers (HeaderCardinality (..), HeaderIsRequestOrResponse (..), KnownHeader (..))
import Network.HTTP.Headers.HeaderFieldName (hPingTo)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype PingTo = PingTo {pingTo :: String}
  deriving stock (Eq, Show)


instance KnownHeader PingTo where
  type ParseFailure PingTo = String
  type Cardinality PingTo = 'ZeroOrOne
  type Direction PingTo = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser pingToParser header of
      OK pt "" -> Right pt
      OK _ rest -> Left $ "Unconsumed input after parsing Ping-To header: " <> show rest
      Fail -> Left "Failed to parse Ping-To header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderPingTo


  headerName _ = hPingTo


pingToParser :: ParserT st String PingTo
pingToParser = PingTo <$> takeRestString


renderPingTo :: PingTo -> M.Builder
renderPingTo (PingTo str) = M.string8 str
