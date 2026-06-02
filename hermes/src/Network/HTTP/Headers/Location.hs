module Network.HTTP.Headers.Location (
  Location (..),
  locationParser,
  renderLocation,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hLocation)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Location header value containing a URI
newtype Location = Location {locationUri :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Location where
  type ParseFailure Location = String
  type Cardinality Location = 'ZeroOrOne
  type Direction Location = 'Response


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser locationParser header of
      OK location "" -> Right location
      OK _ rest -> Left $ "Unconsumed input after parsing Location header: " <> show rest
      Fail -> Left "Failed to parse Location header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderLocation


  headerName _ = hLocation


locationParser :: ParserT st String Location
locationParser = Location <$> takeRestShortText


renderLocation :: Location -> M.Builder
renderLocation (Location uri) = shortText uri
