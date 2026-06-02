module Network.HTTP.Headers.ContentType (
  ContentType (..),
  contentTypeParser,
  renderContentType,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.ContentNegotiation (MediaType, mediaTypeParser, renderMediaType)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentType)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype ContentType = ContentType {contentType :: MediaType}
  deriving stock (Eq, Show)


instance KnownHeader ContentType where
  type ParseFailure ContentType = String
  type Cardinality ContentType = 'ZeroOrOne
  type Direction ContentType = 'RequestAndResponse


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser contentTypeParser header of
      OK ct "" -> Right ct
      OK _ rest -> Left $ "Unconsumed input after parsing Content-Type header: " <> show rest
      Fail -> Left "Failed to parse Content-Type header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderContentType


  headerName _ = hContentType


contentTypeParser :: ParserT st String ContentType
contentTypeParser = ContentType <$> mediaTypeParser


renderContentType :: ContentType -> M.Builder
renderContentType (ContentType mt) = renderMediaType mt
