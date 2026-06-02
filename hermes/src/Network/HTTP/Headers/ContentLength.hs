module Network.HTTP.Headers.ContentLength (
  ContentLength (..),
  contentLengthParser,
  renderContentLength,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype ContentLength = ContentLength {contentLength :: Word}
  deriving stock (Eq, Show)


instance KnownHeader ContentLength where
  type ParseFailure ContentLength = String
  type Cardinality ContentLength = 'ZeroOrOne
  type Direction ContentLength = 'RequestAndResponse


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser contentLengthParser header of
      OK cl "" -> Right cl
      OK _ rest -> Left $ "Unconsumed input after parsing Content-Length header: " <> show rest
      Fail -> Left "Failed to parse Content-Length header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderContentLength


  headerName _ = hContentLength


contentLengthParser :: ParserT st String ContentLength
contentLengthParser = ContentLength <$> anyAsciiDecimalWord


renderContentLength :: ContentLength -> M.Builder
renderContentLength (ContentLength len) = M.wordDec len
