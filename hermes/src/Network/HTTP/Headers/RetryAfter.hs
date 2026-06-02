module Network.HTTP.Headers.RetryAfter (
  RetryAfter (..),
  retryAfterParser,
  renderRetryAfter,
) where

import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hRetryAfter)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


data RetryAfter
  = DelaySeconds Word
  | SpecificTime UTCTime


instance KnownHeader RetryAfter where
  type ParseFailure RetryAfter = String
  type Cardinality RetryAfter = 'ZeroOrOne
  type Direction RetryAfter = 'Response


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser retryAfterParser header of
      OK ra "" -> Right ra
      OK _ rest -> Left $ "Unconsumed input after parsing Retry-After header: " <> show rest
      Fail -> Left "Failed to parse Retry-After header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderRetryAfter


  headerName _ = hRetryAfter


retryAfterParser :: ParserT st String RetryAfter
retryAfterParser =
  (DelaySeconds <$> anyAsciiDecimalWord)
    <|> (SpecificTime <$> dateParser)


renderRetryAfter :: RetryAfter -> M.Builder
renderRetryAfter = \case
  DelaySeconds seconds -> M.wordDec seconds
  SpecificTime time -> renderDate time
