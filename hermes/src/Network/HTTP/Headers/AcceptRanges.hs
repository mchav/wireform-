{-# LANGUAGE TemplateHaskell #-}
{- |
RFC 9110 §14.3 @Accept-Ranges@ — the server's advertisement of
which range-units it accepts (or the literal @none@).

== Grammar

@
Accept-Ranges     = acceptable-ranges
acceptable-ranges = 1#range-unit / \"none\"
range-unit        = bytes-unit / other-range-unit
@
-}
module Network.HTTP.Headers.AcceptRanges
  ( AcceptRanges (..)
  , acceptRangesParser
  , renderAcceptRanges
  ) where

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.Rendering.Util as R
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptRanges)
import Network.HTTP.Headers.Parsing.Util

-- | @AcceptRangesNone@ corresponds to the literal @\"none\"@ which
-- explicitly disables range requests (RFC 9110 §14.3). All other
-- values surface as a non-empty list of range-unit tokens.
data AcceptRanges
  = AcceptRangesNone
  | AcceptRangesUnits !(NonEmpty ST.ShortText)
  deriving stock (Eq, Show)

instance KnownHeader AcceptRanges where
  type ParseFailure AcceptRanges = String
  type Cardinality AcceptRanges = 'ZeroOrOne
  type Direction AcceptRanges = 'Response

  parseFromHeaders _ headers = case runParser acceptRangesParser (NE.head headers) of
    OK ar leftover
      | B.null (dropOws leftover) -> Right ar
      | otherwise ->
          Left ("Unconsumed input after parsing Accept-Ranges: " <> show leftover)
    Fail    -> Left "Failed to parse Accept-Ranges header"
    Err err -> Left err
    where dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)

  renderToHeaders _ = M.toStrictByteString . renderAcceptRanges

  headerName _ = hAcceptRanges

acceptRangesParser :: ParserT st String AcceptRanges
acceptRangesParser = do
  ows
  first <- rfc9110Token
  if ST.toString first == "none"
    then do
      ows
      pure AcceptRangesNone
    else do
      rest <- many (ows *> $(char ',') *> ows *> rfc9110Token)
      ows
      pure (AcceptRangesUnits (first :| rest))

renderAcceptRanges :: AcceptRanges -> M.Builder
renderAcceptRanges = \case
  AcceptRangesNone     -> "none"
  AcceptRangesUnits us ->
    M.intersperse ", " (map R.shortText (NE.toList us))
