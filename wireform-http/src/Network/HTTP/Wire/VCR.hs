{- | VCR-style cassettes for recording and replaying HTTP interactions.

Cassettes serialise to YAML via @wireform-yaml@. The format is human-
readable on purpose — cassettes go in version control, so reviewers
can sanity-check them.

Workflow:

* First run (against a real server):

@
recordSession baseTransport "fixtures\/login.yaml" \\transport -\> do
  runMyClient transport
@

* Subsequent runs (in CI):

@
cassette  <- loadCassette "fixtures\/login.yaml"
transport <- replayTransport cassette sequential
runMyClient transport
@

The mock\/replay 'Transport' shares 'RecordedRequest' \/
'RecordedResponse' with "Network.HTTP.Wire.Test" so VCR cassettes
and request-log assertions speak the same vocabulary.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Wire.VCR
  ( -- * Cassettes
    Cassette (..)
  , Interaction (..)
  , RecordedRequest (..)
  , RecordedResponse (..)
  , RecordedHeader (..)
  , RecordedMethod (..)
  , loadCassette
  , saveCassette
    -- * Recording
  , recordSession
  , withRecording
    -- * Replay
  , replayTransport
  , MatchStrategy (..)
  , sequential
  , byMethodAndURI
  , byMethodURIAndHeaders
  , customStrategy
  , NoMatchingInteraction (..)
    -- * Sanitization
  , Sanitizer (..)
  , redactHeaders
  , redactBodyPattern
  , applySanitizer
  ) where

import Control.Exception (Exception, finally, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Status as S

import qualified YAML.Class as Y

import Network.HTTP.Wire.BodyStream
import Network.HTTP.Wire.Protocol
import qualified Network.HTTP.Wire.Request as WReq
import Network.HTTP.Wire.Response
import Network.HTTP.Wire.Transport
import Network.HTTP.Wire.URI (requestURIToText)

-- ---------------------------------------------------------------------------
-- Cassette data
-- ---------------------------------------------------------------------------

newtype RecordedMethod = RecordedMethod { unRecordedMethod :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)

methodToRec :: M.Method -> RecordedMethod
methodToRec = RecordedMethod . TE.decodeUtf8 . M.fromMethod

-- | A header pair as recorded. We avoid 'CI.CI' in the on-disk shape
-- because @wireform-yaml@'s 'Generic' deriver doesn't know about it.
data RecordedHeader = RecordedHeader
  { rhName  :: !Text
  , rhValue :: !Text
    -- ^ Base64-encoded if the original bytes aren't UTF-8;
    --   plain text otherwise. The 'rhBinary' flag disambiguates.
  , rhBinary :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)

headerToRec :: H.Header -> RecordedHeader
headerToRec (n, v) = case TE.decodeUtf8' v of
  Right t -> RecordedHeader (TE.decodeUtf8 (CI.original n)) t False
  Left  _ -> RecordedHeader
              (TE.decodeUtf8 (CI.original n))
              (TE.decodeUtf8 (B64.encode v))
              True

headerFromRec :: RecordedHeader -> H.Header
headerFromRec h =
  let nameBs = TE.encodeUtf8 (rhName h)
      bytes
        | rhBinary h = fromMaybe (TE.encodeUtf8 (rhValue h))
                                 (eitherToMaybe (B64.decode (TE.encodeUtf8 (rhValue h))))
        | otherwise  = TE.encodeUtf8 (rhValue h)
  in (CI.mk nameBs, bytes)
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe = either (const Nothing) Just

data RecordedRequest = RecordedRequest
  { rrqMethod  :: !RecordedMethod
  , rrqURI     :: !Text
  , rrqHeaders :: ![RecordedHeader]
  , rrqBody    :: !Text
  , rrqBodyBinary :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)

data RecordedResponse = RecordedResponse
  { rrsStatus  :: !Int
  , rrsHeaders :: ![RecordedHeader]
  , rrsBody    :: !Text
  , rrsBodyBinary :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)

data Interaction = Interaction
  { interactionRequest  :: !RecordedRequest
  , interactionResponse :: !RecordedResponse
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)

data Cassette = Cassette
  { cassetteRecordedAt  :: !Text
    -- ^ ISO-8601 timestamp. Stringly typed in the cassette so
    -- consumers don\'t need a 'Day' \/ 'UTCTime' YAML instance.
  , cassetteMetadata    :: ![(Text, Text)]
  , cassetteInteractions :: ![Interaction]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)

newCassette :: UTCTime -> [Interaction] -> Cassette
newCassette ts xs = Cassette
  { cassetteRecordedAt   = T.pack (show ts)
  , cassetteMetadata     = []
  , cassetteInteractions = xs
  }

bodyToRec :: ByteString -> (Text, Bool)
bodyToRec bs = case TE.decodeUtf8' bs of
  Right t -> (t, False)
  Left _  -> (TE.decodeUtf8 (B64.encode bs), True)

bodyFromRec :: Text -> Bool -> ByteString
bodyFromRec t False = TE.encodeUtf8 t
bodyFromRec t True  = case B64.decode (TE.encodeUtf8 t) of
  Right bs -> bs
  Left _   -> TE.encodeUtf8 t

loadCassette :: FilePath -> IO Cassette
loadCassette path = do
  bs <- BS.readFile path
  case Y.decodeYAMLBS bs of
    Right c  -> pure c
    Left err -> throwIO (CassetteParseError path err)

saveCassette :: FilePath -> Cassette -> IO ()
saveCassette path c = BS.writeFile path (Y.encodeYAMLBS c)

data CassetteError = CassetteParseError !FilePath !String
  deriving stock (Show)
instance Exception CassetteError

-- ---------------------------------------------------------------------------
-- Recording
-- ---------------------------------------------------------------------------

-- | Wrap a transport so that every interaction is appended to the
-- given ref. Drains both bodies through a strict buffer so the
-- downstream callee still sees a popper.
withRecording :: IORef [Interaction] -> Middleware IO
withRecording ref inner = Transport $ \req -> do
  reqBody <- bodyStreamBytes (WReq.body req)
  rebuilt <- streamFromStrict reqBody
  let req' = req { WReq.body = rebuilt }
  raw <- sendRaw inner req'
  respBody <- popperBytes (bodyPopper raw)
  let recRq = toRecordedRequest req' reqBody
      recRs = toRecordedResponse raw respBody
  modifyIORef' ref (<> [Interaction recRq recRs])
  newPopper <- popperFromStrict respBody
  pure raw { bodyPopper = newPopper }

-- | Record a session: wraps the action with a recording transport,
-- writes the resulting cassette to disk on success.
recordSession :: Transport IO -> FilePath -> (Transport IO -> IO a) -> IO a
recordSession real path action = do
  ref <- newIORef []
  let transport = withRecording ref real
  result <- action transport `finally`
    (do interactions <- readIORef ref
        now <- getCurrentTime
        saveCassette path (newCassette now interactions))
  pure result

toRecordedRequest :: WReq.Request BodyStream -> ByteString -> RecordedRequest
toRecordedRequest req bs =
  let (body_, binary) = bodyToRec bs
  in RecordedRequest
       { rrqMethod  = methodToRec (WReq.method req)
       , rrqURI     = requestURIToText (WReq.requestURI req)
       , rrqHeaders = map headerToRec (WReq.headers req)
       , rrqBody    = body_
       , rrqBodyBinary = binary
       }

toRecordedResponse :: RawResponse -> ByteString -> RecordedResponse
toRecordedResponse raw bs =
  let (body_, binary) = bodyToRec bs
  in RecordedResponse
       { rrsStatus  = fromIntegral (S.statusCode (statusCode raw))
       , rrsHeaders = map headerToRec (Network.HTTP.Wire.Response.headers raw)
       , rrsBody    = body_
       , rrsBodyBinary = binary
       }

-- ---------------------------------------------------------------------------
-- Replay
-- ---------------------------------------------------------------------------

data MatchStrategy = MatchStrategy
  { strategyName :: !Text
  , matchInteraction
      :: WReq.Request BodyStream
      -> [Interaction]
      -> IO (Maybe (Interaction, [Interaction]))
  }

-- | Replay a cassette as a standalone transport. The transport
-- maintains its own cursor; once exhausted (or on a non-match) it
-- throws 'NoMatchingInteraction'.
replayTransport :: Cassette -> MatchStrategy -> IO (Transport IO)
replayTransport c strat = do
  ref <- newIORef (cassetteInteractions c)
  pure $ Transport $ \req -> do
    interactions <- readIORef ref
    matchResult <- matchInteraction strat req interactions
    case matchResult of
      Nothing -> do
        snapshot <- toRecRequestSnapshot req
        throwIO (NoMatchingInteraction snapshot (strategyName strat))
      Just (interaction, rest) -> do
        writeIORef ref rest
        toRawResponse (interactionResponse interaction)

toRecRequestSnapshot :: WReq.Request BodyStream -> IO RecordedRequest
toRecRequestSnapshot req = do
  bodyBs <- bodyStreamBytes (WReq.body req)
  pure (toRecordedRequest req bodyBs)

toRawResponse :: RecordedResponse -> IO RawResponse
toRawResponse r = do
  popper <- popperFromStrict (bodyFromRec (rrsBody r) (rrsBodyBinary r))
  pure RawResponse
    { statusCode   = S.Status (fromIntegral (rrsStatus r))
    , headers      = map headerFromRec (rrsHeaders r)
    , bodyPopper   = popper
    , protocolInfo = HTTP1_1
    }

-- | Strict sequential matching: each request must match the next
-- interaction in the cassette.
sequential :: MatchStrategy
sequential = MatchStrategy "sequential" $ \req interactions -> case interactions of
  [] -> pure Nothing
  (i : rest) -> do
    bodyBs <- bodyStreamBytes (WReq.body req)
    let rec' = toRecordedRequest req bodyBs
    if rrqMethod (interactionRequest i) == rrqMethod rec'
       && rrqURI (interactionRequest i) == rrqURI rec'
      then pure (Just (i, rest))
      else pure Nothing

-- | Match by method + URI, ignoring order. The matched interaction is
-- removed from the remaining list.
byMethodAndURI :: MatchStrategy
byMethodAndURI = MatchStrategy "byMethodAndURI" $ \req interactions -> do
  bodyBs <- bodyStreamBytes (WReq.body req)
  let rec' = toRecordedRequest req bodyBs
      pred_ i =
        rrqMethod (interactionRequest i) == rrqMethod rec'
        && rrqURI (interactionRequest i) == rrqURI rec'
  pure (extractFirst pred_ interactions)

-- | Like 'byMethodAndURI' but additionally requires that the given
-- header names match in value.
byMethodURIAndHeaders :: [H.HeaderName] -> MatchStrategy
byMethodURIAndHeaders names = MatchStrategy "byMethodURIAndHeaders" $ \req interactions -> do
  bodyBs <- bodyStreamBytes (WReq.body req)
  let rec' = toRecordedRequest req bodyBs
      headersOf rs =
        [ (TE.encodeUtf8 (rhName h), TE.encodeUtf8 (rhValue h))
        | h <- rs
        ]
      pred_ i =
        rrqMethod (interactionRequest i) == rrqMethod rec'
        && rrqURI (interactionRequest i) == rrqURI rec'
        && all (\n ->
                 let want = lookup (CI.original n) (headersOf (rrqHeaders rec'))
                     have = lookup (CI.original n) (headersOf (rrqHeaders (interactionRequest i)))
                 in want == have)
               names
  pure (extractFirst pred_ interactions)

-- | Build a custom strategy.
customStrategy
  :: Text
  -> (RecordedRequest -> Interaction -> Bool)
  -> MatchStrategy
customStrategy nm p = MatchStrategy nm $ \req interactions -> do
  bodyBs <- bodyStreamBytes (WReq.body req)
  let rec' = toRecordedRequest req bodyBs
  pure (extractFirst (p rec') interactions)

extractFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
extractFirst p xs = case break p xs of
  (_, [])     -> Nothing
  (l, m : r)  -> Just (m, l ++ r)

data NoMatchingInteraction = NoMatchingInteraction RecordedRequest Text
  deriving stock (Show)

instance Exception NoMatchingInteraction

-- ---------------------------------------------------------------------------
-- Sanitization
-- ---------------------------------------------------------------------------

data Sanitizer = Sanitizer
  { sanitizeRequest  :: RecordedRequest -> RecordedRequest
  , sanitizeResponse :: RecordedResponse -> RecordedResponse
  }

instance Semigroup Sanitizer where
  a <> b = Sanitizer
    { sanitizeRequest  = sanitizeRequest a . sanitizeRequest b
    , sanitizeResponse = sanitizeResponse a . sanitizeResponse b
    }

instance Monoid Sanitizer where
  mempty = Sanitizer id id

-- | Replace the values of the given header names with @REDACTED@ on
-- both requests and responses.
redactHeaders :: [H.HeaderName] -> Sanitizer
redactHeaders names = Sanitizer redactReq redactResp
  where
    nameTexts = map (TE.decodeUtf8 . CI.foldedCase) names
    redactPair p =
      if T.toCaseFold (rhName p) `elem` map T.toCaseFold nameTexts
        then p { rhValue = "REDACTED", rhBinary = False }
        else p
    redactReq r = r { rrqHeaders = map redactPair (rrqHeaders r) }
    redactResp r = r { rrsHeaders = map redactPair (rrsHeaders r) }

-- | Replace every occurrence of a byte pattern in request and response
-- bodies. Only operates on text bodies (binary stays intact).
redactBodyPattern :: ByteString -> ByteString -> Sanitizer
redactBodyPattern needle replacement = Sanitizer fixReq fixResp
  where
    nT = TE.decodeUtf8 needle
    rT = TE.decodeUtf8 replacement
    fixT t = T.intercalate rT (T.splitOn nT t)
    fixReq r
      | rrqBodyBinary r = r
      | otherwise       = r { rrqBody = fixT (rrqBody r) }
    fixResp r
      | rrsBodyBinary r = r
      | otherwise       = r { rrsBody = fixT (rrsBody r) }

applySanitizer :: Sanitizer -> Cassette -> Cassette
applySanitizer s c = c
  { cassetteInteractions =
      [ Interaction (sanitizeRequest s rq) (sanitizeResponse s rs)
      | Interaction rq rs <- cassetteInteractions c
      ]
  }
