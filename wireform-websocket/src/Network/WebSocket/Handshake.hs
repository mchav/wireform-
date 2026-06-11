{-# LANGUAGE OverloadedStrings #-}

{- | RFC 6455 \u00a74 handshake.

The handshake is a normal HTTP\/1.1 GET request with a few
distinguishing headers.  Once both sides have validated the
request \/ response, the underlying byte stream is no longer
HTTP and the rest of the protocol is framed using
"Network.WebSocket.Frame".

This module provides:

* 'WebSocketRequest' \/ 'WebSocketResponse' shapes built on top of
  the wireform-http 'Request' \/ 'Response' types.
* 'serverAccept' \u2014 validate an incoming wireform-http
  'Request', compute the @Sec-WebSocket-Accept@ value, and
  produce the 'Response' to send back.
* 'clientHandshake' \u2014 roll a fresh @Sec-WebSocket-Key@,
  emit the request bytes on the wire, parse the @101@ reply,
  and verify the @Sec-WebSocket-Accept@ field.

Both sides use 'Wireform.Base64' for the SHA-1 \u2192 base64
step \u2014 the same SIMD implementation every other wireform
format reaches for.
-}
module Network.WebSocket.Handshake (
  -- * Validation
  HandshakeError (..),
  isWebSocketRequest,

  -- * Server side
  WebSocketRequest (..),
  parseWebSocketRequest,
  serverAccept,
  computeAccept,
  webSocketGuid,

  -- * Client side
  WebSocketHandshakeOpts (..),
  defaultWebSocketHandshakeOpts,
  buildClientHandshake,
  verifyServerHandshake,
  generateKey,

  -- * Header helpers
  splitTokenList,
) where

import Control.Exception (Exception)
import Crypto.Hash (SHA1 (..), hashWith)
import Data.Bifunctor qualified
import Data.Bits (shiftR)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Word (Word64, Word8)
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import Network.HTTP.Types.Version qualified as V
import Network.HTTP1.Encode qualified as H1E
import Network.HTTP1.Method qualified as H1M
import Network.HTTP1.Types qualified as H1
import System.Random.Stateful qualified as Rnd
import Wireform.Base64 (decodeBase64, encodeBase64)


------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

data HandshakeError
  = HandshakeBadMethod !ByteString
  | HandshakeMissingHeader !H.HeaderName
  | HandshakeBadUpgrade !ByteString
  | HandshakeBadConnection !ByteString
  | HandshakeBadVersion !ByteString
  | HandshakeBadKey !ByteString
  | HandshakeBadStatus !Int
  | -- | @expected@, @got@.
    HandshakeBadAcceptMismatch !ByteString !ByteString
  deriving stock (Eq, Show)


instance Exception HandshakeError


------------------------------------------------------------------------
-- Sec-WebSocket-Accept
------------------------------------------------------------------------

{- | The fixed GUID from RFC 6455 \u00a74.2.2 used to derive
@Sec-WebSocket-Accept@ from @Sec-WebSocket-Key@.
-}
webSocketGuid :: ByteString
webSocketGuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"


-- | @Sec-WebSocket-Accept = base64(SHA-1(key <> guid))@.
computeAccept :: ByteString -> ByteString
computeAccept key =
  let digest = hashWith SHA1 (key <> webSocketGuid)
  in encodeBase64 (BA.convert digest)


------------------------------------------------------------------------
-- Header sniff
------------------------------------------------------------------------

{- | Cheap check used by routers \/ upgrade-aware HTTP handlers to
decide whether to dispatch a request to the WebSocket subsystem.
Looks for @Upgrade: websocket@ + @Connection: upgrade@ (both
case-insensitive).
-}
isWebSocketRequest :: Request -> Bool
isWebSocketRequest req =
  let hdrs = requestHeaders req
      isUpgrade =
        any
          (containsToken "websocket")
          (H.lookupHeaders H.hUpgrade hdrs)
      isConnUpg =
        any
          (containsToken "upgrade")
          (H.lookupHeaders H.hConnection hdrs)
  in requestMethod req == "GET" && isUpgrade && isConnUpg


------------------------------------------------------------------------
-- Server side
------------------------------------------------------------------------

{- | The slice of the incoming HTTP request the WebSocket layer
actually cares about \u2014 a flattened view of what the
handshake parser found in 'Request'.
-}
data WebSocketRequest = WebSocketRequest
  { wsReqTarget :: !ByteString
  , wsReqAuthority :: !(Maybe ByteString)
  , wsReqKey :: !ByteString
  , wsReqVersion :: !ByteString
  , wsReqProtocols :: ![ByteString]
  -- ^ Values of every @Sec-WebSocket-Protocol@ header, in order.
  , wsReqExtensions :: ![ByteString]
  -- ^ Values of every @Sec-WebSocket-Extensions@ header, in order.
  , wsReqOrigin :: !(Maybe ByteString)
  }
  deriving stock (Eq, Show)


{- | Validate an incoming 'Request' against RFC 6455 \u00a74.2.1.
Returns the digest of header fields the WebSocket layer needs.
-}
parseWebSocketRequest :: Request -> Either HandshakeError WebSocketRequest
parseWebSocketRequest req = do
  let methodBs = M.fromMethod (requestMethod req)
  if methodBs /= "GET"
    then Left (HandshakeBadMethod methodBs)
    else Right ()
  let hdrs = requestHeaders req
  upg <- requireHeader H.hUpgrade hdrs
  con <- requireHeader H.hConnection hdrs
  ver <- requireHeader secWebSocketVersionH hdrs
  key <- requireHeader secWebSocketKeyH hdrs
  if not (containsToken "websocket" upg)
    then Left (HandshakeBadUpgrade upg)
    else Right ()
  if not (containsToken "upgrade" con)
    then Left (HandshakeBadConnection con)
    else Right ()
  if ver /= "13"
    then Left (HandshakeBadVersion ver)
    else Right ()
  case decodeBase64 key of
    Just decoded | BS.length decoded == 16 -> Right ()
    _ -> Left (HandshakeBadKey key)
  pure
    WebSocketRequest
      { wsReqTarget = requestTarget req
      , wsReqAuthority = requestAuthority req
      , wsReqKey = key
      , wsReqVersion = ver
      , wsReqProtocols =
          splitTokenList
            (H.lookupHeaders secWebSocketProtocolH hdrs)
      , wsReqExtensions =
          splitTokenList
            (H.lookupHeaders secWebSocketExtensionsH hdrs)
      , wsReqOrigin = H.lookupHeader (CI.mk "Origin") hdrs
      }
  where
    requireHeader name hs = case H.lookupHeader name hs of
      Just v -> Right v
      Nothing -> Left (HandshakeMissingHeader name)


{- | Build the 101 'Response' that completes the server side of the
handshake.  The selected sub-protocol (if any) is written
verbatim into the response @Sec-WebSocket-Protocol@ header;
callers are responsible for ensuring the choice came from the
client's offered list ('wsReqProtocols') \u2014 the standalone
server's 'wscSelectSubProtocol' callback is the typical
choosing site.
-}
serverAccept
  :: WebSocketRequest
  -> Maybe ByteString
  -- ^ selected sub-protocol.
  -> Response
serverAccept req mSelectedProto =
  let acceptVal = computeAccept (wsReqKey req)
      protoHdr = case mSelectedProto of
        Just p -> [(secWebSocketProtocolH, p)]
        Nothing -> []
  in Response
       { responseStatus = S.status101
       , responseVersion = V.HTTP1_1
       , responseHeaders =
           [ (H.hUpgrade, "websocket")
           , (H.hConnection, "Upgrade")
           , (secWebSocketAcceptH, acceptVal)
           ]
             <> protoHdr
       , responseBody = BodyEmpty
       , responseTrailers = pure []
       , responseH2StreamId = 0
       , responseCancel = pure ()
       , responsePushPromises = pure []
       }


------------------------------------------------------------------------
-- Client side
------------------------------------------------------------------------

data WebSocketHandshakeOpts = WebSocketHandshakeOpts
  { wsOptTarget :: !ByteString
  -- ^ Request target, e.g. @"/chat"@.
  , wsOptAuthority :: !ByteString
  -- ^ @Host@ value, e.g. @"example.com"@.
  , wsOptScheme :: !Scheme
  -- ^ @SchemeHttps@ for @wss:\/\/@, otherwise @SchemeHttp@.
  , wsOptProtocols :: ![ByteString]
  -- ^ @Sec-WebSocket-Protocol@ values to advertise.
  , wsOptExtensions :: ![ByteString]
  -- ^ @Sec-WebSocket-Extensions@ values to advertise.
  , wsOptOrigin :: !(Maybe ByteString)
  {- ^ Optional @Origin@ to send (browsers always do; native
  clients usually do not need to).
  -}
  , wsOptExtraHeaders :: ![H.Header]
  -- ^ Anything else (auth tokens, cookies, etc.).
  }
  deriving stock (Show)


defaultWebSocketHandshakeOpts
  :: ByteString
  -- ^ target
  -> ByteString
  -- ^ authority
  -> WebSocketHandshakeOpts
defaultWebSocketHandshakeOpts t a =
  WebSocketHandshakeOpts
    { wsOptTarget = t
    , wsOptAuthority = a
    , wsOptScheme = SchemeHttp
    , wsOptProtocols = []
    , wsOptExtensions = []
    , wsOptOrigin = Nothing
    , wsOptExtraHeaders = []
    }


{- | Roll a fresh 16-byte @Sec-WebSocket-Key@ and base64-encode it
(RFC 6455 \u00a74.1).  Uses the system splitmix; the value is
only ever inspected by the server for round-tripping, so a CSPRNG
is not required.
-}
generateKey :: IO ByteString
generateKey = do
  hi <- Rnd.uniformM Rnd.globalStdGen :: IO Word64
  lo <- Rnd.uniformM Rnd.globalStdGen :: IO Word64
  pure (encodeBase64 (word64sToBs hi lo))
  where
    word64sToBs :: Word64 -> Word64 -> ByteString
    word64sToBs a b =
      BS.pack
        [ byteOf a 7
        , byteOf a 6
        , byteOf a 5
        , byteOf a 4
        , byteOf a 3
        , byteOf a 2
        , byteOf a 1
        , byteOf a 0
        , byteOf b 7
        , byteOf b 6
        , byteOf b 5
        , byteOf b 4
        , byteOf b 3
        , byteOf b 2
        , byteOf b 1
        , byteOf b 0
        ]
    byteOf w n = fromIntegral (w `shiftR` (8 * n)) :: Word8


{- | Render the client's HTTP\/1.1 request line + header block via
'Network.HTTP1.Encode.encodeRequestHead'.  Returns the byte
string to send and the @Sec-WebSocket-Key@ value that the
server's reply should round-trip to a matching
@Sec-WebSocket-Accept@.

This is the raw shape an upgrade-aware low-level client wants;
if you already have a wireform-http connection in hand, prefer
'Network.HTTP.Upgrade.sendUpgrade' and pass the headers in.
-}
buildClientHandshake :: WebSocketHandshakeOpts -> IO (ByteString, ByteString)
buildClientHandshake opts = do
  key <- generateKey
  let baseHdrs =
        [ ("Host", wsOptAuthority opts)
        , ("Upgrade", "websocket")
        , ("Connection", "Upgrade")
        , ("Sec-WebSocket-Key", key)
        , ("Sec-WebSocket-Version", "13")
        ]
      protoHdrs = case wsOptProtocols opts of
        [] -> []
        xs -> [("Sec-WebSocket-Protocol", BS.intercalate ", " xs)]
      extHdrs = case wsOptExtensions opts of
        [] -> []
        xs -> [("Sec-WebSocket-Extensions", BS.intercalate ", " xs)]
      originHdr = case wsOptOrigin opts of
        Just o -> [("Origin", o)]
        Nothing -> []
      h1Hdrs =
        baseHdrs
          <> protoHdrs
          <> extHdrs
          <> originHdr
          <> map
            (Data.Bifunctor.first CI.original)
            (wsOptExtraHeaders opts)
      h1req =
        H1.Request
          { H1.requestMethod = H1M.GET
          , H1.requestTarget = wsOptTarget opts
          , H1.requestVersion = H1.HTTP_1_1
          , H1.requestHeaders = h1Hdrs
          , H1.requestBody = H1.BodyEmpty
          , H1.requestTrailers = pure []
          }
  pure (H1E.encodeRequestHead h1req, key)


{- | Validate a server's 101 reply.  @rawHeadBlock@ is the
response head as bytes (without the trailing @\\r\\n@), e.g.
the @uaRequestHead@ from 'Network.HTTP.Upgrade.acceptUpgrade'\'s
inverse on the client side.
-}
verifyServerHandshake
  :: ByteString
  -- ^ the @Sec-WebSocket-Key@ we sent
  -> Int
  -- ^ HTTP status code received
  -> [H.Header]
  -- ^ response headers
  -> Either HandshakeError ()
verifyServerHandshake key code hdrs = do
  if code /= 101
    then Left (HandshakeBadStatus code)
    else Right ()
  acc <- case H.lookupHeader secWebSocketAcceptH hdrs of
    Just v -> Right v
    Nothing -> Left (HandshakeMissingHeader secWebSocketAcceptH)
  let expected = computeAccept key
  if acc /= expected
    then Left (HandshakeBadAcceptMismatch expected acc)
    else Right ()


------------------------------------------------------------------------
-- Header names
------------------------------------------------------------------------

secWebSocketKeyH
  , secWebSocketAcceptH
  , secWebSocketVersionH
  , secWebSocketProtocolH
  , secWebSocketExtensionsH
    :: H.HeaderName
secWebSocketKeyH = CI.mk "Sec-WebSocket-Key"
secWebSocketAcceptH = CI.mk "Sec-WebSocket-Accept"
secWebSocketVersionH = CI.mk "Sec-WebSocket-Version"
secWebSocketProtocolH = CI.mk "Sec-WebSocket-Protocol"
secWebSocketExtensionsH = CI.mk "Sec-WebSocket-Extensions"


------------------------------------------------------------------------
-- Header value helpers
------------------------------------------------------------------------

{- | Flatten one or more header values that each carry a
comma-separated token list into a single concatenated list of
trimmed tokens.  RFC 9110 \u00a75.6.1 / RFC 6455 \u00a74.1.
-}
splitTokenList :: [ByteString] -> [ByteString]
splitTokenList = concatMap splitOne
  where
    splitOne v = filter (not . BS.null) (map stripOws (BS.split 0x2C v))
    stripOws = BS.dropWhile isOws . dropTrailingOws
    isOws b = b == 0x20 || b == 0x09
    dropTrailingOws s =
      let !n = BS.length s
          go i
            | i <= 0 = BS.empty
            | isOws (BS.index s (i - 1)) = go (i - 1)
            | otherwise = BS.take i s
      in go n


{- | Case-insensitive comma-separated token membership.  RFC 9110
\u00a75.6.1 grammar (with the OWS-stripping that 6.1 says
recipients SHOULD tolerate).
-}
containsToken :: ByteString -> ByteString -> Bool
containsToken needle haystack =
  any
    (\t -> CI.mk (strip t) == CI.mk needle)
    (BS.split 0x2C {- ',' -} haystack)
  where
    strip = BS.dropWhile isOws . dropWhileEnd isOws
    isOws b = b == 0x20 || b == 0x09
    dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
    dropWhileEnd p bs =
      let !n = BS.length bs
          go i
            | i <= 0 = BS.empty
            | p (BS.index bs (i - 1)) = go (i - 1)
            | otherwise = BS.take i bs
      in go n
