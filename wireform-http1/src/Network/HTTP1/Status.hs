{- | HTTP status codes (RFC 9110 § 15).

We use a single @newtype Status = Status Word16@ rather than enumerate
every IANA-registered code; pattern synonyms in scope cover the common
cases without forcing constructor allocation on the hot encode\/decode
path. Reason phrases are not stored in 'Status' itself — the encoder
maps standard codes to standard reason phrases via 'statusReason' and
servers can override per-response via the encoder API.
-}
{-# LANGUAGE PatternSynonyms #-}
module Network.HTTP1.Status
  ( Status (..)
  , statusReason
  , statusCategory
  , StatusCategory (..)
    -- * Common codes
  , pattern Continue, pattern SwitchingProtocols
  , pattern OK, pattern Created, pattern Accepted, pattern NoContent
  , pattern PartialContent, pattern MovedPermanently, pattern Found
  , pattern SeeOther, pattern NotModified, pattern TemporaryRedirect
  , pattern PermanentRedirect, pattern BadRequest, pattern Unauthorized
  , pattern Forbidden, pattern NotFound, pattern MethodNotAllowed
  , pattern NotAcceptable, pattern RequestTimeout, pattern Conflict
  , pattern Gone, pattern LengthRequired, pattern PayloadTooLarge
  , pattern UriTooLong, pattern UnsupportedMediaType
  , pattern RangeNotSatisfiable, pattern ExpectationFailed
  , pattern UpgradeRequired, pattern PreconditionFailed
  , pattern PreconditionRequired, pattern TooManyRequests
  , pattern RequestHeaderFieldsTooLarge
  , pattern InternalServerError, pattern NotImplemented
  , pattern BadGateway, pattern ServiceUnavailable
  , pattern GatewayTimeout, pattern HttpVersionNotSupported
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Word (Word16)
import GHC.Generics (Generic)

newtype Status = Status { statusCode :: Word16 }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show)

instance NFData Status

data StatusCategory
  = Informational
  | Success
  | Redirection
  | ClientError
  | ServerError
  | Other
  deriving stock (Eq, Show, Generic)

instance NFData StatusCategory

{-# INLINE statusCategory #-}
statusCategory :: Status -> StatusCategory
statusCategory (Status w)
  | w >= 100 && w < 200 = Informational
  | w >= 200 && w < 300 = Success
  | w >= 300 && w < 400 = Redirection
  | w >= 400 && w < 500 = ClientError
  | w >= 500 && w < 600 = ServerError
  | otherwise = Other

-- | The IANA-registered reason phrase for a known status code.
-- Unknown codes get a generic phrase scoped by category so that the
-- output remains a well-formed status line.
statusReason :: Status -> ByteString
statusReason (Status w) = case w of
  100 -> "Continue"
  101 -> "Switching Protocols"
  102 -> "Processing"
  103 -> "Early Hints"
  200 -> "OK"
  201 -> "Created"
  202 -> "Accepted"
  203 -> "Non-Authoritative Information"
  204 -> "No Content"
  205 -> "Reset Content"
  206 -> "Partial Content"
  207 -> "Multi-Status"
  208 -> "Already Reported"
  226 -> "IM Used"
  300 -> "Multiple Choices"
  301 -> "Moved Permanently"
  302 -> "Found"
  303 -> "See Other"
  304 -> "Not Modified"
  305 -> "Use Proxy"
  307 -> "Temporary Redirect"
  308 -> "Permanent Redirect"
  400 -> "Bad Request"
  401 -> "Unauthorized"
  402 -> "Payment Required"
  403 -> "Forbidden"
  404 -> "Not Found"
  405 -> "Method Not Allowed"
  406 -> "Not Acceptable"
  407 -> "Proxy Authentication Required"
  408 -> "Request Timeout"
  409 -> "Conflict"
  410 -> "Gone"
  411 -> "Length Required"
  412 -> "Precondition Failed"
  413 -> "Content Too Large"
  414 -> "URI Too Long"
  415 -> "Unsupported Media Type"
  416 -> "Range Not Satisfiable"
  417 -> "Expectation Failed"
  418 -> "I'm a teapot"
  421 -> "Misdirected Request"
  422 -> "Unprocessable Content"
  423 -> "Locked"
  424 -> "Failed Dependency"
  425 -> "Too Early"
  426 -> "Upgrade Required"
  428 -> "Precondition Required"
  429 -> "Too Many Requests"
  431 -> "Request Header Fields Too Large"
  451 -> "Unavailable For Legal Reasons"
  500 -> "Internal Server Error"
  501 -> "Not Implemented"
  502 -> "Bad Gateway"
  503 -> "Service Unavailable"
  504 -> "Gateway Timeout"
  505 -> "HTTP Version Not Supported"
  506 -> "Variant Also Negotiates"
  507 -> "Insufficient Storage"
  508 -> "Loop Detected"
  510 -> "Not Extended"
  511 -> "Network Authentication Required"
  _ -> case statusCategory (Status w) of
    Informational -> "Informational"
    Success -> "OK"
    Redirection -> "Redirection"
    ClientError -> "Client Error"
    ServerError -> "Server Error"
    Other -> "Unknown"

-- * Common pattern synonyms

pattern Continue, SwitchingProtocols :: Status
pattern Continue           = Status 100
pattern SwitchingProtocols = Status 101

pattern OK, Created, Accepted, NoContent, PartialContent :: Status
pattern OK             = Status 200
pattern Created        = Status 201
pattern Accepted       = Status 202
pattern NoContent      = Status 204
pattern PartialContent = Status 206

pattern MovedPermanently, Found, SeeOther, NotModified
      , TemporaryRedirect, PermanentRedirect :: Status
pattern MovedPermanently   = Status 301
pattern Found              = Status 302
pattern SeeOther           = Status 303
pattern NotModified        = Status 304
pattern TemporaryRedirect  = Status 307
pattern PermanentRedirect  = Status 308

pattern BadRequest, Unauthorized, Forbidden, NotFound
      , MethodNotAllowed, NotAcceptable, RequestTimeout, Conflict, Gone
      , LengthRequired, PayloadTooLarge, UriTooLong, UnsupportedMediaType
      , RangeNotSatisfiable, ExpectationFailed, UpgradeRequired
      , PreconditionFailed, PreconditionRequired, TooManyRequests :: Status
pattern BadRequest           = Status 400
pattern Unauthorized         = Status 401
pattern Forbidden            = Status 403
pattern NotFound             = Status 404
pattern MethodNotAllowed     = Status 405
pattern NotAcceptable        = Status 406
pattern RequestTimeout       = Status 408
pattern Conflict             = Status 409
pattern Gone                 = Status 410
pattern LengthRequired       = Status 411
pattern PreconditionFailed   = Status 412
pattern PayloadTooLarge      = Status 413
pattern UriTooLong           = Status 414
pattern UnsupportedMediaType = Status 415
pattern RangeNotSatisfiable  = Status 416
pattern ExpectationFailed    = Status 417
pattern UpgradeRequired      = Status 426
pattern PreconditionRequired = Status 428
pattern TooManyRequests      = Status 429

pattern RequestHeaderFieldsTooLarge :: Status
pattern RequestHeaderFieldsTooLarge = Status 431

pattern InternalServerError, NotImplemented, BadGateway
      , ServiceUnavailable, GatewayTimeout, HttpVersionNotSupported :: Status
pattern InternalServerError    = Status 500
pattern NotImplemented         = Status 501
pattern BadGateway             = Status 502
pattern ServiceUnavailable     = Status 503
pattern GatewayTimeout         = Status 504
pattern HttpVersionNotSupported = Status 505
