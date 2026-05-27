{- | HTTP status codes.

Vendored from the hermes library. Hermes used a plain @newtype
StatusCode { statusCode :: Word16 }@ with @status100@-style constants;
we keep that shape (it's cheap, comparable, hashable) and add the
convenience names from the IANA HTTP Status Code Registry.

We also add a 'statusCategory' helper and a 'statusReason' that
returns the canonical reason phrase. HTTP\/1.x emits the reason phrase
on the status line; HTTP\/2 and later drop it on the wire but it's
still useful for diagnostics.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Types.Status
  ( Status (..)
  , StatusCategory (..)
  , statusCategory
  , statusReason
    -- * 1xx informational
  , status100, status101, status102, status103
    -- * 2xx success
  , status200, status201, status202, status203, status204
  , status205, status206, status207, status208, status226
    -- * 3xx redirection
  , status300, status301, status302, status303, status304
  , status305, status307, status308
    -- * 4xx client errors
  , status400, status401, status402, status403, status404
  , status405, status406, status407, status408, status409
  , status410, status411, status412, status413, status414
  , status415, status416, status417, status418, status421
  , status422, status423, status424, status425, status426
  , status428, status429, status431, status451
    -- * 5xx server errors
  , status500, status501, status502, status503, status504
  , status505, status506, status507, status508, status510
  , status511
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Word (Word16)
import GHC.Generics (Generic)

newtype Status = Status { statusCode :: Word16 }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, NFData)

instance Show Status where
  showsPrec _ (Status w) = shows w

data StatusCategory
  = Informational  -- ^ 1xx
  | Successful     -- ^ 2xx
  | Redirection    -- ^ 3xx
  | ClientError    -- ^ 4xx
  | ServerError    -- ^ 5xx
  | UnknownCategory
  deriving stock (Eq, Show, Generic)

instance NFData StatusCategory

statusCategory :: Status -> StatusCategory
statusCategory (Status n)
  | n >= 100 && n < 200 = Informational
  | n >= 200 && n < 300 = Successful
  | n >= 300 && n < 400 = Redirection
  | n >= 400 && n < 500 = ClientError
  | n >= 500 && n < 600 = ServerError
  | otherwise           = UnknownCategory

-- | The canonical IANA reason phrase for a known status. Returns an
-- empty 'ByteString' for unknown codes; callers that need to fill in
-- their own phrase can branch on 'BS.null'.
statusReason :: Status -> ByteString
statusReason (Status n) = case n of
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
  _   -> case statusCategory theStatus of
    Informational    -> "Informational"
    Successful       -> "OK"
    Redirection      -> "Redirection"
    ClientError      -> "Client Error"
    ServerError      -> "Server Error"
    UnknownCategory  -> ""

status100, status101, status102, status103 :: Status
status100 = Status 100
status101 = Status 101
status102 = Status 102
status103 = Status 103

status200, status201, status202, status203, status204,
  status205, status206, status207, status208, status226 :: Status
status200 = Status 200
status201 = Status 201
status202 = Status 202
status203 = Status 203
status204 = Status 204
status205 = Status 205
status206 = Status 206
status207 = Status 207
status208 = Status 208
status226 = Status 226

status300, status301, status302, status303, status304,
  status305, status307, status308 :: Status
status300 = Status 300
status301 = Status 301
status302 = Status 302
status303 = Status 303
status304 = Status 304
status305 = Status 305
status307 = Status 307
status308 = Status 308

status400, status401, status402, status403, status404,
  status405, status406, status407, status408, status409,
  status410, status411, status412, status413, status414,
  status415, status416, status417, status418, status421,
  status422, status423, status424, status425, status426,
  status428, status429, status431, status451 :: Status
status400 = Status 400
status401 = Status 401
status402 = Status 402
status403 = Status 403
status404 = Status 404
status405 = Status 405
status406 = Status 406
status407 = Status 407
status408 = Status 408
status409 = Status 409
status410 = Status 410
status411 = Status 411
status412 = Status 412
status413 = Status 413
status414 = Status 414
status415 = Status 415
status416 = Status 416
status417 = Status 417
status418 = Status 418
status421 = Status 421
status422 = Status 422
status423 = Status 423
status424 = Status 424
status425 = Status 425
status426 = Status 426
status428 = Status 428
status429 = Status 429
status431 = Status 431
status451 = Status 451

status500, status501, status502, status503, status504,
  status505, status506, status507, status508, status510,
  status511 :: Status
status500 = Status 500
status501 = Status 501
status502 = Status 502
status503 = Status 503
status504 = Status 504
status505 = Status 505
status506 = Status 506
status507 = Status 507
status508 = Status 508
status510 = Status 510
status511 = Status 511
