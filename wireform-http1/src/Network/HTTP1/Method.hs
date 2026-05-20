{- | HTTP request methods (RFC 9110 § 9).

The eight IANA-registered \"standard\" methods are represented as
constructors; everything else uses 'MethodOther' carrying a strict
'ByteString' so we can roundtrip arbitrary extension methods without
allocating a new constructor on the hot path.
-}
module Network.HTTP1.Method
  ( Method (..)
  , methodFromBytes
  , methodToBytes
  , isSafe
  , isIdempotent
  , bodyAllowedInRequest
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)

-- | RFC 9110-registered methods plus an escape hatch for extensions
-- (WebDAV's @PROPFIND@, etc.).
data Method
  = GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH
  | MethodOther !ByteString
  deriving stock (Eq, Show, Generic)

instance NFData Method

-- | Parse a method token. The 8 standard verbs are recognised by length-
-- discriminated 'ByteString' equality (single comparison per branch);
-- anything else copies into 'MethodOther'.
{-# INLINE methodFromBytes #-}
methodFromBytes :: ByteString -> Method
methodFromBytes bs = case BS.length bs of
  3
    | bs == "GET" -> GET
    | bs == "PUT" -> PUT
    | otherwise -> MethodOther (BS.copy bs)
  4
    | bs == "HEAD" -> HEAD
    | bs == "POST" -> POST
    | otherwise -> MethodOther (BS.copy bs)
  5
    | bs == "PATCH" -> PATCH
    | bs == "TRACE" -> TRACE
    | otherwise -> MethodOther (BS.copy bs)
  6
    | bs == "DELETE" -> DELETE
    | otherwise -> MethodOther (BS.copy bs)
  7
    | bs == "OPTIONS" -> OPTIONS
    | bs == "CONNECT" -> CONNECT
    | otherwise -> MethodOther (BS.copy bs)
  _ -> MethodOther (BS.copy bs)

{-# INLINE methodToBytes #-}
methodToBytes :: Method -> ByteString
methodToBytes = \case
  GET -> "GET"
  HEAD -> "HEAD"
  POST -> "POST"
  PUT -> "PUT"
  DELETE -> "DELETE"
  CONNECT -> "CONNECT"
  OPTIONS -> "OPTIONS"
  TRACE -> "TRACE"
  PATCH -> "PATCH"
  MethodOther bs -> bs

-- | Safe methods per RFC 9110 § 9.2.1.
{-# INLINE isSafe #-}
isSafe :: Method -> Bool
isSafe = \case
  GET -> True
  HEAD -> True
  OPTIONS -> True
  TRACE -> True
  _ -> False

-- | Idempotent methods per RFC 9110 § 9.2.2.
{-# INLINE isIdempotent #-}
isIdempotent :: Method -> Bool
isIdempotent m = case m of
  PUT -> True
  DELETE -> True
  _ -> isSafe m

-- | Whether the request itself can carry a body. CONNECT is the
-- interesting exception: per RFC 9110 § 9.3.6 the request has no body
-- (the body, if any, belongs to the tunnel), so framing logic must
-- treat it as Content-Length: 0 even if the client sends a TE header.
{-# INLINE bodyAllowedInRequest #-}
bodyAllowedInRequest :: Method -> Bool
bodyAllowedInRequest CONNECT = False
bodyAllowedInRequest TRACE = False
bodyAllowedInRequest _ = True
