{- | HTTP request methods.

Vendored from the hermes library and rebranded for wireform. The
original hermes 'Method' was a 'Symbolize.Symbol' for interning;
this version uses a strict 'ByteString' directly because we don't
want the symbol-table dep in the wireform closure. The hot path
(method comparison) is a 'ByteString' equality which is already a
SIMD memcmp on GHC 9.x and below.

The @m*@ constants cover the IANA-registered standard methods (RFC
7231) plus the common WebDAV / CalDAV extensions; arbitrary method
tokens are still constructible via the 'IsString' instance.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Types.Method
  ( Method (..)
  , methodToBytes
  , methodFromBytes
  , mkMethod
  , MethodError (..)
  , isSafe
  , isIdempotent
  , bodyAllowedInRequest
    -- * Standard methods (RFC 9110 \/ 7231)
  , mGet, mHead, mPost, mPut, mDelete
  , mConnect, mOptions, mTrace, mPatch
    -- * WebDAV (RFC 4918) and friends
  , mACL
  , mBaselineControl
  , mBind
  , mCheckin
  , mCheckout
  , mCopy
  , mLabel
  , mLink
  , mLock
  , mMerge
  , mMkActivity
  , mMkCalendar
  , mMkCol
  , mMkRedirectRef
  , mMkWorkspace
  , mMove
  , mOrderPatch
  , mPropFind
  , mPropPatch
  , mRebind
  , mReport
  , mSearch
  , mUnbind
  , mUnlink
  , mUnlock
  , mUpdate
  , mUpdateRedirectRef
  , mVersionControl
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Hashable (Hashable)
import Data.String (IsString (..))
import Data.Word (Word8)
import GHC.Generics (Generic)

import Network.HTTP.Internal.Validation (isTchar)

-- | An HTTP request method token. The wrapped 'ByteString' is the
-- exact uppercase token that appears on the wire (e.g. @"GET"@,
-- @"PROPFIND"@).
newtype Method = Method { fromMethod :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, NFData)

instance Show Method where
  showsPrec _ (Method bs) = shows bs

-- | @fromString@ preserves case verbatim. RFC 9110 \u00a79.1 says the
-- method is a case-sensitive token, so an extension method like
-- @\"PROPFIND\"@, @\"BatchGet\"@, or @\"M-SEARCH\"@ should round-trip
-- exactly. The IANA-registered standard methods are all upper-case
-- already, so this is only observably different for caller-defined
-- extensions.
--
-- (Earlier versions of this module mirrored hermes by applying
-- 'Data.Char.toUpper'; that was an unsafe lossy transform for
-- mixed-case extension tokens. The constants below \u2014 'mGet',
-- 'mPropFind', etc. \u2014 are unaffected because they're already in
-- the canonical wire spelling.)
instance IsString Method where
  fromString = Method . BS8.pack

-- | Render the method as on-the-wire bytes.
methodToBytes :: Method -> ByteString
methodToBytes = fromMethod

-- | Construct a 'Method' from raw bytes without any case-folding or
-- validation. Use 'mkMethod' if you want the bytes checked against
-- the @token@ grammar.
methodFromBytes :: ByteString -> Method
methodFromBytes = Method

-- | Errors raised by 'mkMethod'.
data MethodError
  = MethodEmpty
  | MethodInvalidByte !Word8
  deriving stock (Eq, Show)

-- | Validating constructor: ensures the bytes form a non-empty @token@
-- (RFC 9110 \u00a75.6.2), i.e. only @tchar@ characters. The standard
-- and WebDAV constants are already valid by construction.
mkMethod :: ByteString -> Either MethodError Method
mkMethod bs
  | BS.null bs = Left MethodEmpty
  | otherwise = case BS.find (not . isTchar) bs of
      Nothing -> Right (Method bs)
      Just w  -> Left (MethodInvalidByte w)

-- | RFC 9110 \"safe\" methods: GET / HEAD / OPTIONS / TRACE.
isSafe :: Method -> Bool
isSafe m = m == mGet || m == mHead || m == mOptions || m == mTrace

-- | RFC 9110 \"idempotent\" methods: the safe ones plus PUT and DELETE.
isIdempotent :: Method -> Bool
isIdempotent m = isSafe m || m == mPut || m == mDelete

-- | Whether RFC 9110 §9 forbids sending content for this method.
-- Only @CONNECT@ (§9.3.6) and @TRACE@ (§9.3.8) are forbidden;
-- every other method may carry content (subject to server
-- interpretation, which is a separate concern from the framing
-- layer's "is it legal to send"). This replaces the older,
-- narrower predicate that only returned 'True' for
-- POST\/PUT\/PATCH and a handful of WebDAV methods.
bodyAllowedInRequest :: Method -> Bool
bodyAllowedInRequest m = m /= mConnect && m /= mTrace

-- Standard methods --------------------------------------------------------

mGet, mHead, mPost, mPut, mDelete :: Method
mGet    = Method "GET"
mHead   = Method "HEAD"
mPost   = Method "POST"
mPut    = Method "PUT"
mDelete = Method "DELETE"

mConnect, mOptions, mTrace, mPatch :: Method
mConnect = Method "CONNECT"
mOptions = Method "OPTIONS"
mTrace   = Method "TRACE"
mPatch   = Method "PATCH"

-- WebDAV and friends ------------------------------------------------------

mACL, mBaselineControl, mBind, mCheckin, mCheckout, mCopy, mLabel,
  mLink, mLock, mMerge, mMkActivity, mMkCalendar, mMkCol,
  mMkRedirectRef, mMkWorkspace, mMove, mOrderPatch, mPropFind,
  mPropPatch, mRebind, mReport, mSearch, mUnbind, mUnlink, mUnlock,
  mUpdate, mUpdateRedirectRef, mVersionControl :: Method
mACL               = Method "ACL"
mBaselineControl   = Method "BASELINE-CONTROL"
mBind              = Method "BIND"
mCheckin           = Method "CHECKIN"
mCheckout          = Method "CHECKOUT"
mCopy              = Method "COPY"
mLabel             = Method "LABEL"
mLink              = Method "LINK"
mLock              = Method "LOCK"
mMerge             = Method "MERGE"
mMkActivity        = Method "MKACTIVITY"
mMkCalendar        = Method "MKCALENDAR"
mMkCol             = Method "MKCOL"
mMkRedirectRef     = Method "MKREDIRECTREF"
mMkWorkspace       = Method "MKWORKSPACE"
mMove              = Method "MOVE"
mOrderPatch        = Method "ORDERPATCH"
mPropFind          = Method "PROPFIND"
mPropPatch         = Method "PROPPATCH"
mRebind            = Method "REBIND"
mReport            = Method "REPORT"
mSearch            = Method "SEARCH"
mUnbind            = Method "UNBIND"
mUnlink            = Method "UNLINK"
mUnlock            = Method "UNLOCK"
mUpdate            = Method "UPDATE"
mUpdateRedirectRef = Method "UPDATEREDIRECTREF"
mVersionControl    = Method "VERSION-CONTROL"
