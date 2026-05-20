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
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toUpper)
import Data.Hashable (Hashable)
import Data.String (IsString (..))
import GHC.Generics (Generic)

-- | An HTTP request method token. The wrapped 'ByteString' is the
-- exact uppercase token that appears on the wire (e.g. @"GET"@,
-- @"PROPFIND"@).
newtype Method = Method { fromMethod :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, NFData)

instance Show Method where
  showsPrec _ (Method bs) = shows bs

-- | @fromString@ uppercases the input before storing it, mirroring
-- hermes (which built its 'Symbol' from @fmap toUpper@). This is
-- safe for the IANA-registered tokens but is mildly lossy for
-- extensions that legitimately mix case. Use 'methodFromBytes' if
-- you want exact preservation.
instance IsString Method where
  fromString = Method . BS8.pack . fmap toUpper

-- | Render the method as on-the-wire bytes.
methodToBytes :: Method -> ByteString
methodToBytes = fromMethod

-- | Construct a 'Method' from raw bytes without case-folding.
methodFromBytes :: ByteString -> Method
methodFromBytes = Method

-- | RFC 9110 \"safe\" methods: GET / HEAD / OPTIONS / TRACE.
isSafe :: Method -> Bool
isSafe m = m == mGet || m == mHead || m == mOptions || m == mTrace

-- | RFC 9110 \"idempotent\" methods: the safe ones plus PUT and DELETE.
isIdempotent :: Method -> Bool
isIdempotent m = isSafe m || m == mPut || m == mDelete

-- | Whether RFC 9110 § 9 says a request body is meaningful for this
-- method. GET / HEAD / DELETE may carry bodies in principle but
-- servers are not required to look at them.
bodyAllowedInRequest :: Method -> Bool
bodyAllowedInRequest m =
  m == mPost
    || m == mPut
    || m == mPatch
    || m `elem` [mPropFind, mPropPatch, mMkCol, mReport, mSearch, mLock]

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
