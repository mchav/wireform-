{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}

-- | Utility exception types for the tests
module Test.Util.Exception
  ( -- * User exceptions
    DeliberateException(..)
  , ExceptionId
    -- * Exact exceptions (compat shim)
  , ExactException
  , pattern WrapExactException
  , waitCatchExact
  , tryExact
  , catchAndWrap
    -- * Exception doc (no-op)
  , ToExceptionDoc(..)
  , LinesToExceptionDoc(..)
  ) where

import Control.Concurrent.Async (Async, waitCatchSTM)
import Control.Concurrent.STM (STM)
import Control.Exception
import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  User exceptions

  When a test calls for the client or the server to throw an exception, we throw
  one of these. Their sole purpose is to be "any" kind of exception (not a
  specific one).
-------------------------------------------------------------------------------}

-- | Deliberate exceptions do not constitute test failures
data DeliberateException =
    -- | Deliberate exception thrown in the server
    DeliberateServerException ExceptionId

    -- | Deliberate exception thrown in the client
  | DeliberateClientException ExceptionId
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

-- | We distinguish exceptions from each other simply by a number
type ExceptionId = Int

{-------------------------------------------------------------------------------
  Exact exceptions (compatibility shim)

  In the original grapesy, ExactException was a newtype around SomeException.
  We keep it as a type alias to minimize code changes.
-------------------------------------------------------------------------------}

type ExactException = SomeException

pattern WrapExactException :: SomeException -> SomeException
pattern WrapExactException e = e
{-# COMPLETE WrapExactException #-}

waitCatchExact :: Async a -> STM (Either SomeException a)
waitCatchExact = waitCatchSTM

tryExact :: IO a -> IO (Either SomeException a)
tryExact = try

catchAndWrap :: Exception e => (SomeException -> e) -> IO a -> IO a
catchAndWrap wrap action = catch action (throwIO . wrap)

{-------------------------------------------------------------------------------
  ToExceptionDoc (no-op compatibility shim)
-------------------------------------------------------------------------------}

class ToExceptionDoc a

-- Generic instance so deriving anyclass works
instance {-# OVERLAPPABLE #-} ToExceptionDoc a

newtype LinesToExceptionDoc a = LinesToExceptionDoc a
  deriving stock (Show, Generic)

instance ToExceptionDoc (LinesToExceptionDoc a)
