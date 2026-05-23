module Wireform.Parser.Error
  ( ParseError (..)
  , CleanEof (..)
  ) where

import Control.Exception (Exception, SomeException)
import Data.Typeable (Typeable)
import Data.Word (Word64)

-- | The result of a failed parse.
data ParseError e
  = ParseFail !Word64
    -- ^ The parser failed (recoverably) at the given absolute position
    -- and the failure was not caught by an enclosing alternative.

  | ParseErr !Word64 !e
    -- ^ The parser failed unrecoverably (via cut\/commit) with user error @e@.

  | ParseUnexpectedEof !Word64 !Int
    -- ^ Transport reported EOF while the parser was mid-message.
    -- The position is where the parser was; the 'Int' is how many
    -- additional bytes it needed.

  | ParseTransportError !SomeException
    -- ^ Transport-level failure (broken connection, closed socket, etc.).

  | ParseRingOverflow !Word64 !Int !Int
    -- ^ The parser asked for more bytes than the magic ring can ever
    -- hold (or, more precisely, more than the remaining capacity from
    -- the parser's current position).  Continuing would deadlock —
    -- the producer cannot make room because the consumer is suspended
    -- waiting for bytes the producer cannot deliver.  Fields: parser
    -- position, requested bytes, ring size.
  deriving stock (Show, Functor)

-- | Internal sentinel: clean EOF before any bytes were consumed for
-- this parse.  Never exposed to users — the driver converts it to
-- either clean loop termination or 'ParseUnexpectedEof' depending on
-- context.
data CleanEof = CleanEof
  deriving stock (Show, Typeable)

instance Exception CleanEof
