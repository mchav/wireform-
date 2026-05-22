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
  deriving stock (Show, Functor)

-- | Internal sentinel: clean EOF before any bytes were consumed for
-- this parse.  Never exposed to users — the driver converts it to
-- either clean loop termination or 'ParseUnexpectedEof' depending on
-- context.
data CleanEof = CleanEof
  deriving stock (Show, Typeable)

instance Exception CleanEof
