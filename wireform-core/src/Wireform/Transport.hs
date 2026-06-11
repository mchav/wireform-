{- | Magic-ring transports — receive and send.

This module is a thin umbrella over the symmetric pair
'Wireform.Transport.Receive.ReceiveTransport' (consumer of bytes
the producer side wrote into a ring) and
'Wireform.Transport.Send.SendTransport' (producer of bytes the
consumer side will drain from a ring).  See the per-direction
modules for the full doc.
-}
module Wireform.Transport (
  module Wireform.Transport.Receive,
  module Wireform.Transport.Send,
) where

import Wireform.Transport.Receive
import Wireform.Transport.Send

