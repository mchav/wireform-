{- | The 'Role' of a 'Network.WebSocket.Connection.Connection': are
we the server side or the client side of the RFC 6455 handshake?

Lives in its own module so the extension layer
("Network.WebSocket.PerMessageDeflate") and the connection layer
can share it without a cyclic import.
-}
module Network.WebSocket.Connection.Role
  ( Role (..)
  , peerRole
  ) where

-- | Which side of the connection we are.  Determines masking and
-- handshake direction.
data Role = Server | Client
  deriving stock (Eq, Show)

peerRole :: Role -> Role
peerRole Server = Client
peerRole Client = Server
