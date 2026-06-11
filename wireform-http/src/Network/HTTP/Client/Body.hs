{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- | Request-body and response-body conversion classes.

'Body' takes a user-facing body representation and turns it into the
universal wire shape, an 'IO BodyStream'. 'ResponseBody' is the
mirror image on the consumer side: a marker type that decides how
the raw response popper gets folded into a value the caller actually
wants.

Both classes are open. Users define instances for their own body
types — multipart, server-sent events, gRPC framing, whatever.
-}
module Network.HTTP.Client.Body (
  -- * Request bodies
  Body (..),

  -- * Response bodies
  ResponseBody (..),
  StrictBody (..),
  StreamingBody (..),
  DiscardBody (..),

  -- * Re-exports
  module Network.HTTP.Client.BodyStream,
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Void (Void, absurd)
import Network.HTTP.Client.BodyStream
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Status qualified as S


{- | Class that converts a user-facing request body into a 'BodyStream'.
The result is in 'IO' because even strict bodies want to allocate
an 'Data.IORef.IORef' for the popper.
-}
class Body body where
  toBodyStream :: body -> IO BodyStream


instance Body BodyStream where
  toBodyStream = pure


instance Body ByteString where
  toBodyStream = streamFromStrict


-- | A no-body request: produces 'emptyStream'.
instance Body () where
  toBodyStream () = pure emptyStream


instance Body Void where
  toBodyStream = absurd


{- | A pre-built 'Popper' is treated as a body of unknown size. If you
know the size, build a 'BodyStream' directly.
-}
instance Body Popper where
  toBodyStream popper =
    pure
      BodyStream
        { pull = popper
        , knownSize = Nothing
        }


-- | A list of chunks is treated as a streaming body of known total size.
instance Body [ByteString] where
  toBodyStream = streamFromList


{- | Class that decides how the raw response popper is consumed.

'consumeBody' receives the status, headers, and the response popper.
It returns whatever value the caller asked for. For a strict body
this is the fully-drained 'ByteString'; for a streaming body it
can be the popper itself, or any custom folding.
-}
class ResponseBody r where
  type Consumed r :: Type
  consumeBody :: S.Status -> H.Headers -> Popper -> IO (Consumed r)


{- | Marker type for strict response body consumption: materialise
the popper into a single 'ByteString'. Use as
@send transport request decoder \@StrictBody@.
-}
data StrictBody = StrictBody


instance ResponseBody StrictBody where
  type Consumed StrictBody = ByteString
  consumeBody _status _headers = popperBytes


{- | Marker type that passes the popper through untouched. The caller
is responsible for draining it before the transport's scoped
lifetime ends.
-}
data StreamingBody = StreamingBody


instance ResponseBody StreamingBody where
  type Consumed StreamingBody = Popper
  consumeBody _status _headers = pure


-- | Default response-body mode: strict.
instance ResponseBody ByteString where
  type Consumed ByteString = ByteString
  consumeBody _status _headers = popperBytes


{- | Plain function-style streaming: returns the popper.

> resp <- send transport req decoder \@(IO ByteString)
-}
instance ResponseBody (IO ByteString) where
  type Consumed (IO ByteString) = Popper
  consumeBody _status _headers = pure


{- | Discard the body entirely. Use as @send transport req decoder
\@DiscardBody@.
-}
data DiscardBody = DiscardBody


instance ResponseBody DiscardBody where
  type Consumed DiscardBody = ()
  consumeBody _status _headers = drainPopper
