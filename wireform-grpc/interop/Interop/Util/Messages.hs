-- | Utilities for working with the message types from the interop testsuite
module Interop.Util.Messages (
    -- * BoolValue
    boolValue
    -- * Payload
  , payloadOfZeroes
  , payloadOfType
    -- * ORCA
  , testOrcaToLoadReport
  ) where

import Data.ByteString qualified as BS.Strict

import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec (OrcaLoadReport)

import Interop.Util.Exceptions

import Proto.API.Interop

{-------------------------------------------------------------------------------
  BoolValue
-------------------------------------------------------------------------------}

boolValue :: Bool -> Proto BoolValue
boolValue b = (mempty) & #value .~ b

{-------------------------------------------------------------------------------
  Payload
-------------------------------------------------------------------------------}

payloadOfZeroes :: Int -> Proto Payload
payloadOfZeroes sz = (mempty) & #body .~ BS.Strict.pack (replicate sz 0)

payloadOfType :: Integral size => PayloadType -> size -> IO (Proto Payload)
payloadOfType type' size = do
    body <-
      case type' of
        PayloadType'Compressable ->
          return $ BS.Strict.pack (replicate (fromIntegral size) 0)
        PayloadType''Unrecognized x ->
          assertUnrecognized x
    return $
      (mempty)
        & #type .~ type'
        & #body .~ body

{-------------------------------------------------------------------------------
  ORCA
-------------------------------------------------------------------------------}

-- | Convert the interop test's @TestOrcaReport@ to the standard
-- @xds.data.orca.v3.OrcaLoadReport@ used in the @endpoint-load-metrics-bin@
-- trailer.
testOrcaToLoadReport :: TestOrcaReport -> OrcaLoadReport
testOrcaToLoadReport report =
    (mempty :: OrcaLoadReport)
      & #cpuUtilization .~ (report ^. #cpuUtilization)
      & #memUtilization .~ (report ^. #memoryUtilization)
      & #requestCost    .~ (report ^. #requestCost)
      & #utilization    .~ (report ^. #utilization)
