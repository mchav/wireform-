{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Kafka.Protocol.VersionNegotiation
Description : Pick the right API version for each request to each broker

Builds on 'Kafka.Protocol.ApiVersions' (which knows how to send
the @ApiVersionsRequest@ and parse the response) by adding the
glue every client call site needs:

  * 'ensureVersionsNegotiated' — runs the @ApiVersions@
    handshake on a connection /once per broker/ (idempotent;
    skips if the cache already has an entry). Call this right
    after the connection is established so subsequent calls can
    consult the cache.
  * 'pickApiVersion' — given a per-API @(min, max)@ range the
    client supports, return the highest version both sides
    agree on, falling back to the client's max if the cache
    is empty (e.g. the broker doesn't speak @ApiVersions@,
    which is the case for Kafka < 0.10).
  * 'pickApiVersionFor' — type-driven variant that uses the
    'KafkaMessage' typeclass instance generated for each
    request type, so call sites don't need to spell out the
    api key + client max.

The point of going through this module is to eliminate the
boilerplate that used to live at every call site:

@
brokerVersionM <- atomically $ AV.queryApiVersion cache addr key
let v = case brokerVersionM of
      Nothing    -> 0
      Just range -> case AV.selectVersion clientMax range of
        Nothing -> 0
        Just v  -> v
@

becomes

@
v <- pickApiVersion cache addr apiKey clientMin clientMax fallback
@

and the version mismatch case ("broker advertises max=2 but
the client needs at least min=4") becomes a structured
'VersionMismatch' value the caller can surface to the user
rather than silently dropping to v0 and letting the broker
reject the request with an opaque @InvalidRequestException@.
-}
module Kafka.Protocol.VersionNegotiation
  ( -- * Negotiation
    ensureVersionsNegotiated
  , forceVersionsNegotiation
    -- * Version selection
  , pickApiVersion
  , pickApiVersionFor
  , VersionMismatch (..)
  ) where

import Control.Concurrent.STM (atomically)
import Control.Exception (Exception)
import Data.Int (Int16, Int32)
import Network.Connection (Connection)

import Kafka.Network.Connection (BrokerAddress)
import Kafka.Protocol.ApiVersions
  ( ApiVersionCache
  , ApiVersionRange (..)
  , negotiateVersions
  , queryApiVersion
  , selectVersion
  )
import qualified Kafka.Protocol.Message as Msg

----------------------------------------------------------------------
-- Negotiation lifecycle
----------------------------------------------------------------------

-- | Run @ApiVersions@ on a connection if (and only if) the
-- cache doesn't already have an entry for @addr@.
--
-- This is the function client setup code should call right
-- after a connection is established (and after SASL completes,
-- when configured). It's safe to call repeatedly on the same
-- @(cache, addr)@ pair — the second + subsequent calls are
-- O(1) cache hits.
--
-- Failure modes:
--
--   * The broker doesn't recognise the @ApiVersions@ API key
--     (Kafka < 0.10). We swallow this silently and leave the
--     cache empty; downstream 'pickApiVersion' calls will hit
--     their fallback path.
--   * The broker actively rejects the request (e.g. it's
--     responding with an error code). We log via the returned
--     'Either', /and/ leave the cache empty so callers can
--     decide whether to proceed with their compiled-in
--     defaults.
--
-- The supplied @IO Int32@ action is the caller's correlation
-- id generator (typically backed by a 'TVar' or 'IORef'); the
-- handshake reuses it so its correlation ids stay distinct
-- from the caller's normal request flow.
ensureVersionsNegotiated
  :: Connection
  -> BrokerAddress
  -> ApiVersionCache
  -> IO Int32        -- ^ next-correlation-id action
  -> IO (Either String ())
ensureVersionsNegotiated conn addr cache nextCorrId = do
  -- Fast-path: cache already populated for this broker; skip
  -- the round-trip entirely.
  cached <- atomically (queryApiVersion cache addr 18 {- ApiVersions key -})
  case cached of
    Just _  -> pure (Right ())
    Nothing -> do
      corrId <- nextCorrId
      r <- negotiateVersions conn addr cache corrId
      pure $ case r of
        Left e  -> Left e
        Right _ -> Right ()

-- | Like 'ensureVersionsNegotiated' but always runs the
-- handshake, even if the cache already has an entry.
--
-- Useful after a broker has been bounced (its supported
-- versions might have shrunk in a downgrade) or for tests
-- that want to exercise the full handshake on every call.
forceVersionsNegotiation
  :: Connection
  -> BrokerAddress
  -> ApiVersionCache
  -> IO Int32
  -> IO (Either String ())
forceVersionsNegotiation conn addr cache nextCorrId = do
  corrId <- nextCorrId
  r <- negotiateVersions conn addr cache corrId
  pure $ case r of
    Left e  -> Left e
    Right _ -> Right ()

----------------------------------------------------------------------
-- Version selection
----------------------------------------------------------------------

-- | A signal that the broker can't satisfy the request: it
-- /does/ speak the API in question (we know this because the
-- 'ApiVersions' response listed it), but its supported range
-- doesn't overlap the client's.
--
-- Callers should treat this as a hard error rather than
-- falling back to a guessed version — sending an
-- out-of-range request makes the broker close the connection
-- with @InvalidRequestException@ + @BufferUnderflowException@.
data VersionMismatch = VersionMismatch
  { mismatchApiKey       :: !Int16
  , mismatchClientMin    :: !Int16
  , mismatchClientMax    :: !Int16
  , mismatchBrokerMin    :: !Int16
  , mismatchBrokerMax    :: !Int16
  } deriving (Eq, Show)

instance Exception VersionMismatch

-- | Pick the highest API version this client and this broker
-- both support, given the caller's @(clientMin, clientMax)@
-- range and a fallback for the case where the cache hasn't
-- been populated yet (e.g. handshake hasn't run, or the
-- broker doesn't speak @ApiVersions@ at all).
--
-- Decision tree:
--
--   1. If @cache[addr][apiKey]@ is set:
--
--        * If @brokerMax < clientMin@ — return
--          @Left VersionMismatch@. The broker is too old.
--        * Otherwise — return @Right (min clientMax brokerMax)@,
--          clamped above @brokerMin@ (which is also clamped
--          above @clientMin@).
--
--   2. If the cache is empty for this @(addr, apiKey)@:
--
--        * Return @Right fallback@. The caller is responsible
--          for picking a sensible fallback (typically
--          @clientMax@ for stable APIs; a known-old version
--          like 0 or 1 for APIs whose newer versions added
--          required fields the client wouldn't know how to
--          fill in without negotiation).
--
-- The @clientMin@ argument exists to model APIs that have had
-- /required/ fields added in newer versions. Most call sites
-- can pass 0 here.
pickApiVersion
  :: ApiVersionCache
  -> BrokerAddress
  -> Int16             -- ^ API key
  -> Int16             -- ^ client's minimum supported version
  -> Int16             -- ^ client's maximum supported version
  -> Int16             -- ^ fallback when cache is empty
  -> IO (Either VersionMismatch Int16)
pickApiVersion cache addr apiKey clientMin clientMax fallback = do
  cached <- atomically (queryApiVersion cache addr apiKey)
  pure $ case cached of
    Nothing    -> Right fallback
    Just range
      | rangeMaxVersion range < clientMin ->
          Left VersionMismatch
            { mismatchApiKey    = apiKey
            , mismatchClientMin = clientMin
            , mismatchClientMax = clientMax
            , mismatchBrokerMin = rangeMinVersion range
            , mismatchBrokerMax = rangeMaxVersion range
            }
      | otherwise ->
          -- 'selectVersion' already returns
          -- @Just (min clientMax brokerMax)@ when there's
          -- overlap; we additionally clamp below by
          -- @max clientMin brokerMin@ to make sure neither
          -- side is asked to handle a version it doesn't
          -- support.
          let !chosen = case selectVersion clientMax range of
                Just v  -> max v (max clientMin (rangeMinVersion range))
                Nothing -> max clientMin (rangeMinVersion range)
          in Right chosen

-- | Type-driven version of 'pickApiVersion'.
--
-- Uses the 'KafkaMessage' instance for the request type to
-- supply the api key + client min/max, so call sites read
--
-- @
-- v <- pickApiVersionFor \@MetadataRequest cache addr fallback
-- @
--
-- and stay in lock-step with the codegen-emitted version
-- bounds when the protocol surface evolves.
pickApiVersionFor
  :: forall msg. Msg.KafkaMessage msg
  => ApiVersionCache
  -> BrokerAddress
  -> Int16            -- ^ fallback when cache is empty
  -> IO (Either VersionMismatch Int16)
pickApiVersionFor cache addr fallback =
  pickApiVersion cache addr
    (Msg.messageApiKey      @msg)
    (Msg.messageMinVersion  @msg)
    (Msg.messageMaxVersion  @msg)
    fallback
