{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Kafka.Protocol.Message
Description : Type class for Kafka protocol messages
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module defines the 'KafkaMessage' type class which associates Kafka protocol
message types with their API keys and version information. This provides compile-time
type safety when working with protocol messages, eliminating the need for magic
numbers throughout the codebase.

= Usage

The 'KafkaMessage' type class is instantiated by code generation for all protocol
request and response types. You can use it to obtain message metadata at compile time:

@
import qualified Kafka.Protocol.Generated.ProduceRequest as PReq
import Kafka.Protocol.Message

-- Get API key for ProduceRequest without magic numbers
apiKey = messageApiKey @PReq.ProduceRequest
@

This is particularly useful when:

* Building request headers without hardcoding API keys
* Version negotiation based on message type
* Routing messages based on their API key
* Generic message handling code

= Type Safety

By using the type class, you get:

* Compile-time verification that you're using the correct API key
* Automatic updates when protocol definitions change
* Type-driven API key selection
* Prevention of API key/message type mismatches
-}
module Kafka.Protocol.Message (
  -- * Type Class
  KafkaMessage (..),

  -- * Helper Functions
  isFlexibleVersion,
  isVersionSupported,
) where

import Data.Int (Int16)


{- | Type class associating a Kafka message type with its protocol metadata.

Instances of this class are generated automatically for all protocol
request and response messages. The class uses functional dependencies
to ensure that each message type has a unique API key.

This class provides compile-time access to:

* The numeric API key used in the protocol
* Minimum and maximum supported versions
* The first version that uses flexible (compact) encoding

Note: Not all messages have an API key (e.g., headers). For such types,
this class is not instantiated.
-}
class KafkaMessage a where
  {- | The numeric API key for this message type.

  This is the identifier used in the Kafka protocol to indicate which
  type of request or response is being sent. For example, ProduceRequest
  uses API key 0, while FetchRequest uses API key 1.

  This value is used when constructing request headers.
  -}
  messageApiKey :: Int16


  {- | Minimum supported API version for this message.

  Attempting to encode or decode a version lower than this will result
  in an error. This represents the oldest protocol version for this
  message type.
  -}
  messageMinVersion :: Int16


  {- | Maximum supported API version for this message.

  This library will not attempt to encode or decode versions higher
  than this value. This represents the newest protocol version that
  this implementation supports.
  -}
  messageMaxVersion :: Int16


  {- | First version that uses flexible (compact) encoding, if any.

  Flexible versions use compact encoding for strings and arrays, and
  include tagged fields at the end of messages. This is typically
  version 9 or higher for most messages, but varies by API.

  Returns 'Nothing' if this message never uses flexible encoding.
  -}
  messageFlexibleVersion :: Maybe Int16


{- | Check if a specific version uses flexible encoding for a message type.

Flexible versions use compact encoding (variable-length integers for
lengths) and support tagged fields for forward/backward compatibility.

Example:

@
if isFlexibleVersion \@ProduceRequest 11
  then -- use compact encoding
  else -- use standard encoding
@
-}
isFlexibleVersion :: forall a. KafkaMessage a => Int16 -> Bool
isFlexibleVersion version =
  case messageFlexibleVersion @a of
    Nothing -> False
    Just flexVer -> version >= flexVer


{- | Check if a version is supported for a message type.

A version is supported if it falls within the inclusive range
[messageMinVersion, messageMaxVersion].

Example:

@
when (isVersionSupported \@ProduceRequest requestedVersion) $ do
  -- proceed with encoding/decoding
@
-}
isVersionSupported :: forall a. KafkaMessage a => Int16 -> Bool
isVersionSupported version =
  version >= messageMinVersion @a && version <= messageMaxVersion @a
