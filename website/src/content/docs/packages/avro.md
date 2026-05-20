---
title: wireform-avro
description: "Apache Avro encoding and decoding with schema resolution, Object Container Files, IDL codegen, and logical types."
sidebar:
  order: 30
---

`wireform-avro` implements [Apache Avro](https://avro.apache.org/), a schema-driven
binary format used in Kafka (via Schema Registry), Apache Iceberg manifests, and
Hadoop-era data pipelines. Avro omits type tags on the wire: field order and types
come entirely from the schema. That keeps payloads compact, but it means the schema
must be available at decode time. Use this package when you need writer/reader schema
evolution, self-describing container files, or tight integration with the Confluent
ecosystem.

## Key features

- **Typeclass API** via `ToAvro` and `FromAvro`, with Template Haskell deriving and
  a companion `HasAvroSchema` class that reflects the Avro schema for each type
- **Schema resolution** between writer and reader schemas (added fields with
  defaults, removed fields, reordered fields, type promotions, alias renames)
- **Object Container Files (OCF)** with `null`, `deflate`, and `snappy` codecs
- **Avro IDL parser and codegen** from `.avdl` and `.avsc` schemas
- **Schema fingerprinting** (CRC-64-AVRO and SHA-256) for Schema Registry IDs
- **JSON bridge** for canonical Avro JSON encoding
- **Protocol support** for Avro RPC message envelopes
- **Runtime registry** for dynamic schema lookup
- **Logical types** for decimal, date, time, timestamp, duration, and uuid

## Basic usage

Avro is schema-driven, so the lowest-level encode and decode functions take an
`AvroType` alongside the value. For typed records, derive instances and use the
schema reflection class:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import Avro.Class (toAvro, fromAvro)
import Avro.Decode (decodeAvro)
import Avro.Derive (deriveAvro, HasAvroSchema, avroSchema)
import Avro.Encode (encodeAvro)
import Data.Int (Int32)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data Person = Person
  { personName :: !Text
  , personAge  :: !Int32
  }
  deriving stock (Show, Eq, Generic)

$(deriveAvro ''Person)

encodePerson :: Person -> ByteString
encodePerson p =
  encodeAvro (avroSchema (Proxy :: Proxy Person)) (toAvro p)

decodePerson :: ByteString -> Either String Person
decodePerson bs = do
  val <- decodeAvro (avroSchema (Proxy :: Proxy Person)) bs
  fromAvro val
```

### Schema resolution

When the reader's schema differs from the writer's (for example, a new field was
added with a default), decode with the writer schema and resolve to the reader's
view:

```haskell
import Avro.Decode (decodeAvroResolved)
import Avro.Schema
import qualified Avro.Value as AV
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

writerSchema :: AvroType
writerSchema = AvroRecord
  { avroRecordName      = "Person"
  , avroRecordNamespace = Nothing
  , avroRecordDoc       = Nothing
  , avroRecordAliases   = V.empty
  , avroRecordFields    = V.fromList
      [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
      , AvroField "age"  (AvroPrimitive AvroInt)    Nothing Nothing V.empty Nothing Map.empty
      ]
  , avroRecordProps     = Map.empty
  }

readerSchema :: AvroType
readerSchema = AvroRecord
  { avroRecordName      = "Person"
  , avroRecordNamespace = Nothing
  , avroRecordDoc       = Nothing
  , avroRecordAliases   = V.empty
  , avroRecordFields    = V.fromList
      [ AvroField "name"  (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
      , AvroField "age"   (AvroPrimitive AvroInt)    Nothing Nothing V.empty Nothing Map.empty
      , AvroField "email" (AvroPrimitive AvroString) (Just "\"unknown@example.com\"")
          Nothing V.empty Nothing Map.empty
      ]
  , avroRecordProps     = Map.empty
  }

decodeWithEvolution :: ByteString -> Either String AV.Value
decodeWithEvolution bytes =
  decodeAvroResolved writerSchema readerSchema bytes
```

For self-describing files, write and read Object Container Files:

```haskell
import qualified Avro.Container as OCF

writeRecords :: AvroType -> [AV.Value] -> ByteString
writeRecords schema vals = OCF.writeContainer schema (V.fromList vals)

readRecords :: ByteString -> Either String (AvroType, V.Vector AV.Value)
readRecords bytes = OCF.readContainer bytes
```

## Notable modules

| Module | Purpose |
|--------|---------|
| `Avro.Class` | `ToAvro` / `FromAvro` typeclasses |
| `Avro.Encode` / `Avro.Decode` | Schema-driven `encodeAvro` / `decodeAvro` / `decodeAvroResolved` |
| `Avro.Value` | Dynamic untyped `Value` ADT |
| `Avro.Schema` / `Avro.Schema.Parse` | Schema AST and `.avsc` JSON parser |
| `Avro.IDL` / `Avro.IDLConvert` | `.avdl` IDL parser and converter |
| `Avro.Derive` | `deriveAvro`, `deriveHasAvroSchema`, `HasAvroSchema` |
| `Avro.CodeGen` / `Avro.QQ` | Haskell codegen and `[avsc\| ... \|]` quasiquoter |
| `Avro.Container` | Object Container File reader and writer |
| `Avro.Resolution` | Writer/reader schema resolution rules |
| `Avro.Fingerprint` | CRC-64-AVRO and SHA-256 schema fingerprints |
| `Avro.Protocol` | Avro RPC protocol envelope |
| `Avro.Registry` | Runtime schema registry |
| `Avro.JSON` | Avro JSON encoding bridge |
