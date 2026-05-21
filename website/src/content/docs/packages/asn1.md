---
title: wireform-asn1
description: "ASN.1 BER/DER encoding and decoding with module definition parser, tagging modes, constraints, and codegen."
sidebar:
  order: 35
---

`wireform-asn1` implements ASN.1 Basic Encoding Rules (BER) and Distinguished
Encoding Rules (DER) per [ITU-T X.690](https://www.itu.int/rec/T-REC-X.690).
ASN.1 underpins X.509 certificates, LDAP, SNMP, Kerberos, and smart-card
protocols. DER is the canonical subset required for cryptographic uses. Use this
package when you need standards-compliant DER output, ASN.1 module parsing, or
typed encoding of certificate and telecom structures.

## Key features

- **Typeclass API** via `ToASN1` and `FromASN1` with `encodeASN1` / `decodeASN1`
- **ITU-T X.690 DER encoder** producing canonical byte sequences
- **ASN.1 module definition parser** for `.asn1` schema files
- **Schema AST** with tagging modes (Automatic, Implicit, Explicit) and constraints
- **Codegen and QuasiQuoter** for inline `[asn1| ... |]` modules
- **Template Haskell deriver** with `asn1ImplicitTag` and `asn1ExplicitTag` modifiers

## Basic usage

Derive instances for your record types, then encode to DER and decode back:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import ASN1.Derive
  ( ToASN1, FromASN1
  , encodeASN1, decodeASN1
  , deriveASN1
  , asn1ImplicitTag
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Person = Person
  { personId    :: !Int
  , personName  :: !Text
  , personAdmin :: !Bool
  }
  deriving stock (Show, Eq, Generic)

{-# ANN personAdmin (asn1ImplicitTag 0) #-}

$(deriveASN1 ''Person)

carol :: Person
carol = Person 1 "Carol" True

encodePerson :: Person -> ByteString
encodePerson = encodeASN1

decodePerson :: ByteString -> Either String Person
decodePerson = decodeASN1

roundTrip :: Either String Person
roundTrip = decodePerson (encodePerson carol)
```

For certificate-shaped structures, work directly with the dynamic `Value` ADT
when you need fine-grained control over tagging:

```haskell
import qualified Data.Vector as V
import qualified ASN1.Value as AV
import qualified ASN1.Encode as AE
import qualified ASN1.Decode as AD

tbsPrefix :: AV.Value
tbsPrefix = AV.Sequence $ V.fromList
  [ AV.Tagged AV.ContextSpecific 0 (AV.Integer 2)  -- X.509 version v3
  , AV.Integer 12345                              -- serial number
  ]

derBytes :: ByteString
derBytes = AE.encode tbsPrefix

parseDer :: Either String AV.Value
parseDer = AD.decode derBytes
```

Generate types from ASN.1 modules:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import ASN1.QQ (asn1)

[asn1|
  Person DEFINITIONS ::= BEGIN
    Person ::= SEQUENCE {
      id    INTEGER,
      name  UTF8String,
      admin BOOLEAN
    }
  END
|]
```

```bash
wireform-gen asn1 -i module.asn1 -o src/Gen/
```

## Performance

### DER encode/decode

| Payload | encode | decode |
|---------|--------|--------|
| Subject | 141 ns | 115 ns |
| [Subject] x 100 | 16.9 µs | 12.8 µs |

Sub-microsecond per-record encode and decode. The DER codec is allocation-lean with unboxed field codecs.

Criterion, GHC 9.8.4, Apple Silicon. See `wireform-asn1/bench-results/` for raw data.

## Notable modules

| Module | Purpose |
|--------|---------|
| `ASN1.Derive` | `ToASN1` / `FromASN1`, `encodeASN1` / `decodeASN1`, `deriveASN1` |
| `ASN1.Encode` / `ASN1.Decode` | BER/DER wire encoder and decoder |
| `ASN1.Value` | Dynamic untyped `Value` ADT (Sequence, Tagged, Integer, etc.) |
| `ASN1.Schema` / `ASN1.Parser` | Schema AST and module definition parser |
| `ASN1.CodeGen` / `ASN1.QQ` | Haskell codegen and quasiquoter |
