---
title: wireform-xml
description: "Full XML pipeline: SAX and DOM parsing, XPath queries, XSLT transforms, XSD codegen, and Generic deriving."
sidebar:
  order: 20
---

`wireform-xml` is wireform's XML 1.0 package. It covers the full lifecycle from
byte scanning through tree construction, querying, transformation, and typed
serialization. Reach for it when you need predictable, fast XML handling in
Haskell without pulling in a heavyweight foreign parser, or when you want one
library that can stream large documents, build a queryable DOM, and derive
`ToXML`/`FromXML` from your existing types.

## Key features

| Capability | Module | Why it matters |
|------------|--------|----------------|
| SAX event parser | `XML.SAX` | Constant-memory streaming over large files |
| Zero-copy DOM | `XML.FastDOM` | Sub-millisecond scans when you only need slices into the source bytes |
| Allocating tree DOM | `XML.Decode`, `XML.Value` | Mutable-free tree for XPath, XSLT, and typed decoding |
| XPath queries | `XML.Path` | Navigate and filter without hand-rolling recursive walks |
| XSLT 1.0 | `XML.XSLT` | Apply stylesheets for report generation and legacy integrations |
| XSD codegen | `XML.CodeGen`, `XML.QQ` | Generate Haskell types from schema at compile time or via CLI |
| Incremental parsing | `XML.Incremental` | Feed chunks as they arrive on a socket or from disk |
| Generic deriving | `XML.Class`, `XML.Derive` | Map Haskell records to XML elements with `Generic` |
| C SIMD scanner | `cbits/fast_xml.c` | Vectorized scanning on text-heavy documents |

## Basic usage

### SAX: stream events without building a tree

SAX fits pipelines where you count elements, extract a few fields, or forward
events downstream. Memory stays bounded because the parser never materializes a
full document.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.ByteString (ByteString)
import XML.SAX (SAXEvent(..), parseSAXStream)

countElements :: ByteString -> IO Int
countElements bs = do
  ref <- newIORef (0 :: Int)
  _ <- parseSAXStream bs $ \ev -> case ev of
    StartElement _ _ -> modifyIORef' ref (+ 1)
    _                -> pure ()
  readIORef ref
```

### DOM: parse once, query many times

When you need random access or repeated queries, build a tree with `decode` and
walk it with XPath-style helpers.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import XML.Decode (decode)
import XML.Path (queryPath, textContent)
import XML.Value (docRoot)

extractTitles :: ByteString -> Either String (Vector Text)
extractTitles bs = do
  doc <- decode bs
  let books = queryPath ["catalog", "book"] (docRoot doc)
  pure $ V.map textContent books
```

For read-only workloads where string data should stay in the original buffer,
`parseFast` from `XML.FastDOM` returns span-based nodes and avoids `Text`
allocation during the scan.

### XPath: locate nodes by path

`XML.Path` implements a practical XPath subset: child and descendant axes,
attribute predicates, indexing, and wildcards. Parse a path string once, then
reuse it across many documents.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import XML.Decode (decode)
import XML.Path (parsePath, query, textContent)
import XML.Value (docRoot)

findBySku :: ByteString -> Text -> Either String (Maybe Text)
findBySku bs sku = do
  doc <- decode bs
  path <- parsePath ("inventory/item[@sku='" <> sku <> "']/name")
  case V.uncons (query path (docRoot doc)) of
    Nothing        -> Right Nothing
    Just (node, _) -> Right (Just (textContent node))
```

### Typed records with Generic

For application-level messages, derive `ToXML` and `FromXML` and round-trip with
`encodeXML` / `decodeXML`.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Text (Text)
import XML.Class (ToXML, FromXML, encodeXML, decodeXML)

data Book = Book
  { title  :: !Text
  , author :: !Text
  , year   :: !Int
  } deriving stock (Generic)
    deriving anyclass (ToXML, FromXML)

roundtrip :: Book -> Either String Book
roundtrip book = decodeXML (encodeXML book)
```

For schema-driven types, use the `[xsd| ... |]` quasiquoter or
`wireform-gen xsd` to generate modules from XSD at compile time or in CI.

## Notable modules

| Module | Role |
|--------|------|
| `XML.SAX` | Event parser with `parseSAX`, `parseSAXStream`, and `foldSAX` |
| `XML.FastDOM` | Zero-copy DOM with span-based string access |
| `XML.Decode` | SAX-to-DOM builder producing `XML.Value.Document` |
| `XML.Path` | XPath-lite cursor API over `Node` values |
| `XML.XSLT` | XSLT 1.0 stylesheet application |
| `XML.CodeGen` / `XML.QQ` | XSD-to-Haskell codegen (CLI and Template Haskell) |
| `XML.Incremental` | Chunk-fed parser for concurrent or streaming input |
| `XML.Class` / `XML.Derive` | `ToXML` / `FromXML` typeclasses and TH deriver |
| `XML.JSON` | Bridge between XML values and JSON for tooling |
