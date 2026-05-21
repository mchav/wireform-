---
title: wireform-html
description: "HTML5 parser, CSS Selectors Level 4, streaming rewriter, and Template Haskell deriving with a C SIMD scanner."
sidebar:
  order: 21
---

`wireform-html` implements the HTML5 parsing algorithm and the DOM-style tools
you need on top of it: CSS selector queries, a streaming rewriter, incremental
parsing, and typed `ToHTML`/`FromHTML` deriving. Use it when you scrape or
transform HTML in Haskell and want spec-correct tree construction (validated
against html5lib, 1,779 / 1,779 tests passing) plus fast byte-level rewriting
without building a full DOM.

## Key features

| Capability | Module | Why it matters |
|------------|--------|----------------|
| HTML5 tree builder | `HTML.Parse`, `HTML.DOM` | Spec-compliant documents and fragments |
| CSS Selectors Level 4 | `HTML.Selector`, `HTML.DOM` | Query parsed trees with familiar selector syntax |
| Streaming rewriter | `HTML.Rewriter` | lol-html-style transforms in one pass, bounded memory |
| Incremental parsing | `HTML.DOM` | Feed HTML chunks as they arrive |
| Template Haskell deriving | `HTML.Class`, `HTML.Derive` | `deriveHTML` with wireform-derive annotations; Generic defaults for simple cases |
| C SIMD scanner | `cbits/fast_html.c` | Vectorized tag and text scanning on hot paths |

## Basic usage

### Parse a document and query with CSS selectors

Build a document once, then use selector strings the same way you would in
browser DevTools. The parser also builds a pre-order element index so repeated
queries stay fast.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.ByteString (ByteString)
import HTML.DOM
  ( parseDocument
  , documentElement
  , querySelectorAll
  , getAttribute
  )

extractLinkHrefs :: ByteString -> [Text]
extractLinkHrefs html =
  let doc = parseDocument html
      root = documentElement doc
      links = querySelectorAll root "a[href]"
  in map (\node -> fromMaybe "" (getAttribute node "href")) links
```

For a single match, `querySelector` returns `Maybe Node`. Pre-parse selectors
with `HTML.Selector.parseSelector` when you run the same query many times.

### Streaming rewriter: mutate HTML in one pass

The rewriter fires callbacks when CSS selectors match and writes transformed
output without materializing a tree. Memory scales with nesting depth and the
number of registered selectors, not document size. This is the right tool for
adding attributes, rewriting tags, or redacting text in large HTML files.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString (ByteString)
import HTML.Rewriter
  ( buildRewriter
  , onElement
  , setElemAttr
  , rewrite
  )
import HTML.Selector (parseSelector)

addNoopener :: ByteString -> IO ByteString
addNoopener input = do
  let Right sel = parseSelector "a[target=_blank]"
  let Right rw = buildRewriter $
        onElement sel $ \er ->
          setElemAttr er "rel" "noopener noreferrer"
  rewrite rw input
```

Register handlers with `onText`, `onComment`, and `onDoctype` for non-element
nodes. Element mutation helpers include `setTagName`, `replaceElement`,
`prependToElement`, and `removeElement`.

### Typed HTML fragments

When your output is structured data rendered as HTML, derive `ToHTML` with the
Template Haskell deriver and emit with `encodeHTMLTyped`.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Text (Text)
import HTML.Class (ToHTML, encodeHTMLTyped)
import HTML.Derive (deriveHTML)

data Person = Person
  { name :: !Text
  , role :: !Text
  } deriving stock (Generic)

$(deriveHTML ''Person)

renderPerson :: Person -> Text
renderPerson = encodeHTMLTyped
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToHTML Person` and
`instance FromHTML Person` declarations.

### Incremental parsing

For network streams or chunked file reads, create a `Parser` with `newParser`,
call `feedParser` for each chunk, and finish with `finishParser`. The tree
builder carries incomplete tag fragments across chunk boundaries.

## Notable modules

| Module | Role |
|--------|------|
| `HTML.Parse` | HTML5 tokenizer and tree-construction algorithm |
| `HTML.DOM` | Zipper-based `Node` API, `parseDocument`, selector queries |
| `HTML.Selector` | CSS Selectors Level 4 parser and matcher |
| `HTML.Rewriter` | Single-pass streaming rewriter with selector callbacks |
| `HTML.Class` / `HTML.Derive` | `ToHTML` / `FromHTML` and annotation-driven TH |
| `HTML.Encode` | Serialize DOM nodes back to HTML bytes |
| `HTML.TagId` | Intern table for hot tag-name comparisons |
