module Network.HTTP2.HPACK
  ( -- * Encoding
    encodeHeaderBlock
  , encodeHeader
  , EncodeStrategy (..)
  , defaultEncodeStrategy
    -- * Decoding
  , decodeHeaderBlock
    -- * Dynamic table
  , DynamicTable
  , newDynamicTable
  , setMaxSize
  , tableSize
  , tableMaxSize
    -- * Huffman
  , huffmanEncode
  , huffmanDecode
  , huffmanEncodeLength
    -- * Types
  , DecodeError (..)
  ) where

import Network.HTTP2.HPACK.Decode
import Network.HTTP2.HPACK.Encode
import Network.HTTP2.HPACK.Huffman
import Network.HTTP2.HPACK.Table
import Network.HTTP2.HPACK.Types
