module Network.HTTP2.HPACK.Types
  ( Token (..)
  , TokenHeader
  , IndexingStrategy (..)
  , DecodeError (..)
  , EncodeStrategy (..)
  , defaultEncodeStrategy
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)

data Token = Token
  { tokenIndex :: !Int
  , tokenName :: !ByteString
  , tokenShouldIndex :: !Bool
  }
  deriving stock (Eq, Show)

type TokenHeader = (Token, ByteString)

data IndexingStrategy
  = IndexingYes
  | IndexingNo
  | IndexingNever
  deriving stock (Eq, Show)

data DecodeError
  = IndexOutOfRange !Int
  | InvalidHuffmanEncoding
  | IntegerOverflow
  | InvalidTableSizeUpdate !Int
  | HeaderBlockTruncated
  deriving stock (Eq, Show, Generic)

instance NFData DecodeError

data EncodeStrategy = EncodeStrategy
  { useHuffman :: !Bool
  , useDynamicTable :: !Bool
  }
  deriving stock (Eq, Show)

defaultEncodeStrategy :: EncodeStrategy
defaultEncodeStrategy = EncodeStrategy
  { useHuffman = True
  , useDynamicTable = True
  }
