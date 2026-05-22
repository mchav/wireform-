-- | Adapt a @binary@ 'B.Get' to a wireform 'ChunkParser'.
module Wireform.Parser.Adapter.Binary (fromBinary) where

import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Wireform.Parser.Adapter

-- | Convert a @binary@ 'B.Get' decoder to a 'ChunkParser'.
fromBinary :: B.Get a -> ChunkParser a
fromBinary g = goDecoder (B.runGetIncremental g)
  where
    goDecoder d = ChunkParser
      { stepChunk = \bs -> step (B.pushChunk d bs) (BS.length bs)
      , stepEof   = case B.pushEndOfInput d of
          B.Done _ _ x   -> FinalDone x
          B.Fail _ _ msg -> FinalFailed (ChunkParseError msg [] 0)
          B.Partial _    -> FinalFailed (ChunkParseError "unexpected EOF" [] 0)
      }
    step (B.Done leftover _ x) fed = ChunkDone x (fed - BS.length leftover)
    step (B.Partial k) fed         = ChunkConsumed fed (goDecoder (B.Partial k))
    step (B.Fail _ _ msg) _        = ChunkFailed (ChunkParseError msg [] 0)
