-- | Adapt a @cereal@ 'S.Get' to a wireform 'ChunkParser'.
module Wireform.Parser.Adapter.Cereal (fromCereal) where

import qualified Data.Serialize.Get as S
import qualified Data.ByteString as BS
import Wireform.Parser.Adapter

-- | Convert a @cereal@ 'S.Get' decoder to a 'ChunkParser'.
fromCereal :: S.Get a -> ChunkParser a
fromCereal g = goResult (S.runGetPartial g)
  where
    goResult cont = ChunkParser
      { stepChunk = \bs -> case cont bs of
          S.Done a leftover -> ChunkDone a (BS.length bs - BS.length leftover)
          S.Partial cont'   -> ChunkConsumed (BS.length bs) (goResult cont')
          S.Fail msg _      -> ChunkFailed (ChunkParseError msg [] 0)
      , stepEof = case cont BS.empty of
          S.Done a _     -> FinalDone a
          S.Partial _    -> FinalFailed (ChunkParseError "unexpected EOF" [] 0)
          S.Fail msg _   -> FinalFailed (ChunkParseError msg [] 0)
      }
