-- | Adapt an attoparsec 'A.Parser' to a wireform 'ChunkParser'.
--
-- @
-- import Wireform.Parser.Adapter.Attoparsec (fromAttoparsec)
-- import qualified Data.Attoparsec.ByteString as A
--
-- myParser :: A.Parser MyType
-- myParser = ...
--
-- main = withRecvTransport cfg sock $ \\t ->
--   runChunked t ChunkCopy (fromAttoparsec myParser) >>= print
-- @
module Wireform.Parser.Adapter.Attoparsec (fromAttoparsec) where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import Wireform.Parser.Adapter

-- | Convert an attoparsec parser to a 'ChunkParser'.
fromAttoparsec :: A.Parser a -> ChunkParser a
fromAttoparsec p = go (A.parse p)
  where
    go cont = ChunkParser
      { stepChunk = \bs -> case cont bs of
          A.Done leftover x -> ChunkDone x (BS.length bs - BS.length leftover)
          A.Partial cont'   -> ChunkConsumed (BS.length bs) (go cont')
          A.Fail _ ctx msg  -> ChunkFailed (ChunkParseError msg ctx 0)
      , stepEof = case cont BS.empty of
          A.Done _ x       -> FinalDone x
          A.Partial _      -> FinalFailed (ChunkParseError "unexpected EOF" [] 0)
          A.Fail _ ctx msg -> FinalFailed (ChunkParseError msg ctx 0)
      }
