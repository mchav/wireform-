{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Util.HeaderTable (
    -- * General auxiliary
    fromHeaderTable,
) where

import Network.GRPC.Util.Imports
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI

import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Engine.Types qualified as Engine

{-------------------------------------------------------------------------------
  General auxiliary
-------------------------------------------------------------------------------}


fromHeaderTable :: Engine.TokenHeaderTable -> [HTTP.Header]
fromHeaderTable =
      map (first (CI.mk . Engine.tokenCIKey))
    . filter (not . isPseudo)
    . fst
  where
    isPseudo (Engine.Token k, _) = ":" `BS.isPrefixOf` k
