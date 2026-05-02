{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Template Haskell deriver for 'Arrow.Record.Table'.
--
-- When a record type doesn't have a 'GHC.Generics.Generic'
-- instance, or callers want explicit control over column names
-- (e.g. @"user_id"@ for a selector named @userId@), use the
-- 'deriveTable' splice instead of @'Arrow.Record.Generic.genericTable'@.
--
-- @
-- {-# LANGUAGE TemplateHaskell #-}
--
-- import Arrow.Record.TH
--
-- data Trade = Trade { sym :: Text, qty :: Int32, note :: Maybe Text }
--
-- tradeTable :: 'Table' Trade
-- tradeTable = $('deriveTable' ''Trade)
-- @
--
-- or with a custom column-name transformer (e.g. snake_case):
--
-- @
-- tradeTable :: 'Table' Trade
-- tradeTable = $('deriveTableWith' toSnakeCase ''Trade)
--   where
--     toSnakeCase = ... :: String -> String
-- @
--
-- The splice emits code equivalent to a hand-written
-- 'Arrow.Record.table' expression: 'Arrow.Record.fieldE' for
-- every record selector, 'Arrow.Record.columnD' on the decoder
-- side. Field types must have 'HasEncoder' / 'HasDecoder'
-- instances from "Arrow.Record.Generic".
module Arrow.Record.TH
  ( deriveTable
  , deriveTableWith
  ) where

import qualified Data.Text as T
import Language.Haskell.TH

import Arrow.Record
  ( columnD
  , fieldE
  , table
  )
import Arrow.Record.Generic (HasDecoder (hasDecoder), HasEncoder (hasEncoder))

-- | Derive a 'Table' for the named record type, using the
-- record selectors as column names verbatim.
deriveTable :: Name -> Q Exp
deriveTable = deriveTableWith id

-- | Like 'deriveTable' but renames columns via the supplied
-- pure function. Common choices: @'map' 'Data.Char.toLower'@,
-- a snake-case converter, or a full alias map via
-- @\\n -> 'Data.Map.findWithDefault' n n aliasMap@.
deriveTableWith :: (String -> String) -> Name -> Q Exp
deriveTableWith renameField tyName = do
  info <- reify tyName
  fields <- case info of
    TyConI (DataD _ _ _ _ [RecC _ fs] _) -> pure fs
    TyConI (NewtypeD _ _ _ _ (RecC _ fs) _) -> pure fs
    _ -> fail $
      "Arrow.Record.TH.deriveTable: " ++ show tyName
        ++ " must be a single-constructor record type"

  -- Build the RowEncoder: mconcat of fieldE "name" selector hasEncoder.
  let mkEncoderPart (selName', _, _) = do
        let colName = renameField (nameBase selName')
        [| fieldE (T.pack colName) $(varE selName') hasEncoder |]
  encoderParts <- mapM mkEncoderPart fields
  let encoderExp = foldr1 (\a b -> [| $a <> $b |]) (map pure encoderParts)

  -- Build the RowDecoder: Ctor <$> columnD "name1" hasDecoder <*> ...
  let ctorName = case info of
        TyConI (DataD _ _ _ _ [RecC c _] _) -> c
        TyConI (NewtypeD _ _ _ _ (RecC c _) _) -> c
        _ -> error "unreachable: validated above"
  let mkDecoderPart (selName', _, _) =
        let colName = renameField (nameBase selName')
        in  [| columnD (T.pack colName) hasDecoder |]
      initial = [| pure $(conE ctorName) |]
      combine acc step = [| $acc <*> $step |]
  decoderExp <- foldl combine initial (map mkDecoderPart fields)

  encoderExp' <- encoderExp
  -- Build: table encoderExp decoderExp
  [| table $(pure encoderExp') $(pure decoderExp) |]
