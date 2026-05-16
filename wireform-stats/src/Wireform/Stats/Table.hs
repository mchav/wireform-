-- | Markdown table renderer.
--
-- Used to materialise the \"numbers\" half of a benchmark / coverage /
-- test region. Output is GitHub-flavoured markdown with column
-- alignment markers (@:---@, @---:@, @:---:@) so the rendered table
-- aligns the way the caller chose.
module Wireform.Stats.Table
  ( -- * Table
    Table (..)
  , Align (..)
  , renderTable
    -- * Common helpers
  , renderInlineCode
  ) where

import Data.Text (Text)
import Data.Text qualified as T

-- | A markdown table.
--
-- 'tableHeader' and 'tableAlign' must have the same length; each row
-- in 'tableRows' is padded or truncated to the header length.
data Table = Table
  { tableHeader :: ![Text]
  , tableAlign  :: ![Align]
  , tableRows   :: ![[Text]]
  } deriving stock (Eq, Show)

-- | Per-column alignment.
data Align = AlignLeft | AlignRight | AlignCenter
  deriving stock (Eq, Show)

-- | Render the table as GitHub-flavoured markdown. Each column is
-- padded so the rendered source is human-readable; readers don't see
-- the padding because GitHub collapses whitespace inside cells.
renderTable :: Table -> Text
renderTable t =
  let header = tableHeader t
      align  = tableAlign  t
      rows   = map (padRow (length header)) (tableRows t)
      widths = map (\(i, h) ->
                     maximum (T.length h : map (cellWidth i) rows))
                   (zip [0 ..] header)
      headerLine = renderRow widths align header
      sepLine    = renderSep widths align
      bodyLines  = map (renderRow widths align) rows
  in T.unlines (headerLine : sepLine : bodyLines)
  where
    padRow n cells = take n (cells ++ repeat "")
    cellWidth i row = case drop i row of
      (c : _) -> T.length c
      []      -> 0

renderRow :: [Int] -> [Align] -> [Text] -> Text
renderRow widths aligns cells =
  "| " <> T.intercalate " | " (zipWith3 padCell widths aligns cells) <> " |"

padCell :: Int -> Align -> Text -> Text
padCell w a c = case a of
  AlignLeft   -> c <> T.replicate (max 0 (w - T.length c)) " "
  AlignRight  -> T.replicate (max 0 (w - T.length c)) " " <> c
  AlignCenter ->
    let total = max 0 (w - T.length c)
        l     = total `div` 2
        r     = total - l
    in T.replicate l " " <> c <> T.replicate r " "

renderSep :: [Int] -> [Align] -> Text
renderSep widths aligns =
  "| " <> T.intercalate " | " (zipWith sepCell widths aligns) <> " |"
  where
    sepCell w a =
      let bar = T.replicate (max 3 w) "-"
      in case a of
           AlignLeft   -> ":" <> T.drop 1 bar
           AlignRight  -> T.dropEnd 1 bar <> ":"
           AlignCenter -> ":" <> T.dropEnd 1 (T.drop 1 bar) <> ":"

-- | Wrap a value in single-backticks for the table cell.
renderInlineCode :: Text -> Text
renderInlineCode t = "`" <> t <> "`"
