-- | Rust-quality error rendering for protobuf parser diagnostics.
--
-- Produces output like:
--
-- @
-- error: unexpected end of input
--   --> example.proto:5:25
--    |
--  5 |   string name = 1
--    |                   ^ expected ';' after field definition
--    |
-- @
module Proto.Parser.Error
  ( renderParseErrors
  , renderParseError
  ) where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Error (ErrorItem(..), ErrorFancy(..))

-- | Render all errors in a ParseErrorBundle to a single Rust-style diagnostic string.
renderParseErrors :: ParseErrorBundle Text Void -> String
renderParseErrors bundle =
  let errs = NE.toList (bundleErrors bundle)
      src = pstateInput (bundlePosState bundle)
      sourceLines = T.lines src
      renderedErrors = fmap (renderOneError sourceLines bundle) errs
  in intercalate "\n" renderedErrors

-- | Render a single ParseErrorBundle (convenience for the common single-error case).
renderParseError :: ParseErrorBundle Text Void -> String
renderParseError = renderParseErrors

renderOneError :: [Text] -> ParseErrorBundle Text Void -> ParseError Text Void -> String
renderOneError sourceLines bundle err =
  let offset = errorOffset err
      pst = bundlePosState bundle
      (_, pst') = reachOffset offset pst
      sp = pstateSourcePos pst'
      filePath = sourceName sp
      line = unPos (sourceLine sp)
      col = unPos (sourceColumn sp)
      (summary, details) = describeError err
      lineNumWidth = length (show line)
      pad = replicate lineNumWidth ' '
      contextBefore = if line >= 2
        then showSourceLine sourceLines (line - 1) lineNumWidth
        else Nothing
      mainLine = showSourceLine sourceLines line lineNumWidth
      pointer = makePointer lineNumWidth col (pointerWidth err sourceLines line col)
  in unlines $ catMaybes
    [ Just $ "error: " <> summary
    , Just $ pad <> " --> " <> filePath <> ":" <> show line <> ":" <> show col
    , Just $ pad <> " |"
    , contextBefore
    , mainLine
    , Just $ pointer <> " " <> details
    , Just $ pad <> " |"
    ]

showSourceLine :: [Text] -> Int -> Int -> Maybe String
showSourceLine sourceLines lineNum lineNumWidth
  | lineNum < 1 || lineNum > length sourceLines = Nothing
  | otherwise =
    let content = T.unpack (sourceLines !! (lineNum - 1))
        num = show lineNum
        padding = replicate (lineNumWidth - length num) ' '
    in Just $ padding <> num <> " | " <> content

makePointer :: Int -> Int -> Int -> String
makePointer lineNumWidth col width =
  let pad = replicate lineNumWidth ' '
      caretPad = replicate (col - 1) ' '
      carets = if width <= 1
        then "^"
        else replicate width '^'
  in pad <> " | " <> caretPad <> carets

pointerWidth :: ParseError Text Void -> [Text] -> Int -> Int -> Int
pointerWidth err sourceLines line col =
  case err of
    TrivialError _ (Just (Tokens ts)) _ ->
      max 1 (NE.length ts)
    TrivialError _ (Just EndOfInput) _ -> 1
    TrivialError _ (Just (Label _)) _ -> 1
    TrivialError _ Nothing _ -> 1
    FancyError _ _ -> 1

describeError :: ParseError Text Void -> (String, String)
describeError (TrivialError _ unexpected' expected') =
  let summary = describeUnexpected unexpected'
      details = describeExpected expected'
  in (summary, details)
describeError (FancyError _ fancyErrors) =
  let msgs = Set.toList fancyErrors
  in case msgs of
    [] -> ("syntax error", "")
    _  -> (describeFancySet msgs, "")

describeUnexpected :: Maybe (ErrorItem Char) -> String
describeUnexpected Nothing = "syntax error"
describeUnexpected (Just item) = "unexpected " <> describeItem item

describeItem :: ErrorItem Char -> String
describeItem (Tokens ts) =
  let s = NE.toList ts
  in case s of
    [c] | c == '\n' -> "newline"
        | c == '\t' -> "tab"
        | c == ' '  -> "space"
        | otherwise  -> "'" <> [c] <> "'"
    _ -> "\"" <> escapeString s <> "\""
describeItem (Label cs) = NE.toList cs
describeItem EndOfInput = "end of input"

escapeString :: String -> String
escapeString = concatMap go
  where
    go '\n' = "\\n"
    go '\t' = "\\t"
    go '\r' = "\\r"
    go c    = [c]

describeExpected :: Set (ErrorItem Char) -> String
describeExpected items
  | Set.null items = ""
  | otherwise =
    let groups = categorizeExpected (Set.toList items)
    in "expected " <> formatExpectedGroups groups

data ExpectedGroup = ExpectedGroup
  { egLabels  :: [String]
  , egTokens  :: [String]
  , egEOI     :: Bool
  }

categorizeExpected :: [ErrorItem Char] -> ExpectedGroup
categorizeExpected = foldl go (ExpectedGroup [] [] False)
  where
    go acc (Label cs) = acc { egLabels = NE.toList cs : egLabels acc }
    go acc (Tokens ts) = acc { egTokens = showToken (NE.toList ts) : egTokens acc }
    go acc EndOfInput = acc { egEOI = True }

showToken :: String -> String
showToken [c] | c == ';' = "';'"
              | c == '{' = "'{'"
              | c == '}' = "'}'"
              | c == '=' = "'='"
              | c == '(' = "'('"
              | c == ')' = "')'"
              | c == '<' = "'<'"
              | c == '>' = "'>'"
              | c == '[' = "'['"
              | c == ']' = "']'"
              | c == ',' = "','"
              | c == '.' = "'.'"
              | c == '"' = "'\"'"
              | c == '\'' = "\"'\""
              | c == '\n' = "newline"
              | c == ' ' = "space"
showToken s = "\"" <> s <> "\""

formatExpectedGroups :: ExpectedGroup -> String
formatExpectedGroups (ExpectedGroup labels tokens eoi) =
  let allParts = labels <> tokens <> ["end of input" | eoi]
  in case allParts of
    []  -> "something"
    [x] -> x
    _   -> commaOr allParts

commaOr :: [String] -> String
commaOr [] = ""
commaOr [x] = x
commaOr [x, y] = x <> " or " <> y
commaOr xs =
  let front = init xs
      end = last xs
  in intercalate ", " front <> ", or " <> end

describeFancySet :: [ErrorFancy Void] -> String
describeFancySet = intercalate "; " . fmap describeFancy

describeFancy :: ErrorFancy Void -> String
describeFancy (ErrorFail msg) = msg
describeFancy (ErrorIndentation ord ref actual) =
  "incorrect indentation (got " <> show (unPos actual)
  <> ", should be " <> showOrd ord <> show (unPos ref) <> ")"
describeFancy (ErrorCustom v) = absurd v
  where absurd :: Void -> a
        absurd x = case x of {}

showOrd :: Ordering -> String
showOrd LT = "less than "
showOrd EQ = "equal to "
showOrd GT = "greater than "
