{- | Rust-quality error rendering for protobuf parser diagnostics.

Produces output like:

@
error: unexpected end of input
  --> example.proto:5:25
   |
 5 |   string name = 1
   |                   ^ expected ';' after field definition
   |
@
-}
module Proto.IDL.Parser.Error (
  renderParseErrors,
  renderParseError,
) where

import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, isNothing)
import Data.Maybe qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec


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
      lineNumWidth = max 1 (length (show line))
      pad = replicate lineNumWidth ' '
      mainLine = showSourceLine sourceLines line lineNumWidth
      -- When error is past EOF (line doesn't exist), show the last available line
      -- and point to the end of it
      useFallback = isNothing mainLine && line >= 2
      fallbackLine =
        if useFallback
          then showSourceLine sourceLines (line - 1) lineNumWidth
          else Nothing
      effectiveLine = if Data.Maybe.isJust mainLine then mainLine else fallbackLine
      effectiveCol =
        if Data.Maybe.isJust mainLine
          then col
          else maybe col (\l -> T.length l + 1) (getSourceLine sourceLines (line - 1))
      -- Show context line before the effective error line, avoiding duplicates
      contextLineNum = if useFallback then line - 2 else line - 1
      contextBefore =
        if contextLineNum >= 1
          then showSourceLine sourceLines contextLineNum lineNumWidth
          else Nothing
      pointer = makePointer lineNumWidth effectiveCol (pointerWidth err)
  in unlines $
      catMaybes
        [ Just $ "error: " <> summary
        , Just $ pad <> " --> " <> filePath <> ":" <> show line <> ":" <> show col
        , Just $ pad <> " |"
        , contextBefore
        , effectiveLine
        , Just $ pointer <> " " <> details
        , Just $ pad <> " |"
        ]


getSourceLine :: [Text] -> Int -> Maybe Text
getSourceLine sourceLines lineNum
  | lineNum < 1 || lineNum > length sourceLines = Nothing
  | otherwise = Just (sourceLines !! (lineNum - 1))


showSourceLine :: [Text] -> Int -> Int -> Maybe String
showSourceLine sourceLines lineNum lineNumWidth = do
  content <- getSourceLine sourceLines lineNum
  let num = show lineNum
      padding = replicate (lineNumWidth - length num) ' '
  pure $ padding <> num <> " | " <> T.unpack content


makePointer :: Int -> Int -> Int -> String
makePointer lineNumWidth col width =
  let pad = replicate lineNumWidth ' '
      caretPad = replicate (max 0 (col - 1)) ' '
      carets =
        if width <= 1
          then "^"
          else replicate width '^'
  in pad <> " | " <> caretPad <> carets


pointerWidth :: ParseError Text Void -> Int
pointerWidth = \case
  TrivialError _ (Just (Tokens ts)) _ -> max 1 (NE.length ts)
  _ -> 1


describeError :: ParseError Text Void -> (String, String)
describeError (TrivialError _ unexpected' expected') =
  (describeUnexpected unexpected', describeExpected expected')
describeError (FancyError _ fancyErrors) =
  case Set.toList fancyErrors of
    [] -> ("syntax error", "")
    msgs -> (describeFancySet msgs, "")


describeUnexpected :: Maybe (ErrorItem Char) -> String
describeUnexpected Nothing = "syntax error"
describeUnexpected (Just item) = "unexpected " <> describeItem item


describeItem :: ErrorItem Char -> String
describeItem (Tokens ts) =
  let s = NE.toList ts
  in case s of
      [c]
        | c == '\n' -> "newline"
        | c == '\t' -> "tab"
        | c == ' ' -> "space"
        | otherwise -> "'" <> [c] <> "'"
      _ -> "\"" <> escapeString s <> "\""
describeItem (Label cs) = NE.toList cs
describeItem EndOfInput = "end of input"


escapeString :: String -> String
escapeString = concatMap go
  where
    go '\n' = "\\n"
    go '\t' = "\\t"
    go '\r' = "\\r"
    go c = [c]


describeExpected :: Set (ErrorItem Char) -> String
describeExpected items
  | Set.null items = ""
  | otherwise =
      let groups = categorizeExpected (Set.toList items)
      in "expected " <> formatExpectedGroups groups


data ExpectedGroup = ExpectedGroup
  { egLabels :: [String]
  , egTokens :: [String]
  , egEOI :: Bool
  }


categorizeExpected :: [ErrorItem Char] -> ExpectedGroup
categorizeExpected = foldl go (ExpectedGroup [] [] False)
  where
    go acc (Label cs) = acc {egLabels = NE.toList cs : egLabels acc}
    go acc (Tokens ts) = acc {egTokens = showToken (NE.toList ts) : egTokens acc}
    go acc EndOfInput = acc {egEOI = True}


showToken :: String -> String
showToken [c]
  | c == ';' = "';'"
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
formatExpectedGroups (ExpectedGroup labels toks eoi) =
  let allParts = labels <> toks <> (if eoi then ["end of input"] else [])
  in case allParts of
      [] -> "something"
      [x] -> x
      _ -> commaOr allParts


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
  "incorrect indentation (got "
    <> show (unPos actual)
    <> ", should be "
    <> showOrd ord
    <> show (unPos ref)
    <> ")"
describeFancy (ErrorCustom v) = absurd v
  where
    absurd :: Void -> a
    absurd x = case x of {}


showOrd :: Ordering -> String
showOrd LT = "less than "
showOrd EQ = "equal to "
showOrd GT = "greater than "
