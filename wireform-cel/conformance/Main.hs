{-# LANGUAGE OverloadedStrings #-}

-- | Runner for the upstream CEL conformance suite
-- (<https://github.com/google/cel-spec> @tests/simple/testdata/*.textproto@).
--
-- This is opt-in, matching the @TOML_TEST_SUITE@ / @YAML_TEST_SUITE@ pattern
-- used elsewhere in the monorepo: set @CEL_SPEC_DIR@ to a checkout of
-- @cel-spec@ (or directly to its @tests/simple/testdata@ directory) and run
-- the @wireform-cel-conformance@ test suite. Without the variable the runner
-- prints a notice and succeeds.
--
-- The runner parses the textproto @SimpleTestFile@ messages with a small
-- self-contained parser, decodes the expected @cel.expr.Value@ results and
-- variable bindings, evaluates each expression with this library, and reports
-- pass / skip / fail counts per file.
--
-- Tests that exercise features this library does not implement (protocol
-- buffer message values, CEL extension libraries, the optional type checker,
-- unknown tracking) are counted as /skipped/ rather than failed: skips happen
-- either because the expected value cannot be represented (e.g. an
-- @object_value@) or because evaluation reports an "unsupported" error.
module Main (main) where

import Control.Monad (forM, when)
import Data.Char (chr, isAlphaNum, isHexDigit, digitToInt)
import Data.List (foldl', isSuffixOf, sortOn)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

import CEL

----------------------------------------------------------------------
-- Textproto AST + parser
----------------------------------------------------------------------

data PtVal = PStr [Word8] | PAtom String | PMsg Fields
  deriving stock (Show)

type Fields = [(String, PtVal)]

data Tok
  = TLBrace | TRBrace | TLBrack | TRBrack | TLAngle | TRAngle
  | TColon | TComma
  | TStr [Word8] | TAtom String
  deriving stock (Show, Eq)

tokenize :: String -> [Tok]
tokenize [] = []
tokenize (c : cs)
  | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' = tokenize cs
  | c == '#' = tokenize (dropWhile (/= '\n') cs)
  | c == '{' = TLBrace : tokenize cs
  | c == '}' = TRBrace : tokenize cs
  | c == '[' = TLBrack : tokenize cs
  | c == ']' = TRBrack : tokenize cs
  | c == '<' = TLAngle : tokenize cs
  | c == '>' = TRAngle : tokenize cs
  | c == ':' = TColon : tokenize cs
  | c == ',' = TComma : tokenize cs
  | c == '"' || c == '\'' = let (bytes, rest) = scanStr c cs in TStr bytes : tokenize rest
  | isAtomChar c = let (a, rest) = span isAtomChar (c : cs) in TAtom a : tokenize rest
  | otherwise = tokenize cs

isAtomChar :: Char -> Bool
isAtomChar c = isAlphaNum c || c `elem` ("_+-." :: String)

scanStr :: Char -> String -> ([Word8], String)
scanStr q = go []
  where
    go acc [] = (reverse acc, [])
    go acc (c : cs)
      | c == q = (reverse acc, cs)
      | c == '\\' = case esc cs of (bs, rest) -> go (reverse bs ++ acc) rest
      | otherwise = go (reverse (utf8 c) ++ acc) cs

    esc [] = ([], [])
    esc (e : rest) = case e of
      'a' -> ([0x07], rest)
      'b' -> ([0x08], rest)
      'f' -> ([0x0C], rest)
      'n' -> ([0x0A], rest)
      'r' -> ([0x0D], rest)
      't' -> ([0x09], rest)
      'v' -> ([0x0B], rest)
      '\\' -> ([0x5C], rest)
      '\'' -> ([0x27], rest)
      '"' -> ([0x22], rest)
      '?' -> ([0x3F], rest)
      '`' -> ([0x60], rest)
      'x' -> hexN 2 rest
      'X' -> hexN 2 rest
      'u' -> uni 4 rest
      'U' -> uni 8 rest
      _ | e >= '0' && e <= '7' -> octal (e : rest)
        | otherwise -> (utf8 e, rest)

    hexN n s =
      let ds = takeWhile isHexDigit (take n s)
          consumed = length ds
       in ([fromIntegral (foldl' (\a d -> a * 16 + digitToInt d) 0 ds)], drop consumed s)
    octal s =
      let (ds, _) = splitAt 3 s
          octs = takeWhile (\d -> d >= '0' && d <= '7') (take 3 ds)
       in ([fromIntegral (foldl' (\a d -> a * 8 + digitToInt d) 0 octs)], drop (length octs) s)
    uni n s =
      let (ds, r) = splitAt n s
       in if length ds == n && all isHexDigit ds
            then (utf8 (chr (foldl' (\a d -> a * 16 + digitToInt d) 0 ds)), r)
            else (utf8 '?', s)

utf8 :: Char -> [Word8]
utf8 = BS.unpack . TE.encodeUtf8 . T.singleton

-- Parse a sequence of fields up to a closing brace/angle (or end of input).
parseFields :: [Tok] -> (Fields, [Tok])
parseFields = go []
  where
    go acc toks = case toks of
      [] -> (reverse acc, [])
      (TRBrace : rest) -> (reverse acc, rest)
      (TRAngle : rest) -> (reverse acc, rest)
      (TAtom name : rest) -> field name rest acc
      _ -> go acc (drop 1 toks) -- skip stray tokens defensively

    field name toks acc = case toks of
      (TColon : TLBrace : rest) -> sub name rest acc
      (TColon : TLAngle : rest) -> sub name rest acc
      (TColon : TLBrack : rest) -> bracketList name rest acc
      (TColon : rest) -> scalar name rest acc
      (TLBrace : rest) -> sub name rest acc
      (TLAngle : rest) -> sub name rest acc
      _ -> go acc toks

    sub name rest acc =
      let (fs, rest') = parseFields rest
       in go ((name, PMsg fs) : acc) rest'

    scalar name rest acc = case rest of
      (TStr bs : more) ->
        let (bs', more') = mergeStrs bs more
         in go ((name, PStr bs') : acc) more'
      (TAtom a : more) -> go ((name, PAtom a) : acc) more
      _ -> go acc rest

    bracketList name toks acc = case toks of
      (TRBrack : rest) -> go acc rest
      _ ->
        let (item, rest) = parseItem toks
            acc' = (name, item) : acc
         in case rest of
              (TComma : rest') -> bracketList name rest' acc'
              (TRBrack : rest') -> go acc' rest'
              _ -> go acc' rest

    parseItem toks = case toks of
      (TLBrace : rest) -> let (fs, r) = parseFields rest in (PMsg fs, r)
      (TLAngle : rest) -> let (fs, r) = parseFields rest in (PMsg fs, r)
      (TStr bs : more) -> let (bs', m) = mergeStrs bs more in (PStr bs', m)
      (TAtom a : more) -> (PAtom a, more)
      _ -> (PAtom "", drop 1 toks)

    mergeStrs bs (TStr more : rest) = mergeStrs (bs ++ more) rest
    mergeStrs bs rest = (bs, rest)

parseFile :: String -> Fields
parseFile = fst . parseFields . tokenize

----------------------------------------------------------------------
-- Field helpers
----------------------------------------------------------------------

field1 :: String -> Fields -> Maybe PtVal
field1 n fs = lookup n fs

fieldAll :: String -> Fields -> [PtVal]
fieldAll n fs = [v | (k, v) <- fs, k == n]

asText :: PtVal -> Maybe Text
asText (PStr bs) = Just (TE.decodeUtf8With lenient (BS.pack bs))
  where lenient _ _ = Just '\xFFFD'
asText (PAtom a) = Just (T.pack a)
asText _ = Nothing

asMsg :: PtVal -> Maybe Fields
asMsg (PMsg fs) = Just fs
asMsg _ = Nothing

----------------------------------------------------------------------
-- Decoding expected values / bindings
----------------------------------------------------------------------

-- Decode a cel.expr.Value message. 'Left' is a skip reason.
decodeValue :: Fields -> Either String Value
decodeValue fs = case mapMaybe tryField fs of
  (v : _) -> v
  [] -> Left "empty Value message"
  where
    tryField (n, v) = case n of
      "null_value" -> Just (Right VNull)
      "bool_value" -> Just (decAtom v (\a -> VBool (a == "true")))
      "int64_value" -> Just (decInt v (VInt . fromInteger))
      "uint64_value" -> Just (decInt v (VUInt . fromInteger))
      "double_value" -> Just (decDouble v)
      "string_value" -> Just (decStr v VString)
      "bytes_value" -> Just (decBytes v)
      "type_value" -> Just (decType v)
      "enum_value" -> Just (decEnum v)
      "list_value" -> Just (decList v)
      "map_value" -> Just (decMap v)
      "object_value" -> Just (Left "object_value (protobuf message) unsupported")
      _ -> Nothing

    decAtom (PAtom a) f = Right (f a)
    decAtom _ _ = Left "expected atom"

    decInt (PAtom a) f = case reads a of
      [(n, "")] -> Right (f (n :: Integer))
      _ -> Left ("bad integer: " <> a)
    decInt _ _ = Left "expected integer atom"

    decDouble (PAtom a) = case a of
      "inf" -> Right (VDouble (1 / 0))
      "-inf" -> Right (VDouble (-1 / 0))
      "nan" -> Right (VDouble (0 / 0))
      _ -> case reads a of
        [(d, "")] -> Right (VDouble d)
        _ -> Left ("bad double: " <> a)
    decDouble _ = Left "expected double atom"

    decStr (PStr bs) f = Right (f (TE.decodeUtf8With (\_ _ -> Just '\xFFFD') (BS.pack bs)))
    decStr _ _ = Left "expected string"

    decBytes (PStr bs) = Right (VBytes (BS.pack bs))
    decBytes _ = Left "expected bytes"

    decType v = case asText v of
      Just t -> Right (VType (typeFromName t))
      Nothing -> Left "expected type name"

    decEnum (PMsg sub) = case field1 "value" sub of
      Just (PAtom a) | [(n, "")] <- reads a -> Right (VInt (fromInteger (n :: Integer)))
      Nothing -> Right (VInt 0) -- unset enum value defaults to 0
      _ -> Left "bad enum value"
    decEnum _ = Left "expected enum message"

    decList (PMsg sub) =
      let items = fieldAll "values" sub
       in case mapM (\pv -> asMsg pv >>= Just . decodeValue) items of
            Just rs -> VList . V.fromList <$> sequence rs
            Nothing -> Left "bad list element"
    decList _ = Left "expected list message"

    decMap (PMsg sub) =
      let entries = fieldAll "entries" sub
       in do
            kvs <- mapM decEntry entries
            case celMap kvs of
              Left e -> Left (T.unpack e)
              Right m -> Right (VMap m)
    decMap _ = Left "expected map message"

    decEntry (PMsg e) = do
      kF <- maybe (Left "entry without key") Right (field1 "key" e >>= asMsg)
      vF <- maybe (Left "entry without value") Right (field1 "value" e >>= asMsg)
      k <- decodeValue kF
      val <- decodeValue vF
      Right (k, val)
    decEntry _ = Left "bad map entry"

typeFromName :: Text -> CelType
typeFromName t = case t of
  "bool" -> TyBool
  "int" -> TyInt
  "uint" -> TyUInt
  "double" -> TyDouble
  "string" -> TyString
  "bytes" -> TyBytes
  "list" -> TyList
  "map" -> TyMap
  "null_type" -> TyNull
  "type" -> TyType
  "google.protobuf.Timestamp" -> TyTimestamp
  "google.protobuf.Duration" -> TyDuration
  _ -> TyMessage t

-- A binding's value is an ExprValue { value | error | unknown }.
decodeBinding :: Fields -> Either String (Text, Value)
decodeBinding fs = do
  keyV <- maybe (Left "binding without key") Right (field1 "key" fs)
  key <- maybe (Left "binding key not text") Right (asText keyV)
  exprValue <- maybe (Left "binding without value") Right (field1 "value" fs >>= asMsg)
  case field1 "value" exprValue >>= asMsg of
    Just valueMsg -> do
      v <- decodeValue valueMsg
      Right (key, v)
    Nothing -> Left "binding is not a plain value (error/unknown)"

----------------------------------------------------------------------
-- Running a single test
----------------------------------------------------------------------

data Outcome = Pass | Skip String | Fail String

runTest :: Fields -> Outcome
runTest t =
  case field1 "expr" t >>= asText of
    Nothing -> Skip "no expr"
    Just expr ->
      let container = maybe "" id (field1 "container" t >>= asText)
          bindingResults = map decodeBinding (mapMaybe asMsg (fieldAll "bindings" t))
       in case sequence bindingResults of
            Left reason -> Skip reason
            Right binds ->
              let env = withContainer container (bindAll binds emptyEnv)
                  result = run env expr
                  hasError = not (null (fieldAll "eval_error" t))
               in case (field1 "value" t, hasError) of
                    (Just valMsg, _) -> case asMsg valMsg of
                      Nothing -> Skip "value not a message"
                      Just vm -> case decodeValue vm of
                        Left reason -> Skip ("expected value: " <> reason)
                        Right expected -> case result of
                          Right got
                            | structEq got expected -> Pass
                            | otherwise -> Fail ("want " <> show expected <> ", got " <> show got <> "  [" <> T.unpack expr <> "]")
                          Left e
                            | errKind e == ErrUnsupported -> Skip ("unsupported: " <> T.unpack (errMsg e))
                            | otherwise -> Fail ("want " <> show expected <> ", got error " <> show e <> "  [" <> T.unpack expr <> "]")
                    (Nothing, True) -> case result of
                      Left _ -> Pass
                      Right got -> Fail ("want error, got " <> show got <> "  [" <> T.unpack expr <> "]")
                    (Nothing, False) -> Skip "no expected value or error"

structEq :: Value -> Value -> Bool
structEq a b = case (a, b) of
  (VNull, VNull) -> True
  (VBool x, VBool y) -> x == y
  (VInt x, VInt y) -> x == y
  (VUInt x, VUInt y) -> x == y
  (VDouble x, VDouble y) -> (isNaN x && isNaN y) || x == y
  (VString x, VString y) -> x == y
  (VBytes x, VBytes y) -> x == y
  (VType x, VType y) -> typeNameText x == typeNameText y
  (VDuration x, VDuration y) -> x == y
  (VTimestamp x, VTimestamp y) -> x == y
  (VList x, VList y) ->
    V.length x == V.length y && and (zipWith structEq (V.toList x) (V.toList y))
  (VMap x, VMap y) ->
    let ex = celMapEntries x
        ey = celMapEntries y
     in length ex == length ey
          && all (\(k, v) -> maybe False (structEq v) (celMapLookup k y)) ex
  _ -> False

----------------------------------------------------------------------
-- Collecting tests from a file
----------------------------------------------------------------------

collectTests :: Fields -> [Fields]
collectTests file =
  let sections = mapMaybe asMsg (fieldAll "section" file)
      sectionTests = concatMap (\s -> mapMaybe asMsg (fieldAll "test" s)) sections
      topTests = mapMaybe asMsg (fieldAll "test" file)
   in topTests ++ sectionTests

-- Files that exercise CEL extension libraries or checker-only behavior that
-- are out of scope for the core language definition this package targets.
excludedFiles :: [String]
excludedFiles =
  [ "bindings_ext.textproto"
  , "block_ext.textproto"
  , "encoders_ext.textproto"
  , "math_ext.textproto"
  , "network_ext.textproto"
  , "string_ext.textproto"
  , "proto2_ext.textproto"
  , "optionals.textproto"
  , "type_deduction.textproto"
  , "unknowns.textproto"
  , "enums.textproto" -- requires protobuf enum declarations
  ]

----------------------------------------------------------------------
-- Main
----------------------------------------------------------------------

main :: IO ()
main = do
  mDir <- lookupEnv "CEL_SPEC_DIR"
  case mDir of
    Nothing -> do
      putStrLn "CEL_SPEC_DIR not set; skipping upstream conformance suite."
      putStrLn "  Set CEL_SPEC_DIR to a cel-spec checkout to run it."
      exitSuccess
    Just dir -> do
      testdata <- resolveTestdata dir
      entries <- listDirectory testdata
      let files =
            sortOn id
              [ f
              | f <- entries
              , ".textproto" `isSuffixOf` f
              , f `notElem` excludedFiles
              ]
      perFile <- forM files $ \f -> do
        contents <- readFile (testdata </> f)
        let tests = collectTests (parseFile contents)
            outcomes = map runTest tests
            p = length [() | Pass <- outcomes]
            skips = [r | Skip r <- outcomes]
            fs = [r | Fail r <- outcomes]
        putStrLn $
          padRight 26 f
            <> "  pass=" <> padLeft 4 (show p)
            <> "  skip=" <> padLeft 4 (show (length skips))
            <> "  fail=" <> padLeft 4 (show (length fs))
        mapM_ (\r -> putStrLn ("    FAIL: " <> r)) (take 12 fs)
        when (length fs > 12) $
          putStrLn ("    ... and " <> show (length fs - 12) <> " more failures")
        pure (p, skips, fs)
      let totP = sum [a | (a, _, _) <- perFile]
          allSkips = concat [b | (_, b, _) <- perFile]
          totF = sum [length c | (_, _, c) <- perFile]
      putStrLn "----------------------------------------------------------------"
      putStrLn "Skip reasons (feature not implemented by this library):"
      mapM_
        (\(reason, n) -> putStrLn ("  " <> padLeft 4 (show n) <> "  " <> reason))
        (tally (map skipCategory allSkips))
      putStrLn "----------------------------------------------------------------"
      putStrLn $
        "TOTAL  pass=" <> show totP <> "  skip=" <> show (length allSkips) <> "  fail=" <> show totF
      if totF == 0 then exitSuccess else exitFailure

-- Group a skip reason into a coarse category (everything up to the first ':').
skipCategory :: String -> String
skipCategory = takeWhile (/= ':')

tally :: [String] -> [(String, Int)]
tally xs = sortOn (negate . snd) (foldr bump [] xs)
  where
    bump x acc = case lookup x acc of
      Just _ -> map (\(k, n) -> if k == x then (k, n + 1) else (k, n)) acc
      Nothing -> (x, 1) : acc

resolveTestdata :: FilePath -> IO FilePath
resolveTestdata dir = do
  let nested = dir </> "tests" </> "simple" </> "testdata"
  haveNested <- doesDirectoryExist nested
  pure (if haveNested then nested else dir)

padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' ++ s
