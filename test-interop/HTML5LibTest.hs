{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (forM)
import Data.IORef
import Data.List (isSuffixOf, sort, isPrefixOf, intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr)
import Test.Tasty
import Test.Tasty.HUnit

import HTML.Parse (parseHTML)
import HTML.Value

-- ---------------------------------------------------------------------------
-- Test-case record
-- ---------------------------------------------------------------------------

data TestCase = TestCase
  { tcData     :: !String
  , tcDocument :: ![String]
  , tcFragment :: !(Maybe String)
  , tcIndex    :: !Int
  } deriving (Show)

data TestResult = Pass | Fail !String | Skip !String
  deriving (Show)

-- ---------------------------------------------------------------------------
-- .dat file parser
-- ---------------------------------------------------------------------------

parseTestFile :: String -> [TestCase]
parseTestFile contents =
  let ls = lines contents
      raw = parseTests ls
  in  zipWith (\i tc -> tc { tcIndex = i }) [1..] raw

parseTests :: [String] -> [TestCase]
parseTests [] = []
parseTests ls =
  case dropWhile (/= "#data") ls of
    [] -> []
    ("#data":rest) ->
      let (tc, remaining) = parseSingleTest rest
      in  tc : parseTests remaining
    _ -> []

parseSingleTest :: [String] -> (TestCase, [String])
parseSingleTest ls =
  let (dataLines, afterData)     = break isSectionHeader ls
      sections                   = collectSections afterData
      docLines                   = lookupSection "#document" sections
      fragLine                   = case lookupSection "#document-fragment" sections of
                                     []    -> Nothing
                                     (x:_) -> Just x
      remaining                  = findNextTest afterData
  in  ( TestCase
          { tcData     = unlines' dataLines
          , tcDocument = docLines
          , tcFragment = fragLine
          , tcIndex    = 0
          }
      , remaining
      )

isSectionHeader :: String -> Bool
isSectionHeader [] = False
isSectionHeader (c:_) = c == '#'

collectSections :: [String] -> [(String, [String])]
collectSections [] = []
collectSections (h:rest)
  | isSectionHeader h =
      let (body, after) = break isSectionHeader rest
      in  (h, body) : collectSections after
  | otherwise = collectSections rest

lookupSection :: String -> [(String, [String])] -> [String]
lookupSection key secs = case lookup key secs of
  Nothing -> []
  Just xs -> xs

findNextTest :: [String] -> [String]
findNextTest [] = []
findNextTest ls =
  let sections = collectSections ls
      allLines = concatMap (\(h, body) -> h : body) sections
      afterDoc = dropWhile (/= "#data") (drop 1 (dropWhile (/= "#document") allLines))
  in  case dropWhile isBlankOrNonData afterDoc of
        found@("#data":_) -> found
        _ -> skipToNextData ls
  where
    isBlankOrNonData "#data" = False
    isBlankOrNonData s = null s || all (== ' ') s

skipToNextData :: [String] -> [String]
skipToNextData [] = []
skipToNextData ls =
  case break (== "#data") ls of
    (_, []) -> []
    (_, found) ->
      if any (== "#document") (takeWhile (/= "#data") ls)
        then found
        else case found of
               ("#data":rest) ->
                 case break (== "#data") rest of
                   (_, found2@("#data":_)) -> found2
                   _ -> []
               _ -> found

unlines' :: [String] -> String
unlines' [] = ""
unlines' xs = intercalate "\n" xs

-- ---------------------------------------------------------------------------
-- Expected-tree parser
-- ---------------------------------------------------------------------------

data ExpNode
  = ExpElement  !Text ![ExpAttr] ![ExpNode]
  | ExpText     !Text
  | ExpComment  !Text
  | ExpDoctype  !Text
  | ExpTemplate ![ExpNode]
  deriving (Show, Eq)

data ExpAttr = ExpAttr !Text !Text
  deriving (Show, Eq, Ord)

parseExpectedTree :: [String] -> [ExpNode]
parseExpectedTree docLines =
  let joined  = joinContinuationLines docLines
      parsed  = map parseLine joined
      stripped = [(d, n) | Just (d, n) <- parsed]
  in  buildTree stripped

-- Lines starting with "| " introduce new nodes; other lines are
-- continuations of the previous node (multi-line text/comments).
joinContinuationLines :: [String] -> [String]
joinContinuationLines [] = []
joinContinuationLines (l:ls)
  | "| " `isPrefixOf` l =
      let (cont, rest) = span (not . isDocLine) ls
      in  if null cont
            then l : joinContinuationLines rest
            else (l ++ "\n" ++ unlines' cont) : joinContinuationLines rest
  | otherwise = joinContinuationLines ls
  where
    isDocLine s = "| " `isPrefixOf` s

data RawLine
  = RLElement !Text
  | RLAttr !Text !Text
  | RLText !Text
  | RLComment !Text
  | RLDoctype !Text
  | RLTemplateContent
  deriving (Show)

parseLine :: String -> Maybe (Int, RawLine)
parseLine s
  | "| " `isPrefixOf` s =
      let afterBar = drop 2 s
          (spaces, content) = span (== ' ') afterBar
          depth = length spaces `div` 2
      in  if null content then Nothing else Just (depth, parseContent content)
  | otherwise = Nothing

parseContent :: String -> RawLine
parseContent s
  | "<!DOCTYPE" `isPrefixOf` s =
      let rest = drop 9 s
          name = T.strip $ T.pack $ takeWhile (/= '>') $
                   dropWhile (== ' ') rest
      in  RLDoctype name
  | "<!-- " `isPrefixOf` s && " -->" `isSuffixOf` s =
      RLComment (T.pack (drop 5 (take (length s - 4) s)))
  | "<!-- " `isPrefixOf` s && "-->" `isSuffixOf` s =
      RLComment (T.pack (drop 5 (take (length s - 3) s)))
  | "<!--" `isPrefixOf` s && " -->" `isSuffixOf` s =
      RLComment (T.pack (drop 4 (take (length s - 4) s)))
  | "<!--" `isPrefixOf` s && "-->" `isSuffixOf` s =
      RLComment (T.pack (drop 4 (take (length s - 3) s)))
  | "<!-- " `isPrefixOf` s =
      RLComment (T.pack (drop 5 s))
  | "<!--" `isPrefixOf` s =
      RLComment (T.pack (drop 4 s))
  | not (null s) && safeHead s == '<' =
      let tag = takeWhile (\c -> c /= '>' && c /= ' ') (tail s)
      in  RLElement (T.pack tag)
  | not (null s) && safeHead s == '"' =
      let inner = drop 1 s
          stripped = if not (null inner) && last inner == '"'
                     then init inner
                     else inner
      in  RLText (T.pack stripped)
  | s == "content" =
      RLTemplateContent
  | otherwise =
      case break (== '=') s of
        (name, '=':'"':rest) ->
          let val = if not (null rest) && last rest == '"'
                    then init rest
                    else rest
          in  RLAttr (T.pack name) (T.pack val)
        (name, '=':rest) ->
          RLAttr (T.pack name) (T.pack rest)
        _ -> RLText (T.pack s)

safeHead :: String -> Char
safeHead [] = '\0'
safeHead (c:_) = c

buildTree :: [(Int, RawLine)] -> [ExpNode]
buildTree [] = []
buildTree items = go items
  where
    go [] = []
    go ((d, rl):rest) =
      case rl of
        RLElement tag ->
          let (attrs, afterAttrs) = spanAttrs (d + 1) rest
              (children, afterChildren) = spanChildren (d + 1) afterAttrs
          in  ExpElement tag attrs children : go afterChildren
        RLText t ->
          ExpText t : go rest
        RLComment t ->
          ExpComment t : go rest
        RLDoctype name ->
          ExpDoctype name : go rest
        RLTemplateContent ->
          let (children, afterChildren) = spanChildren (d + 1) rest
          in  ExpTemplate children : go afterChildren
        RLAttr _ _ ->
          go rest

    spanAttrs _ [] = ([], [])
    spanAttrs targetD ((d, RLAttr n v):rest)
      | d == targetD = let (more, rem') = spanAttrs targetD rest
                       in  (ExpAttr n v : more, rem')
    spanAttrs _ xs = ([], xs)

    spanChildren _ [] = ([], [])
    spanChildren targetD xs@((d, _):_)
      | d >= targetD =
          let (child, rest') = takeOne xs
              (more, rem')   = spanChildren targetD rest'
          in  (child ++ more, rem')
    spanChildren _ xs = ([], xs)

    takeOne [] = ([], [])
    takeOne ((d, rl):rest) =
      case rl of
        RLElement tag ->
          let (attrs, afterAttrs) = spanAttrs (d + 1) rest
              (children, afterChildren) = spanChildren (d + 1) afterAttrs
          in  ([ExpElement tag attrs children], afterChildren)
        RLText t -> ([ExpText t], rest)
        RLComment t -> ([ExpComment t], rest)
        RLDoctype name -> ([ExpDoctype name], rest)
        RLTemplateContent ->
          let (children, afterChildren) = spanChildren (d + 1) rest
          in  ([ExpTemplate children], afterChildren)
        RLAttr _ _ -> ([], rest)

-- ---------------------------------------------------------------------------
-- Convert parsed HTMLDocument to [ExpNode]
-- ---------------------------------------------------------------------------

docToExpNodes :: HTMLDocument -> [ExpNode]
docToExpNodes (HTMLDocument mdt root) =
  let dtNodes = case mdt of
        Nothing -> []
        Just (Doctype mname _ _) ->
          [ExpDoctype (maybe "" id mname)]
  in  dtNodes ++ [nodeToExp root]

nodeToExp :: HTMLNode -> ExpNode
nodeToExp (HTMLElement tag attrs children) =
  ExpElement tag
    (map (\(HTMLAttribute n v) -> ExpAttr n v) (V.toList attrs))
    (map nodeToExp (V.toList children))
nodeToExp (HTMLText t) = ExpText t
nodeToExp (HTMLComment t) = ExpComment t
nodeToExp (HTMLDoctype t) = ExpDoctype t

-- ---------------------------------------------------------------------------
-- Tree comparison
-- ---------------------------------------------------------------------------

compareTrees :: [ExpNode] -> [ExpNode] -> Bool
compareTrees actual expected =
  compareNodeLists (normalize actual) (normalize expected)

normalize :: [ExpNode] -> [ExpNode]
normalize = concatMap normNode
  where
    normNode (ExpElement tag attrs children) =
      [ExpElement (T.toLower tag) (sort attrs) (normalize children)]
    normNode (ExpText t)
      | isWhitespaceOnly t = []
      | otherwise = [ExpText t]
    normNode (ExpComment t) = [ExpComment t]
    normNode (ExpDoctype t) = [ExpDoctype (T.toLower t)]
    normNode (ExpTemplate children) = normalize children

isWhitespaceOnly :: Text -> Bool
isWhitespaceOnly = T.all (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')

compareNodeLists :: [ExpNode] -> [ExpNode] -> Bool
compareNodeLists [] [] = True
compareNodeLists (a:as') (e:es) = compareNode a e && compareNodeLists as' es
compareNodeLists _ _ = False

compareNode :: ExpNode -> ExpNode -> Bool
compareNode (ExpElement aTag aAttrs aChildren) (ExpElement eTag eAttrs eChildren) =
  aTag == eTag
  && aAttrs == eAttrs
  && compareNodeLists aChildren eChildren
compareNode (ExpText aT) (ExpText eT) = aT == eT
compareNode (ExpComment aT) (ExpComment eT) = aT == eT
compareNode (ExpDoctype aName) (ExpDoctype eName) = aName == eName
compareNode _ _ = False

-- ---------------------------------------------------------------------------
-- Render trees for diagnostics
-- ---------------------------------------------------------------------------

showExpNodes :: [ExpNode] -> String
showExpNodes nodes = unlines (concatMap (showExp 0) nodes)

showExp :: Int -> ExpNode -> [String]
showExp d (ExpElement tag attrs children) =
  (indStr d ++ "<" ++ T.unpack tag ++ ">") :
  concatMap (\(ExpAttr n v) -> [indStr (d+1) ++ T.unpack n ++ "=\"" ++ T.unpack v ++ "\""]) attrs ++
  concatMap (showExp (d+1)) children
showExp d (ExpText t) = [indStr d ++ show (T.unpack t)]
showExp d (ExpComment t) = [indStr d ++ "<!-- " ++ T.unpack t ++ " -->"]
showExp d (ExpDoctype name) = [indStr d ++ "<!DOCTYPE " ++ T.unpack name ++ ">"]
showExp d (ExpTemplate children) =
  (indStr d ++ "content") : concatMap (showExp (d+1)) children

indStr :: Int -> String
indStr n = replicate (n * 2) ' '

-- ---------------------------------------------------------------------------
-- Run a single test
-- ---------------------------------------------------------------------------

runTest :: TestCase -> TestResult
runTest tc
  | Just _ <- tcFragment tc = Skip "fragment parsing"
  | null (tcDocument tc)    = Skip "no #document section"
  | otherwise =
      let input    = TE.encodeUtf8 (T.pack (tcData tc))
          expected = parseExpectedTree (tcDocument tc)
          doc      = parseHTML input
          actual   = docToExpNodes doc
      in  if compareTrees actual expected
            then Pass
            else Fail (treeDiff actual expected)

treeDiff :: [ExpNode] -> [ExpNode] -> String
treeDiff actual expected =
  "Expected:\n" ++ showExpNodes (normalize expected)
  ++ "Actual:\n" ++ showExpNodes (normalize actual)

-- ---------------------------------------------------------------------------
-- Main: Tasty-based with summary reporting
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  let testDir = "test-data/html5lib/tree-construction"
  allFiles <- sort . filter (".dat" `isSuffixOf`) <$> listDirectory testDir

  passRef  <- newIORef (0 :: Int)
  failRef  <- newIORef (0 :: Int)
  skipRef  <- newIORef (0 :: Int)
  totalRef <- newIORef (0 :: Int)
  fileTests <- forM allFiles $ \file -> do
    content <- readFile (testDir </> file)
    let cases = parseTestFile content
    pure (file, cases)

  let mkFileGroup refs (file, cases) =
        testGroup file $
          map (mkTestCase refs file) cases

      mkTestCase (pRef, fRef, sRef, tRef) _file tc =
        testCase ("#" ++ show (tcIndex tc) ++ ": " ++ ellipsis 50 (tcData tc)) $ do
          modifyIORef' tRef (+1)
          result <- (evaluate (runTest tc))
                    `catch` (\(e :: SomeException) ->
                               pure (Fail ("Exception: " ++ show e)))
          case result of
            Pass   -> modifyIORef' pRef (+1)
            Skip _ -> modifyIORef' sRef (+1)
            Fail _ -> modifyIORef' fRef (+1)

      ellipsis n s
        | length s <= n = s
        | otherwise     = take n s ++ "..."

      refs = (passRef, failRef, skipRef, totalRef)

      summaryTest = testCase "SUMMARY" $ do
        p <- readIORef passRef
        f <- readIORef failRef
        s <- readIORef skipRef
        t <- readIORef totalRef
        let run = p + f
            report l = hPutStrLn stderr l
        report ""
        report "===== html5lib tree-construction results ====="
        report $ "Total test cases:   " ++ show t
        report $ "Skipped (fragment): " ++ show s
        report $ "Executed:           " ++ show run
        report $ "Passed:             " ++ show p
        report $ "Failed:             " ++ show f
        report $ "Pass rate:          " ++ showPercent p run
        hFlush stderr

      tree = testGroup "html5lib tree-construction"
        (map (mkFileGroup refs) fileTests ++ [summaryTest])

  defaultMain tree

showPercent :: Int -> Int -> String
showPercent _ 0 = "N/A"
showPercent p t =
  let pct = (fromIntegral p * 100 :: Double) / fromIntegral t
  in  show (round pct :: Int) ++ "% (" ++ show p ++ "/" ++ show t ++ ")"
