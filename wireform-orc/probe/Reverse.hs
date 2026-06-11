{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Reverse-direction ORC interop probe.

Given a directory of ORC files (typically produced by
pyarrow.orc in the companion shell driver), try to decode
each one with wireform-orc's facade and print one line per
file: 'OK <name>' or 'FAIL <name>: <error>'. Exit code is
the number of failures (capped at 99) so CI can flag
regressions.
-}
module Main (main) where

import Arrow.Types qualified as AT
import Control.Exception (SomeException, catch)
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import ORC qualified
import ORC.Arrow qualified as OArrow
import ORC.Read qualified as OR
import ORC.Types qualified as OT
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeExtension, (</>))


main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> do
      files <- filter ((== ".orc") . takeExtension) <$> listDirectory dir
      results <- mapM (probe dir) files
      mapM_ printResult results
      let !nFail = length [() | (_, Left _) <- results]
      putStrLn $ replicate 50 '-'
      putStrLn $
        show (length results)
          ++ " files, "
          ++ show (length results - nFail)
          ++ " ok, "
          ++ show nFail
          ++ " failed"
      if nFail == 0 then pure () else exitWith (ExitFailure (min 99 nFail))
    _ -> do
      putStrLn "usage: wireform-orc-reverse-probe <dir>"
      exitFailure


printResult :: (FilePath, Either String String) -> IO ()
printResult (n, Left e) = putStrLn $ "  FAIL " ++ n ++ ": " ++ e
printResult (n, Right s) = putStrLn $ "  OK   " ++ n ++ " " ++ s


probe :: FilePath -> FilePath -> IO (FilePath, Either String String)
probe dir name = do
  let !path = dir </> name
  bs <- BS.readFile path
  res <- decodeOnce bs `catch` \e -> pure (Left (show (e :: SomeException)))
  pure (name, res)


-- | Try a full footer + every-stripe decode.
decodeOnce :: BS.ByteString -> IO (Either String String)
decodeOnce bs = case ORC.decodeORC bs of
  Left e -> pure $ Left ("decode footer: " ++ e)
  Right footer -> do
    let !nStripes = V.length (OT.orcStripes footer)
        !nCols = V.length (OT.orcTypes footer)
    case OR.loadORCFile bs of
      Left e -> pure $ Left ("loadORCFile: " ++ e)
      Right ofile -> do
        -- Run the bridge stripe-by-stripe; treat the first
        -- stripe failure as the file's failure.
        let !sch = inferArrowSchemaFromORC footer
            results =
              [ (i, OArrow.orcStripeToArrow sch bs footer i)
              | i <- [0 .. nStripes - 1]
              ]
            firstFail =
              [ "stripe " ++ show i ++ ": " ++ show e
              | (i, Left e) <- results
              ]
            _ = ofile
        pure $ case firstFail of
          [] ->
            Right $
              "(" ++ show nStripes ++ " stripes x " ++ show nCols ++ " types)"
          (e : _) -> Left e


{- | Build a top-level Arrow schema from an ORC footer's type
table. Walks @orcTypes@ (column 0 is the synthetic root
struct, its fieldNames + subtypes name the leaves) and
emits one 'AT.Field' per top-level child.
-}
inferArrowSchemaFromORC :: OT.ORCFooter -> AT.Schema
inferArrowSchemaFromORC footer =
  let !types = OT.orcTypes footer
      !root = if V.null types then Nothing else Just (V.unsafeIndex types 0)
      mkField (nm, cid) =
        let !leafTy =
              if cid >= 0 && cid < V.length types
                then V.unsafeIndex types cid
                else V.unsafeIndex types 0
        in AT.defaultLeafField nm False (orcKindToArrow (OT.otKind leafTy))
      children = case root of
        Just r ->
          V.zipWith
            (\nm cid -> mkField (nm, fromIntegral cid))
            (OT.otFieldNames r)
            (OT.otSubtypes r)
        Nothing -> V.empty
  in AT.defaultSchema children


{- | Coarse mapping; the bridge does the precise version.
Anything unsupported maps to Binary so the read at least
attempts something sensible.
-}
orcKindToArrow :: OT.TypeKind -> AT.ArrowType
orcKindToArrow = \case
  OT.TKBoolean -> AT.ABool
  OT.TKByte -> AT.AInt 8 True
  OT.TKShort -> AT.AInt 16 True
  OT.TKInt -> AT.AInt 32 True
  OT.TKLong -> AT.AInt 64 True
  OT.TKFloat -> AT.AFloatingPoint AT.Single
  OT.TKDouble -> AT.AFloatingPoint AT.DoublePrecision
  OT.TKString -> AT.AUtf8
  OT.TKVarchar -> AT.AUtf8
  OT.TKChar -> AT.AUtf8
  OT.TKBinary -> AT.ABinary
  _ -> AT.ABinary
