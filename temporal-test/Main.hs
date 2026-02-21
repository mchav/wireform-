{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import Proto.Encode (encodeMessage)
import Proto.Decode (decodeMessage)
import Proto.JSON (protoToJSON, protoFromJSON, renderJson, JsonValue(..))

import Proto.Temporal.Temporal.Api.Common.V1.Message
import Proto.Temporal.Temporal.Api.Enums.V1.Common

main :: IO ()
main = do
  hPutStrLn stderr "=== Temporal API codegen validation ==="

  hPutStrLn stderr "--- Testing DataBlob round-trip ---"
  let blob = defaultDataBlob
        { dataBlobEncodingtype = EncodingTypeProto3
        , dataBlobData = "hello world"
        }
  let encoded = encodeMessage blob
  hPutStrLn stderr ("  Encoded " <> show (BS.length encoded) <> " bytes")
  case decodeMessage encoded of
    Left err -> do
      hPutStrLn stderr ("  FAIL: decode error: " <> show err)
      exitFailure
    Right decoded -> do
      if decoded.dataBlobEncodingtype == blob.dataBlobEncodingtype
         && decoded.dataBlobData == blob.dataBlobData
        then hPutStrLn stderr "  OK: round-trip matches"
        else do
          hPutStrLn stderr "  FAIL: round-trip mismatch"
          exitFailure

  hPutStrLn stderr "--- Testing WorkflowExecution round-trip ---"
  let wfExec = defaultWorkflowExecution
        { workflowExecutionWorkflowid = "my-workflow-123"
        , workflowExecutionRunid = "run-abc-456"
        }
  let wfEncoded = encodeMessage wfExec
  hPutStrLn stderr ("  Encoded " <> show (BS.length wfEncoded) <> " bytes")
  case decodeMessage wfEncoded of
    Left err -> do
      hPutStrLn stderr ("  FAIL: decode error: " <> show err)
      exitFailure
    Right decoded -> do
      if decoded.workflowExecutionWorkflowid == wfExec.workflowExecutionWorkflowid
         && decoded.workflowExecutionRunid == wfExec.workflowExecutionRunid
        then hPutStrLn stderr "  OK: round-trip matches"
        else do
          hPutStrLn stderr "  FAIL: round-trip mismatch"
          exitFailure

  hPutStrLn stderr "--- Testing JSON round-trip ---"
  let json = protoToJSON wfExec
  hPutStrLn stderr ("  JSON: " <> show (renderJson json))
  case protoFromJSON json of
    Left err -> do
      hPutStrLn stderr ("  FAIL: JSON decode error: " <> err)
      exitFailure
    Right (decoded :: WorkflowExecution) -> do
      hPutStrLn stderr "  OK: JSON round-trip succeeded"

  hPutStrLn stderr "--- Testing enum JSON ---"
  let enumJson = protoToJSON EncodingTypeJson
  hPutStrLn stderr ("  Enum JSON: " <> show (renderJson enumJson))
  case enumJson of
    JsonString "ENCODING_TYPE_JSON" -> hPutStrLn stderr "  OK: enum name matches"
    _ -> do
      hPutStrLn stderr "  FAIL: unexpected enum JSON"
      exitFailure

  hPutStrLn stderr "\n=== All tests passed ==="
  exitSuccess
