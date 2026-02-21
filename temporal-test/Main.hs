{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import Proto.Encode (encodeMessage)
import Proto.Decode (decodeMessage, DecodeError)
import Proto.JSON (protoToJSON, renderJson, JsonValue(..))

import Proto.Temporal.Temporal.Api.Common.V1.Message
import Proto.Temporal.Temporal.Api.Enums.V1.Common

main :: IO ()
main = do
  hPutStrLn stderr "=== Temporal API codegen validation ==="
  ok <- sequence
    [ testBinaryRoundTrip
    , testJsonAnnotations
    , testPythonInterop
    ]
  if and ok
    then hPutStrLn stderr "\n=== All tests passed ===" >> exitSuccess
    else hPutStrLn stderr "\n=== SOME TESTS FAILED ===" >> exitFailure

testBinaryRoundTrip :: IO Bool
testBinaryRoundTrip = do
  hPutStrLn stderr "\n--- Binary round-trip tests ---"

  let blob = defaultDataBlob
        { dataBlobEncodingtype = EncodingType'EncodingTypeProto3
        , dataBlobData = "hello world"
        }
  let encoded = encodeMessage blob
  hPutStrLn stderr ("  DataBlob: encoded " <> show (BS.length encoded) <> " bytes")
  case decodeMessage encoded of
    Left err -> do
      hPutStrLn stderr ("  FAIL: DataBlob decode: " <> show err)
      pure False
    Right (decoded :: DataBlob) ->
      if decoded.dataBlobEncodingtype == blob.dataBlobEncodingtype
         && decoded.dataBlobData == blob.dataBlobData
        then do hPutStrLn stderr "  OK: DataBlob round-trip matches"; pure True
        else do hPutStrLn stderr "  FAIL: DataBlob mismatch"; pure False

testJsonAnnotations :: IO Bool
testJsonAnnotations = do
  hPutStrLn stderr "\n--- JSON annotation tests ---"

  let wfExec = defaultWorkflowExecution
        { workflowExecutionWorkflowid = "my-wf"
        , workflowExecutionRunid = "my-run"
        }
  let json = renderJson (protoToJSON wfExec)
  hPutStrLn stderr ("  WorkflowExecution JSON: " <> show json)

  let enumJson = renderJson (protoToJSON EncodingType'EncodingTypeJson)
  hPutStrLn stderr ("  EncodingType JSON: " <> show enumJson)
  case protoToJSON EncodingType'EncodingTypeJson of
    JsonString "ENCODING_TYPE_JSON" -> do
      hPutStrLn stderr "  OK: enum JSON name correct"
      pure True
    other -> do
      hPutStrLn stderr ("  FAIL: unexpected enum JSON: " <> show other)
      pure False

testPythonInterop :: IO Bool
testPythonInterop = do
  hPutStrLn stderr "\n--- Python interop tests ---"

  let hsBlob = defaultDataBlob
        { dataBlobEncodingtype = EncodingType'EncodingTypeJson
        , dataBlobData = "hello from haskell"
        }
  BS.writeFile "/tmp/interop_hs_datablob.bin" (encodeMessage hsBlob)
  hPutStrLn stderr "  Wrote /tmp/interop_hs_datablob.bin"

  let hsWf = defaultWorkflowExecution
        { workflowExecutionWorkflowid = "hs-workflow-999"
        , workflowExecutionRunid = "hs-run-888"
        }
  BS.writeFile "/tmp/interop_hs_wfexec.bin" (encodeMessage hsWf)
  hPutStrLn stderr "  Wrote /tmp/interop_hs_wfexec.bin"

  result <- try (BS.readFile "/tmp/interop_datablob.bin")
  case result of
    Left (_ :: SomeException) -> do
      hPutStrLn stderr "  SKIP: Python data not found (run interop_gen.py first)"
      pure True
    Right pyBlobData -> do
      hPutStrLn stderr ("  Read Python DataBlob: " <> show (BS.length pyBlobData) <> " bytes")
      case decodeMessage pyBlobData of
        Left err -> do
          hPutStrLn stderr ("  FAIL: decode Python DataBlob: " <> show err)
          pure False
        Right (decoded :: DataBlob) -> do
          let ok1 = decoded.dataBlobEncodingtype == EncodingType'EncodingTypeProto3
              ok2 = decoded.dataBlobData == "hello from python"
          if ok1 && ok2
            then hPutStrLn stderr "  OK: Python DataBlob decoded correctly"
            else hPutStrLn stderr ("  FAIL: Python DataBlob mismatch: enc=" <> show decoded.dataBlobEncodingtype <> " data=" <> show decoded.dataBlobData)

          pyWfData <- BS.readFile "/tmp/interop_wfexec.bin"
          hPutStrLn stderr ("  Read Python WorkflowExecution: " <> show (BS.length pyWfData) <> " bytes")
          case decodeMessage pyWfData of
            Left err -> do
              hPutStrLn stderr ("  FAIL: decode Python WF: " <> show err)
              pure False
            Right (wf :: WorkflowExecution) -> do
              let wok1 = wf.workflowExecutionWorkflowid == "wf-python-test-123"
                  wok2 = wf.workflowExecutionRunid == "run-python-abc-456"
              if wok1 && wok2
                then do hPutStrLn stderr "  OK: Python WorkflowExecution decoded correctly"; pure True
                else do hPutStrLn stderr ("  FAIL: Python WF mismatch"); pure False
