module Main (main) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Network.GRPC.Server.Otel (noopTracer)
import Proto.GRPC (grpcFrame, grpcUnframe, grpcFrameMany, grpcUnframeMany)

main :: IO ()
main = do
  testNoopTracer
  testGrpcFramingSingle
  testGrpcFramingEmpty
  testGrpcFramingMany
  testGrpcFramingLarge
  putStrLn "All wireform-grpc tests passed"

testNoopTracer :: IO ()
testNoopTracer = do
  let _tracer = noopTracer
  putStrLn "noopTracer: OK"

testGrpcFramingSingle :: IO ()
testGrpcFramingSingle = do
  let msg = BS.Char8.pack "hello world"
      framed = grpcFrame msg
  case grpcUnframe framed of
    Right decoded
      | decoded == msg -> putStrLn "gRPC framing single: OK"
      | otherwise -> error $ "gRPC framing mismatch: " ++ show decoded
    Left err -> error $ "gRPC framing failed: " ++ err

testGrpcFramingEmpty :: IO ()
testGrpcFramingEmpty = do
  let msg = BS.empty
      framed = grpcFrame msg
  case grpcUnframe framed of
    Right decoded
      | decoded == msg -> putStrLn "gRPC framing empty: OK"
      | otherwise -> error $ "gRPC framing empty mismatch: " ++ show decoded
    Left err -> error $ "gRPC framing empty failed: " ++ err

testGrpcFramingMany :: IO ()
testGrpcFramingMany = do
  let msgs = map BS.Char8.pack ["alpha", "beta", "gamma"]
      framed = grpcFrameMany msgs
  case grpcUnframeMany framed of
    Right decoded
      | decoded == msgs -> putStrLn "gRPC framing many: OK"
      | otherwise -> error $ "gRPC framing many mismatch: " ++ show decoded
    Left err -> error $ "gRPC framing many failed: " ++ err

testGrpcFramingLarge :: IO ()
testGrpcFramingLarge = do
  let msg = BS.replicate 100000 0x42
      framed = grpcFrame msg
  case grpcUnframe framed of
    Right decoded
      | decoded == msg -> putStrLn "gRPC framing large: OK"
      | otherwise -> error "gRPC framing large mismatch"
    Left err -> error $ "gRPC framing large failed: " ++ err
