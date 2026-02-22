{-# LANGUAGE OverloadedStrings #-}
-- | Example: parsing a .proto file and generating Haskell code.
--
-- Demonstrates the full pipeline from proto IDL text to generated
-- Haskell module source.
--
-- Run with: cabal run example-codegen
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

import Proto.AST
import Proto.Parser
import Proto.CodeGen
import Proto.Annotations

sampleProto :: Text
sampleProto = T.unlines
  [ "syntax = \"proto3\";"
  , "package myapp.users;"
  , ""
  , "option (custom_validation) = true;"
  , ""
  , "// User account message"
  , "message User {"
  , "  string id = 1;"
  , "  string display_name = 2;"
  , "  string email = 3;"
  , "  int64 created_at = 4;"
  , "  bool is_admin = 5;"
  , "  repeated string roles = 6;"
  , ""
  , "  // Address submessage"
  , "  message Address {"
  , "    string street = 1;"
  , "    string city = 2;"
  , "    string country = 3;"
  , "    string postal_code = 4;"
  , "  }"
  , ""
  , "  Address address = 7;"
  , ""
  , "  oneof contact {"
  , "    string phone = 8;"
  , "    string mobile = 9;"
  , "  }"
  , "}"
  , ""
  , "enum AccountStatus {"
  , "  ACCOUNT_STATUS_UNSPECIFIED = 0;"
  , "  ACCOUNT_STATUS_ACTIVE = 1;"
  , "  ACCOUNT_STATUS_SUSPENDED = 2;"
  , "  ACCOUNT_STATUS_DELETED = 3;"
  , "}"
  , ""
  , "message GetUserRequest {"
  , "  string user_id = 1;"
  , "}"
  , ""
  , "message GetUserResponse {"
  , "  User user = 1;"
  , "  AccountStatus status = 2;"
  , "}"
  , ""
  , "service UserService {"
  , "  rpc GetUser (GetUserRequest) returns (GetUserResponse);"
  , "  rpc ListUsers (ListUsersRequest) returns (stream User);"
  , "}"
  , ""
  , "message ListUsersRequest {"
  , "  int32 page_size = 1;"
  , "  string page_token = 2;"
  , "}"
  ]

main :: IO ()
main = do
  putStrLn "=== Proto File Parsing & Code Generation ===\n"

  -- 1. Parse the proto file
  putStrLn "--- Parsing proto IDL ---"
  case parseProtoFile "<example>" sampleProto of
    Left err -> putStrLn $ renderParseError err
    Right pf -> do
      putStrLn $ "Syntax:   " <> show (protoSyntax pf)
      putStrLn $ "Package:  " <> show (protoPackage pf)
      putStrLn $ "Options:  " <> show (length (protoOptions pf))

      -- 2. Inspect custom annotations
      putStrLn "\n--- Custom Annotations ---"
      let anns = extractAnnotations (protoOptions pf)
      case anns of
        [] -> putStrLn "  (none)"
        _  -> mapM_ (\a -> putStrLn $ "  " <> show (annotationName a) <> " = " <> show (annotationValue a)) anns

      -- 3. Enumerate top-level definitions
      putStrLn "\n--- Definitions ---"
      mapM_ showDef (protoTopLevels pf)

      -- 4. Generate Haskell code
      putStrLn "\n--- Generated Haskell Code ---"
      let opts = defaultGenerateOpts { genModulePrefix = "MyApp.Proto" }
      let code = generateModuleText opts pf
      TIO.putStrLn code

showDef :: TopLevel -> IO ()
showDef = \case
  TLMessage msg -> do
    putStrLn $ "  message " <> show (msgName msg)
    mapM_ showElem (msgElements msg)
  TLEnum e ->
    putStrLn $ "  enum " <> show (enumName e) <> " (" <> show (length (enumValues e)) <> " values)"
  TLService svc ->
    putStrLn $ "  service " <> show (svcName svc) <> " (" <> show (length (svcRpcs svc)) <> " RPCs)"
  TLExtend name _ ->
    putStrLn $ "  extend " <> show name
  TLOption opt ->
    putStrLn $ "  option " <> show (optName opt)

showElem :: MessageElement -> IO ()
showElem = \case
  MEField fd -> putStrLn $ "    field " <> show (fieldName fd) <> " = " <> show (unFieldNumber (fieldNumber fd))
  MEMessage msg -> putStrLn $ "    message " <> show (msgName msg)
  MEEnum e -> putStrLn $ "    enum " <> show (enumName e)
  MEOneof o -> putStrLn $ "    oneof " <> show (oneofName o)
  MEMapField mf -> putStrLn $ "    map " <> show (mapFieldName mf)
  _ -> pure ()
