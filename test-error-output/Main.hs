{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Proto.IDL.Parser (parseProtoFile, renderParseError)


main :: IO ()
main = do
  test
    "Missing semicolon"
    "syntax = \"proto3\";\nmessage Foo {\n  string name = 1\n}\n"

  test
    "Missing closing brace"
    "syntax = \"proto3\";\nmessage Foo {\n  string name = 1;\n"

  test
    "Bad syntax declaration"
    "syntax = \"proto4\";\n"

  test
    "Missing equals in field"
    "syntax = \"proto3\";\nmessage Foo {\n  string name 1;\n}\n"

  test
    "Missing field number"
    "syntax = \"proto3\";\nmessage Foo {\n  string name = ;\n}\n"

  test
    "Unexpected token at top level"
    "syntax = \"proto3\";\n12345\n"

  test
    "Unclosed string literal"
    "syntax = \"proto3;\n"

  test
    "Missing comma in map type"
    "syntax = \"proto3\";\nmessage Foo {\n  map<string int32> x = 1;\n}\n"

  test
    "Error deep in file"
    ( T.unlines
        [ "syntax = \"proto3\";"
        , "package myapp.users;"
        , ""
        , "import \"google/protobuf/timestamp.proto\";"
        , ""
        , "message User {"
        , "  string name = 1;"
        , "  int32 age = 2;"
        , "  bool active = 3;"
        , "  string email = 4"
        , "}"
        ]
    )

  test
    "Invalid syntax version"
    "syntax = \"proto5\";\nmessage Foo {}\n"

  test
    "Missing message name"
    "syntax = \"proto3\";\nmessage {\n  string name = 1;\n}\n"

  test
    "Duplicate semicolons"
    "syntax = \"proto3\";\nmessage Foo {\n  string name = 1;;\n}\n"


test :: String -> T.Text -> IO ()
test name input = do
  putStrLn $ "=== " <> name <> " ==="
  case parseProtoFile "example.proto" input of
    Left e -> putStrLn (renderParseError e)
    Right _ -> putStrLn "(parsed OK)\n"
