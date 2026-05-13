module Test.Resolver (resolverTests) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Proto.IDL.AST
import Proto.IDL.Parser.Resolver
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  removeDirectoryRecursive,
 )
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit


resolverTests :: TestTree
resolverTests =
  testGroup
    "Proto.IDL.Parser.Resolver"
    [ testCase "resolve simple proto with no imports" $ do
        withTempProtoDir $ \dir -> do
          writeProtoFile (dir </> "simple.proto") $
            T.unlines
              [ "syntax = \"proto3\";"
              , "package test;"
              , "message Foo {"
              , "  string name = 1;"
              , "}"
              ]
          result <- resolveProtoImports [dir] (dir </> "simple.proto")
          case result of
            Left e -> assertFailure ("resolve failed: " <> show e)
            Right rp -> do
              rpPath rp @?= dir </> "simple.proto"
              Map.null (rpImports rp) @?= True
              protoPackage (rpFile rp) @?= Just "test"
    , testCase "resolve proto with local import" $ do
        withTempProtoDir $ \dir -> do
          writeProtoFile (dir </> "dep.proto") $
            T.unlines
              [ "syntax = \"proto3\";"
              , "package dep;"
              , "message Bar {"
              , "  int32 id = 1;"
              , "}"
              ]
          writeProtoFile (dir </> "main.proto") $
            T.unlines
              [ "syntax = \"proto3\";"
              , "package main;"
              , "import \"dep.proto\";"
              , "message Foo {"
              , "  string name = 1;"
              , "}"
              ]
          result <- resolveProtoImports [dir] (dir </> "main.proto")
          case result of
            Left e -> assertFailure ("resolve failed: " <> show e)
            Right rp -> do
              Map.size (rpImports rp) @?= 1
              assertBool "import dep.proto present" (Map.member "dep.proto" (rpImports rp))
              case Map.lookup "dep.proto" (rpImports rp) of
                Nothing -> assertFailure "dep.proto not in imports"
                Just dep -> protoPackage (rpFile dep) @?= Just "dep"
    , testCase "resolve proto with subdirectory import" $ do
        withTempProtoDir $ \dir -> do
          createDirectoryIfMissing True (dir </> "sub")
          writeProtoFile (dir </> "sub" </> "dep.proto") $
            T.unlines
              [ "syntax = \"proto3\";"
              , "package sub;"
              , "message Inner {"
              , "  bool flag = 1;"
              , "}"
              ]
          writeProtoFile (dir </> "main.proto") $
            T.unlines
              [ "syntax = \"proto3\";"
              , "import \"sub/dep.proto\";"
              , "message Outer {"
              , "  string val = 1;"
              , "}"
              ]
          result <- resolveProtoImports [dir] (dir </> "main.proto")
          case result of
            Left e -> assertFailure ("resolve failed: " <> show e)
            Right rp -> do
              assertBool "sub/dep.proto imported" (Map.member "sub/dep.proto" (rpImports rp))
    , testCase "well-known import google/protobuf/timestamp.proto resolves" $ do
        withTempProtoDir $ \dir -> do
          writeProtoFile (dir </> "with_ts.proto") $
            T.unlines
              [ "syntax = \"proto3\";"
              , "import \"google/protobuf/timestamp.proto\";"
              , "message Event {"
              , "  string name = 1;"
              , "}"
              ]
          result <- resolveProtoImports [dir] (dir </> "with_ts.proto")
          case result of
            Left e -> assertFailure ("resolve failed: " <> show e)
            Right rp -> do
              assertBool
                "timestamp.proto imported"
                (Map.member "google/protobuf/timestamp.proto" (rpImports rp))
              let tsProto = rpImports rp Map.! "google/protobuf/timestamp.proto"
              protoPackage (rpFile tsProto) @?= Just "google.protobuf"
    , testCase "types from imported files available via registry" $ do
        withTempProtoDir $ \dir -> do
          writeProtoFile (dir </> "dep.proto") $
            T.unlines
              [ "syntax = \"proto3\";"
              , "package mypkg;"
              , "message Payload {"
              , "  bytes data = 1;"
              , "}"
              ]
          writeProtoFile (dir </> "main.proto") $
            T.unlines
              [ "syntax = \"proto3\";"
              , "package mypkg;"
              , "import \"dep.proto\";"
              , "message Request {"
              , "  string id = 1;"
              , "}"
              ]
          result <- resolveProtoImports [dir] (dir </> "main.proto")
          case result of
            Left e -> assertFailure ("resolve failed: " <> show e)
            Right rp -> do
              let importedProtos = rpImports rp
              assertBool "dep.proto imported" (Map.member "dep.proto" importedProtos)
              let depPf = rpFile (importedProtos Map.! "dep.proto")
                  topLevels = protoTopLevels depPf
              assertBool "Payload message found" $ any isPayloadMsg topLevels
    , testCase "missing import returns FileNotFound" $ do
        withTempProtoDir $ \dir -> do
          writeProtoFile (dir </> "bad.proto") $
            T.unlines
              [ "syntax = \"proto3\";"
              , "import \"nonexistent.proto\";"
              , "message Foo {"
              , "  int32 x = 1;"
              , "}"
              ]
          result <- resolveProtoImports [dir] (dir </> "bad.proto")
          case result of
            Left (FileNotFound _ _ _) -> pure ()
            Left e -> assertFailure ("unexpected error: " <> show e)
            Right _ -> assertFailure "expected FileNotFound error"
    , testCase "circular import detected" $ do
        withTempProtoDir $ \dir -> do
          writeProtoFile (dir </> "a.proto") $
            T.unlines
              [ "syntax = \"proto3\";"
              , "import \"b.proto\";"
              , "message A { int32 x = 1; }"
              ]
          writeProtoFile (dir </> "b.proto") $
            T.unlines
              [ "syntax = \"proto3\";"
              , "import \"a.proto\";"
              , "message B { int32 y = 1; }"
              ]
          result <- resolveProtoImports [dir] (dir </> "a.proto")
          case result of
            Left (CircularImport _) -> pure ()
            Left e -> assertFailure ("unexpected error: " <> show e)
            Right _ -> assertFailure "expected CircularImport error"
    , testCase "getBundledIncludeDir returns valid path" $ do
        dir <- getBundledIncludeDir
        assertBool "bundled dir is non-empty" (not (null dir))
    ]


isPayloadMsg :: TopLevel -> Bool
isPayloadMsg (TLMessage m) = msgName m == "Payload"
isPayloadMsg _ = False


withTempProtoDir :: (FilePath -> IO ()) -> IO ()
withTempProtoDir action = do
  let dir = "/tmp/wireform-test-resolver"
  exists <- doesDirectoryExist dir
  if exists then removeDirectoryRecursive dir else pure ()
  createDirectoryIfMissing True dir
  action dir
  removeDirectoryRecursive dir


writeProtoFile :: FilePath -> T.Text -> IO ()
writeProtoFile path content = BS.writeFile path (TE.encodeUtf8 content)
