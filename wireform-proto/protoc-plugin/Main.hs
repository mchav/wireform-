{- | protoc plugin for wireform.

Usage: protoc --plugin=protoc-gen-wireform=./protoc-gen-wireform --wireform_out=gen/ foo.proto
-}
module Main where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Proto.CodeGen
import Proto.Google.Protobuf.Compiler.Plugin
import Proto.Google.Protobuf.Descriptor
import Proto.IDL.Descriptor (fileDescriptorToAST)
import Proto.IDL.Parser.Resolver (ResolvedProto (..))


main :: IO ()
main = pluginMain handleRequest


handleRequest :: CodeGeneratorRequest -> IO CodeGeneratorResponse
handleRequest req = do
  let requestedFiles = V.toList (cgrFileToGenerate req)
      allProtos = V.toList (cgrProtoFile req)
      opts = parsePluginOpts (cgrParameter req)
      resolvedPairs = fmap fdpToResolved allProtos
      typeReg = buildTypeRegistry opts resolvedPairs
      outputFiles = concatMap (generateForFile opts typeReg requestedFiles allProtos) allProtos
  pure
    defaultCodeGeneratorResponse
      { cgrsFile = V.fromList outputFiles
      , cgrsSupportedFeatures = 1
      }


parsePluginOpts :: T.Text -> GenerateOpts
parsePluginOpts _param = defaultGenerateOpts


fdpToResolved :: FileDescriptorProto -> (FilePath, ResolvedProto)
fdpToResolved fdp =
  let path = T.unpack (fdpName fdp)
      pf = fileDescriptorToAST fdp
  in ( path
     , ResolvedProto {rpFile = pf, rpPath = path, rpImports = Map.empty}
     )


generateForFile
  :: GenerateOpts
  -> TypeRegistry
  -> [T.Text]
  -> [FileDescriptorProto]
  -> FileDescriptorProto
  -> [CodeGeneratorResponseFile]
generateForFile opts reg requestedFiles _allFdps fdp =
  if fdpName fdp `elem` requestedFiles
    then
      let filePath = T.unpack (fdpName fdp)
          pf = fileDescriptorToAST fdp
          moduleName = moduleNameForProto opts filePath pf
          outputPath = T.replace "." "/" moduleName <> ".hs"
          content = generateModuleText opts reg filePath pf
      in [CodeGeneratorResponseFile outputPath content]
    else []
