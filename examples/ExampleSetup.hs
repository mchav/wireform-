-- | Example Setup.hs showing how to use the proto code generation hook.
--
-- Put this in your package's Setup.hs, and set build-type: Custom in
-- your .cabal file with a custom-setup stanza depending on wireform.
--
-- This will automatically find all .proto files in "proto/" and generate
-- Haskell modules into "gen/" during the pre-build step.
module Main where

-- In a real Setup.hs you would use:
-- import Distribution.Simple
-- import Proto.Setup
--
-- main :: IO ()
-- main = defaultMainWithHooks simpleUserHooks
--   { preBuild = \args flags -> do
--       protoGenPreBuildHook defaultProtoGenConfig
--         { pgcProtoDir    = "proto"
--         , pgcOutputDir   = "gen"
--         , pgcModulePrefix = "Proto.Gen"
--         }
--       preBuild simpleUserHooks args flags
--   }

-- For demonstration, we just invoke the generator directly:
import Proto.Setup

main :: IO ()
main = do
  putStrLn "=== Setup.hs Hook Demo ==="
  putStrLn ""
  putStrLn "Running proto code generation..."
  generateProtos defaultProtoGenConfig
    { pgcProtoDir    = "example"
    , pgcOutputDir   = "/tmp/wireform-setup-demo"
    , pgcModulePrefix = "Demo.Proto"
    }
  putStrLn ""
  putStrLn "Done. Check /tmp/wireform-setup-demo/ for generated files."
