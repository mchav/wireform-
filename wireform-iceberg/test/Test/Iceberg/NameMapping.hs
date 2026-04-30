module Test.Iceberg.NameMapping (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.JSON (nameMappingFromJSON, nameMappingToJSON)
import Iceberg.Types

tests :: TestTree
tests = testGroup "Iceberg.NameMapping"
  [ testCase "Empty name mapping round-trips" $ do
      let nm = NameMapping V.empty
      case nameMappingFromJSON (nameMappingToJSON nm) of
        Right nm' -> nm' @?= nm
        Left e    -> assertFailure e

  , testCase "Flat field mapping round-trips" $ do
      let nm = NameMapping $ V.fromList
            [ MappedField (V.fromList ["id", "ID"]) (Just 1) (NameMapping V.empty)
            , MappedField (V.fromList ["name"])     (Just 2) (NameMapping V.empty)
            ]
      case nameMappingFromJSON (nameMappingToJSON nm) of
        Right nm' -> nm' @?= nm
        Left e    -> assertFailure e

  , testCase "Nested struct field mapping round-trips" $ do
      let inner = NameMapping $ V.singleton
            (MappedField (V.singleton "x") (Just 10) (NameMapping V.empty))
          nm = NameMapping $ V.singleton
            (MappedField (V.singleton "point") (Just 5) inner)
      case nameMappingFromJSON (nameMappingToJSON nm) of
        Right nm' -> nm' @?= nm
        Left e    -> assertFailure e

  , testCase "JSON shape matches the Iceberg spec" $ do
      let nm = NameMapping $ V.singleton
            (MappedField (V.singleton "id") (Just 1) (NameMapping V.empty))
          encoded = Aeson.encode (nameMappingToJSON nm)
      Aeson.decode encoded
        @?= (Just (Aeson.Array (V.singleton
              (Aeson.object [ "names" Aeson..= ["id" :: String]
                            , "field-id" Aeson..= (1 :: Int)
                            ]))))
  ]
