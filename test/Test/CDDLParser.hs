module Test.CDDLParser (cddlParserTests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import CBOR.CDDLSchema
import CBOR.CDDL

cddlParserTests :: TestTree
cddlParserTests = testGroup "CDDL Parser"
  [ testCase "parse person schema with map, array, optional, choice" $ do
      let input = T.pack $ unlines
            [ "person = {"
            , "  name: tstr,"
            , "  age: uint,"
            , "  ? email: tstr,"
            , "}"
            , "color = &( red: 0, green: 1, blue: 2 )"
            , "persons = [* person]"
            ]
      case parseCDDL input of
        Left err -> assertFailure err
        Right (CDDLSchema rules) -> do
          V.length rules @?= 3

          let CDDLRule n1 t1 = rules V.! 0
          n1 @?= "person"
          case t1 of
            CTMap members -> do
              V.length members @?= 3
              let CDDLMember mn1 mt1 mo1 = members V.! 0
              mn1 @?= "name"
              mt1 @?= CTTstr
              mo1 @?= Once
              let CDDLMember mn2 mt2 mo2 = members V.! 1
              mn2 @?= "age"
              mt2 @?= CTUint
              mo2 @?= Once
              let CDDLMember mn3 mt3 mo3 = members V.! 2
              mn3 @?= "email"
              mt3 @?= CTTstr
              mo3 @?= Optional
            _ -> assertFailure ("expected CTMap, got " ++ show t1)

          let CDDLRule n2 t2 = rules V.! 1
          n2 @?= "color"
          case t2 of
            CTChoice vals -> do
              V.length vals @?= 3
              (vals V.! 0) @?= CTLiteral "0"
              (vals V.! 1) @?= CTLiteral "1"
              (vals V.! 2) @?= CTLiteral "2"
            _ -> assertFailure ("expected CTChoice for color, got " ++ show t2)

          let CDDLRule n3 t3 = rules V.! 2
          n3 @?= "persons"
          case t3 of
            CTArray members -> do
              V.length members @?= 1
              let CDDLMember _ mt _ = members V.! 0
              mt @?= CTRef "person"
            _ -> assertFailure ("expected CTArray for persons, got " ++ show t3)

  , testCase "parse builtin types" $ do
      let input = T.pack $ unlines
            [ "a = uint"
            , "b = int"
            , "c = tstr"
            , "d = bstr"
            , "e = float"
            , "f = bool"
            , "g = nil"
            ]
      case parseCDDL input of
        Left err -> assertFailure err
        Right (CDDLSchema rules) -> do
          V.length rules @?= 7
          let getType (CDDLRule _ t) = t
          getType (rules V.! 0) @?= CTUint
          getType (rules V.! 1) @?= CTInt
          getType (rules V.! 2) @?= CTTstr
          getType (rules V.! 3) @?= CTBstr
          getType (rules V.! 4) @?= CTFloat
          getType (rules V.! 5) @?= CTBool
          getType (rules V.! 6) @?= CTNil

  , testCase "parse choice with /" $ do
      let input = T.pack "value = uint / tstr / bool\n"
      case parseCDDL input of
        Left err -> assertFailure err
        Right (CDDLSchema rules) -> do
          V.length rules @?= 1
          let CDDLRule _ t = rules V.! 0
          case t of
            CTChoice alts -> do
              V.length alts @?= 3
              (alts V.! 0) @?= CTUint
              (alts V.! 1) @?= CTTstr
              (alts V.! 2) @?= CTBool
            _ -> assertFailure ("expected choice, got " ++ show t)

  , testCase "parse type reference" $ do
      let input = T.pack "alias = person\n"
      case parseCDDL input of
        Left err -> assertFailure err
        Right (CDDLSchema rules) -> do
          V.length rules @?= 1
          let CDDLRule _ t = rules V.! 0
          t @?= CTRef "person"

  , testCase "parse tagged type" $ do
      let input = T.pack "tagged-date = #6.0(tstr)\n"
      case parseCDDL input of
        Left err -> assertFailure err
        Right (CDDLSchema rules) -> do
          V.length rules @?= 1
          let CDDLRule _ t = rules V.! 0
          case t of
            CTTagged 0 CTTstr -> pure ()
            _ -> assertFailure ("expected #6.0(tstr), got " ++ show t)

  , testCase "parse empty document" $ do
      case parseCDDL "" of
        Left err -> assertFailure err
        Right (CDDLSchema rules) ->
          V.length rules @?= 0
  ]
