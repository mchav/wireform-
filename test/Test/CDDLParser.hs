module Test.CDDLParser (cddlParserTests) where

import CBOR.CDDL
import CBOR.CDDLSchema
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd


cddlParserTests :: Spec
cddlParserTests =
  describe "CDDL Parser" $
    sequence_
      [ it "parse person schema with map, array, optional, choice" $ do
          let input =
                T.pack $
                  unlines
                    [ "person = {"
                    , "  name: tstr,"
                    , "  age: uint,"
                    , "  ? email: tstr,"
                    , "}"
                    , "color = &( red: 0, green: 1, blue: 2 )"
                    , "persons = [* person]"
                    ]
          case parseCDDL input of
            Left err -> expectationFailure err
            Right (CDDLSchema rules) -> do
              V.length rules `shouldBe` 3

              let CDDLRule n1 t1 = rules V.! 0
              n1 `shouldBe` "person"
              case t1 of
                CTMap members -> do
                  V.length members `shouldBe` 3
                  let CDDLMember mn1 mt1 mo1 = members V.! 0
                  mn1 `shouldBe` "name"
                  mt1 `shouldBe` CTTstr
                  mo1 `shouldBe` Once
                  let CDDLMember mn2 mt2 mo2 = members V.! 1
                  mn2 `shouldBe` "age"
                  mt2 `shouldBe` CTUint
                  mo2 `shouldBe` Once
                  let CDDLMember mn3 mt3 mo3 = members V.! 2
                  mn3 `shouldBe` "email"
                  mt3 `shouldBe` CTTstr
                  mo3 `shouldBe` Optional
                _ -> expectationFailure ("expected CTMap, got " ++ show t1)

              let CDDLRule n2 t2 = rules V.! 1
              n2 `shouldBe` "color"
              case t2 of
                CTChoice vals -> do
                  V.length vals `shouldBe` 3
                  (vals V.! 0) `shouldBe` CTLiteral "0"
                  (vals V.! 1) `shouldBe` CTLiteral "1"
                  (vals V.! 2) `shouldBe` CTLiteral "2"
                _ -> expectationFailure ("expected CTChoice for color, got " ++ show t2)

              let CDDLRule n3 t3 = rules V.! 2
              n3 `shouldBe` "persons"
              case t3 of
                CTArray members -> do
                  V.length members `shouldBe` 1
                  let CDDLMember _ mt _ = members V.! 0
                  mt `shouldBe` CTRef "person"
                _ -> expectationFailure ("expected CTArray for persons, got " ++ show t3)
      , it "parse builtin types" $ do
          let input =
                T.pack $
                  unlines
                    [ "a = uint"
                    , "b = int"
                    , "c = tstr"
                    , "d = bstr"
                    , "e = float"
                    , "f = bool"
                    , "g = nil"
                    ]
          case parseCDDL input of
            Left err -> expectationFailure err
            Right (CDDLSchema rules) -> do
              V.length rules `shouldBe` 7
              let getType (CDDLRule _ t) = t
              getType (rules V.! 0) `shouldBe` CTUint
              getType (rules V.! 1) `shouldBe` CTInt
              getType (rules V.! 2) `shouldBe` CTTstr
              getType (rules V.! 3) `shouldBe` CTBstr
              getType (rules V.! 4) `shouldBe` CTFloat
              getType (rules V.! 5) `shouldBe` CTBool
              getType (rules V.! 6) `shouldBe` CTNil
      , it "parse choice with /" $ do
          let input = T.pack "value = uint / tstr / bool\n"
          case parseCDDL input of
            Left err -> expectationFailure err
            Right (CDDLSchema rules) -> do
              V.length rules `shouldBe` 1
              let CDDLRule _ t = rules V.! 0
              case t of
                CTChoice alts -> do
                  V.length alts `shouldBe` 3
                  (alts V.! 0) `shouldBe` CTUint
                  (alts V.! 1) `shouldBe` CTTstr
                  (alts V.! 2) `shouldBe` CTBool
                _ -> expectationFailure ("expected choice, got " ++ show t)
      , it "parse type reference" $ do
          let input = T.pack "alias = person\n"
          case parseCDDL input of
            Left err -> expectationFailure err
            Right (CDDLSchema rules) -> do
              V.length rules `shouldBe` 1
              let CDDLRule _ t = rules V.! 0
              t `shouldBe` CTRef "person"
      , it "parse tagged type" $ do
          let input = T.pack "tagged-date = #6.0(tstr)\n"
          case parseCDDL input of
            Left err -> expectationFailure err
            Right (CDDLSchema rules) -> do
              V.length rules `shouldBe` 1
              let CDDLRule _ t = rules V.! 0
              case t of
                CTTagged 0 CTTstr -> pure ()
                _ -> expectationFailure ("expected #6.0(tstr), got " ++ show t)
      , it "parse empty document" $ do
          case parseCDDL "" of
            Left err -> expectationFailure err
            Right (CDDLSchema rules) ->
              V.length rules `shouldBe` 0
      ]
