module Test.ASN1Parser (asn1ParserTests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Syd

import ASN1.Schema
import ASN1.Parser

asn1ParserTests :: Spec
asn1ParserTests = describe "ASN.1 Parser" $ sequence_
  [ it "parse module with SEQUENCE, ENUMERATED, constraints, OPTIONAL" $ do
      let input = T.pack $ unlines
            [ "MyModule DEFINITIONS AUTOMATIC TAGS ::= BEGIN"
            , "  Person ::= SEQUENCE {"
            , "    name    UTF8String,"
            , "    age     INTEGER (0..150),"
            , "    email   UTF8String OPTIONAL"
            , "  }"
            , "  Color ::= ENUMERATED { red(0), green(1), blue(2) }"
            , "  PhoneNumber ::= SEQUENCE {"
            , "    number  VisibleString (SIZE(1..15)),"
            , "    type    ENUMERATED { home(0), work(1), mobile(2) }"
            , "  }"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> expectationFailure err
        Right m -> do
          asnModuleName m `shouldBe` "MyModule"
          asnTagMode m `shouldBe` AutomaticTags
          V.length (asnAssignments m) `shouldBe` 3

          let TypeAssignment n1 td1 = asnAssignments m V.! 0
          n1 `shouldBe` "Person"
          case td1 of
            TDSequence comps -> do
              V.length comps `shouldBe` 3
              let ComponentType cn1 ct1 opt1 = comps V.! 0
              cn1 `shouldBe` "name"
              ct1 `shouldBe` TDUTF8String
              opt1 `shouldBe` False
              let ComponentType cn2 ct2 opt2 = comps V.! 1
              cn2 `shouldBe` "age"
              case ct2 of
                TDInteger (Just (RangeConstraint (Just 0) (Just 150))) -> pure ()
                _ -> expectationFailure ("expected INTEGER (0..150), got " ++ show ct2)
              opt2 `shouldBe` False
              let ComponentType cn3 ct3 opt3 = comps V.! 2
              cn3 `shouldBe` "email"
              ct3 `shouldBe` TDUTF8String
              opt3 `shouldBe` True
            _ -> expectationFailure "expected SEQUENCE for Person"

          let TypeAssignment n2 td2 = asnAssignments m V.! 1
          n2 `shouldBe` "Color"
          case td2 of
            TDEnumerated vals -> do
              V.length vals `shouldBe` 3
              (fst (vals V.! 0)) `shouldBe` "red"
              (snd (vals V.! 0)) `shouldBe` Just 0
              (fst (vals V.! 1)) `shouldBe` "green"
              (snd (vals V.! 1)) `shouldBe` Just 1
              (fst (vals V.! 2)) `shouldBe` "blue"
              (snd (vals V.! 2)) `shouldBe` Just 2
            _ -> expectationFailure "expected ENUMERATED for Color"

          let TypeAssignment n3 td3 = asnAssignments m V.! 2
          n3 `shouldBe` "PhoneNumber"
          case td3 of
            TDSequence comps -> do
              V.length comps `shouldBe` 2
              let ComponentType pn1 pt1 _ = comps V.! 0
              pn1 `shouldBe` "number"
              pt1 `shouldBe` TDVisibleString
            _ -> expectationFailure "expected SEQUENCE for PhoneNumber"

  , it "parse IMPLICIT tags" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS IMPLICIT TAGS ::= BEGIN"
            , "  Flag ::= BOOLEAN"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> expectationFailure err
        Right m -> do
          asnTagMode m `shouldBe` ImplicitTags
          V.length (asnAssignments m) `shouldBe` 1

  , it "parse EXPLICIT tags" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS EXPLICIT TAGS ::= BEGIN"
            , "  Null ::= NULL"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> expectationFailure err
        Right m -> do
          asnTagMode m `shouldBe` ExplicitTags
          let TypeAssignment _ td = asnAssignments m V.! 0
          td `shouldBe` TDNULL

  , it "parse CHOICE type" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS AUTOMATIC TAGS ::= BEGIN"
            , "  Value ::= CHOICE {"
            , "    text    UTF8String,"
            , "    number  INTEGER"
            , "  }"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> expectationFailure err
        Right m -> do
          let TypeAssignment _ td = asnAssignments m V.! 0
          case td of
            TDChoice comps -> do
              V.length comps `shouldBe` 2
              let ComponentType cn1 ct1 _ = comps V.! 0
              cn1 `shouldBe` "text"
              ct1 `shouldBe` TDUTF8String
              let ComponentType cn2 ct2 _ = comps V.! 1
              cn2 `shouldBe` "number"
              case ct2 of
                TDInteger Nothing -> pure ()
                _ -> expectationFailure "expected INTEGER without constraint"
            _ -> expectationFailure "expected CHOICE"

  , it "parse SEQUENCE OF" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS AUTOMATIC TAGS ::= BEGIN"
            , "  Names ::= SEQUENCE OF UTF8String"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> expectationFailure err
        Right m -> do
          let TypeAssignment _ td = asnAssignments m V.! 0
          case td of
            TDSequenceOf TDUTF8String -> pure ()
            _ -> expectationFailure ("expected SEQUENCE OF UTF8String, got " ++ show td)

  , it "parse named type reference" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS AUTOMATIC TAGS ::= BEGIN"
            , "  Alias ::= Person"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> expectationFailure err
        Right m -> do
          let TypeAssignment _ td = asnAssignments m V.! 0
          td `shouldBe` TDNamedType "Person"

  , it "parse BIT STRING" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS AUTOMATIC TAGS ::= BEGIN"
            , "  Flags ::= BIT STRING"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> expectationFailure err
        Right m -> do
          let TypeAssignment _ td = asnAssignments m V.! 0
          td `shouldBe` TDBitString

  , it "parse OCTET STRING with SIZE constraint" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS AUTOMATIC TAGS ::= BEGIN"
            , "  Data ::= OCTET STRING (SIZE(1..256))"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> expectationFailure err
        Right m -> do
          let TypeAssignment _ td = asnAssignments m V.! 0
          case td of
            TDOctetString (Just (SizeConstraint (Just 1) (Just 256))) -> pure ()
            _ -> expectationFailure ("expected OCTET STRING (SIZE(1..256)), got " ++ show td)
  ]
