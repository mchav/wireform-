module Test.ASN1Parser (asn1ParserTests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import ASN1.Schema
import ASN1.Parser

asn1ParserTests :: TestTree
asn1ParserTests = testGroup "ASN.1 Parser"
  [ testCase "parse module with SEQUENCE, ENUMERATED, constraints, OPTIONAL" $ do
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
        Left err -> assertFailure err
        Right m -> do
          asnModuleName m @?= "MyModule"
          asnTagMode m @?= AutomaticTags
          V.length (asnAssignments m) @?= 3

          let TypeAssignment n1 td1 = asnAssignments m V.! 0
          n1 @?= "Person"
          case td1 of
            TDSequence comps -> do
              V.length comps @?= 3
              let ComponentType cn1 ct1 opt1 = comps V.! 0
              cn1 @?= "name"
              ct1 @?= TDUTF8String
              opt1 @?= False
              let ComponentType cn2 ct2 opt2 = comps V.! 1
              cn2 @?= "age"
              case ct2 of
                TDInteger (Just (RangeConstraint (Just 0) (Just 150))) -> pure ()
                _ -> assertFailure ("expected INTEGER (0..150), got " ++ show ct2)
              opt2 @?= False
              let ComponentType cn3 ct3 opt3 = comps V.! 2
              cn3 @?= "email"
              ct3 @?= TDUTF8String
              opt3 @?= True
            _ -> assertFailure "expected SEQUENCE for Person"

          let TypeAssignment n2 td2 = asnAssignments m V.! 1
          n2 @?= "Color"
          case td2 of
            TDEnumerated vals -> do
              V.length vals @?= 3
              (fst (vals V.! 0)) @?= "red"
              (snd (vals V.! 0)) @?= Just 0
              (fst (vals V.! 1)) @?= "green"
              (snd (vals V.! 1)) @?= Just 1
              (fst (vals V.! 2)) @?= "blue"
              (snd (vals V.! 2)) @?= Just 2
            _ -> assertFailure "expected ENUMERATED for Color"

          let TypeAssignment n3 td3 = asnAssignments m V.! 2
          n3 @?= "PhoneNumber"
          case td3 of
            TDSequence comps -> do
              V.length comps @?= 2
              let ComponentType pn1 pt1 _ = comps V.! 0
              pn1 @?= "number"
              pt1 @?= TDVisibleString
            _ -> assertFailure "expected SEQUENCE for PhoneNumber"

  , testCase "parse IMPLICIT tags" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS IMPLICIT TAGS ::= BEGIN"
            , "  Flag ::= BOOLEAN"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> assertFailure err
        Right m -> do
          asnTagMode m @?= ImplicitTags
          V.length (asnAssignments m) @?= 1

  , testCase "parse EXPLICIT tags" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS EXPLICIT TAGS ::= BEGIN"
            , "  Null ::= NULL"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> assertFailure err
        Right m -> do
          asnTagMode m @?= ExplicitTags
          let TypeAssignment _ td = asnAssignments m V.! 0
          td @?= TDNULL

  , testCase "parse CHOICE type" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS AUTOMATIC TAGS ::= BEGIN"
            , "  Value ::= CHOICE {"
            , "    text    UTF8String,"
            , "    number  INTEGER"
            , "  }"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> assertFailure err
        Right m -> do
          let TypeAssignment _ td = asnAssignments m V.! 0
          case td of
            TDChoice comps -> do
              V.length comps @?= 2
              let ComponentType cn1 ct1 _ = comps V.! 0
              cn1 @?= "text"
              ct1 @?= TDUTF8String
              let ComponentType cn2 ct2 _ = comps V.! 1
              cn2 @?= "number"
              case ct2 of
                TDInteger Nothing -> pure ()
                _ -> assertFailure "expected INTEGER without constraint"
            _ -> assertFailure "expected CHOICE"

  , testCase "parse SEQUENCE OF" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS AUTOMATIC TAGS ::= BEGIN"
            , "  Names ::= SEQUENCE OF UTF8String"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> assertFailure err
        Right m -> do
          let TypeAssignment _ td = asnAssignments m V.! 0
          case td of
            TDSequenceOf TDUTF8String -> pure ()
            _ -> assertFailure ("expected SEQUENCE OF UTF8String, got " ++ show td)

  , testCase "parse named type reference" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS AUTOMATIC TAGS ::= BEGIN"
            , "  Alias ::= Person"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> assertFailure err
        Right m -> do
          let TypeAssignment _ td = asnAssignments m V.! 0
          td @?= TDNamedType "Person"

  , testCase "parse BIT STRING" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS AUTOMATIC TAGS ::= BEGIN"
            , "  Flags ::= BIT STRING"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> assertFailure err
        Right m -> do
          let TypeAssignment _ td = asnAssignments m V.! 0
          td @?= TDBitString

  , testCase "parse OCTET STRING with SIZE constraint" $ do
      let input = T.pack $ unlines
            [ "TestMod DEFINITIONS AUTOMATIC TAGS ::= BEGIN"
            , "  Data ::= OCTET STRING (SIZE(1..256))"
            , "END"
            ]
      case parseASN1Module input of
        Left err -> assertFailure err
        Right m -> do
          let TypeAssignment _ td = asnAssignments m V.! 0
          case td of
            TDOctetString (Just (SizeConstraint (Just 1) (Just 256))) -> pure ()
            _ -> assertFailure ("expected OCTET STRING (SIZE(1..256)), got " ++ show td)
  ]
