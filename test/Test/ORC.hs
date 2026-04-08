module Test.ORC (orcTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import ORC.Types
import ORC.Footer

orcTests :: TestTree
orcTests = testGroup "ORC"
  [ footerTests
  , typeKindTests
  , magicTests
  , propertyTests
  ]

footerTests :: TestTree
footerTests = testGroup "Footer roundtrip"
  [ testCase "Minimal footer" $ do
      let footer = ORCFooter
            { orcHeaderLength = 3
            , orcContentLength = 100
            , orcStripes = V.empty
            , orcTypes = V.empty
            , orcMetadata = V.empty
            , orcNumberOfRows = 0
            , orcStatistics = V.empty
            }
      readORCFooter (writeORCFooter footer) @?= Right footer

  , testCase "Footer with types and stripes" $ do
      let stripe = StripeInformation
            { siOffset = 3
            , siIndexLength = 100
            , siDataLength = 5000
            , siFooterLength = 50
            , siNumberOfRows = 1000
            }
          typ = ORCType
            { otKind = TKStruct
            , otSubtypes = V.fromList [1, 2, 3]
            , otFieldNames = V.fromList ["id", "name", "value"]
            }
          col1 = ORCType
            { otKind = TKLong
            , otSubtypes = V.empty
            , otFieldNames = V.empty
            }
          col2 = ORCType
            { otKind = TKString
            , otSubtypes = V.empty
            , otFieldNames = V.empty
            }
          col3 = ORCType
            { otKind = TKDouble
            , otSubtypes = V.empty
            , otFieldNames = V.empty
            }
          footer = ORCFooter
            { orcHeaderLength = 3
            , orcContentLength = 5150
            , orcStripes = V.singleton stripe
            , orcTypes = V.fromList [typ, col1, col2, col3]
            , orcMetadata = V.empty
            , orcNumberOfRows = 1000
            , orcStatistics = V.empty
            }
      readORCFooter (writeORCFooter footer) @?= Right footer

  , testCase "Footer with metadata" $ do
      let footer = ORCFooter
            { orcHeaderLength = 3
            , orcContentLength = 0
            , orcStripes = V.empty
            , orcTypes = V.empty
            , orcMetadata = V.fromList
                [ ("key1", "value1")
                , ("key2", "value2")
                ]
            , orcNumberOfRows = 0
            , orcStatistics = V.empty
            }
      readORCFooter (writeORCFooter footer) @?= Right footer

  , testCase "Footer with column statistics" $ do
      let stats = V.fromList
            [ ColumnStatistics (Just 100) (Just False) (Just 800)
            , ColumnStatistics (Just 95) (Just True) (Just 500)
            , ColumnStatistics Nothing Nothing Nothing
            ]
          footer = ORCFooter
            { orcHeaderLength = 3
            , orcContentLength = 0
            , orcStripes = V.empty
            , orcTypes = V.empty
            , orcMetadata = V.empty
            , orcNumberOfRows = 100
            , orcStatistics = stats
            }
      readORCFooter (writeORCFooter footer) @?= Right footer

  , testCase "Footer with multiple stripes" $ do
      let mkStripe off n = StripeInformation
            { siOffset = off
            , siIndexLength = 50
            , siDataLength = 2000
            , siFooterLength = 30
            , siNumberOfRows = n
            }
          footer = ORCFooter
            { orcHeaderLength = 3
            , orcContentLength = 6240
            , orcStripes = V.fromList
                [ mkStripe 3 500
                , mkStripe 2083 500
                , mkStripe 4163 500
                ]
            , orcTypes = V.singleton ORCType
                { otKind = TKInt
                , otSubtypes = V.empty
                , otFieldNames = V.empty
                }
            , orcMetadata = V.empty
            , orcNumberOfRows = 1500
            , orcStatistics = V.empty
            }
      readORCFooter (writeORCFooter footer) @?= Right footer
  ]

typeKindTests :: TestTree
typeKindTests = testGroup "TypeKind"
  [ testCase "All TypeKinds map correctly" $ do
      let allKinds = [TKBoolean .. TKChar]
      mapM_ (\tk -> intToTypeKind (typeKindToInt tk) @?= Just tk) allKinds

  , testCase "Invalid TypeKind returns Nothing" $
      intToTypeKind 99 @?= Nothing

  , testCase "TypeKind values are sequential" $ do
      typeKindToInt TKBoolean @?= 0
      typeKindToInt TKChar @?= 17

  , testCase "Footer with all type kinds" $ do
      let allKinds = [TKBoolean .. TKChar]
          types = V.fromList $ map (\tk -> ORCType tk V.empty V.empty) allKinds
          footer = ORCFooter 3 0 V.empty types V.empty 0 V.empty
      readORCFooter (writeORCFooter footer) @?= Right footer
  ]

magicTests :: TestTree
magicTests = testGroup "Magic"
  [ testCase "PostScript magic is ORC" $
      orcMagic @?= "ORC"

  , testCase "Empty input fails" $
      case readORCFooter BS.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"

  , testCase "Too short input fails" $
      case readORCFooter (BS.pack [1, 2]) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on short input"
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property roundtrips"
  [ testProperty "Footer with random row counts" $ property $ do
      nRows <- forAll $ Gen.word64 (Range.linear 0 1000000)
      headerLen <- forAll $ Gen.word64 (Range.linear 0 100)
      contentLen <- forAll $ Gen.word64 (Range.linear 0 1000000)
      let footer = ORCFooter
            { orcHeaderLength = headerLen
            , orcContentLength = contentLen
            , orcStripes = V.empty
            , orcTypes = V.empty
            , orcMetadata = V.empty
            , orcNumberOfRows = nRows
            , orcStatistics = V.empty
            }
      readORCFooter (writeORCFooter footer) === Right footer

  , testProperty "Footer with random stripes" $ property $ do
      nStripes <- forAll $ Gen.int (Range.linear 0 5)
      stripes <- forAll $ V.replicateM nStripes $ do
        off <- Gen.word64 (Range.linear 0 10000)
        idx <- Gen.word64 (Range.linear 0 1000)
        dat <- Gen.word64 (Range.linear 0 10000)
        ftr <- Gen.word64 (Range.linear 0 1000)
        nr  <- Gen.word64 (Range.linear 0 10000)
        pure (StripeInformation off idx dat ftr nr)
      let footer = ORCFooter 3 0 stripes V.empty V.empty 0 V.empty
      readORCFooter (writeORCFooter footer) === Right footer

  , testProperty "Footer with random types" $ property $ do
      nTypes <- forAll $ Gen.int (Range.linear 0 5)
      types <- forAll $ V.replicateM nTypes $ do
        tk <- Gen.element [TKBoolean .. TKChar]
        nSub <- Gen.int (Range.linear 0 3)
        subs <- V.replicateM nSub (Gen.word32 (Range.linear 0 10))
        nFld <- Gen.int (Range.linear 0 3)
        flds <- V.replicateM nFld (Gen.text (Range.linear 1 20) Gen.alphaNum)
        pure (ORCType tk subs flds)
      let footer = ORCFooter 3 0 V.empty types V.empty 0 V.empty
      readORCFooter (writeORCFooter footer) === Right footer
  ]
