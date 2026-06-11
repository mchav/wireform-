{- | Round-trip tests for the auto-detecting 'Proto.TH.Derive.deriveProto'
entry point. These records carry no shape hints — only the
mandatory @tag N@ annotations — so passing tests prove the
type-driven shape detection (Vector / [] / Map / sum-of-tagged
constructors / Enum) actually works end-to-end.
-}
module Test.Proto.Derive.Auto (tests) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word8)
import Proto.Decode qualified as PD
import Proto.Encode qualified as PE
import Test.Proto.Derive.AutoInstances ()
import Test.Proto.Derive.AutoTypes (
  AutoCard (..),
  AutoChoice (..),
  AutoColor (..),
  AutoEnvelope (..),
  AutoPackedNums (..),
  AutoTagged (..),
 )
import Test.Syd


tests :: Spec
tests =
  describe
    "Proto.TH.Derive auto-detection (analyseField)"
    $ sequence_
      [ describe
          "auto-detected enum (Maybe AutoColor)"
          $ sequence_
            [ it "default round-trips" $ do
                let c = AutoCard 0 Nothing []
                PD.decodeMessage (PE.encodeMessage c) `shouldBe` Right c
            , it "Just AutoBlue round-trips" $ do
                let c = AutoCard 7 (Just AutoBlue) [T.pack "n1", T.pack "n2"]
                PD.decodeMessage (PE.encodeMessage c) `shouldBe` Right c
            , it "Just AutoRed (zero-valued enum) still encodes" $ do
                -- Field-presence semantics: a Just at the zero enum is
                -- still observably present, because the carrier is Maybe.
                let c = AutoCard 0 (Just AutoRed) []
                PD.decodeMessage (PE.encodeMessage c) `shouldBe` Right c
            ]
      , describe
          "auto-detected list-repeated string (FKRepeated RepList)"
          $ sequence_
            [ it "empty list round-trips" $ do
                let c = AutoCard 0 Nothing []
                PD.decodeMessage (PE.encodeMessage c) `shouldBe` Right c
            , it "preserves order" $ do
                let c =
                      AutoCard
                        1
                        Nothing
                        [T.pack "alpha", T.pack "beta", T.pack "gamma"]
                PD.decodeMessage (PE.encodeMessage c) `shouldBe` Right c
            ]
      , describe
          "auto-detected map (FKMap MapKeyString)"
          $ sequence_
            [ it "empty map round-trips" $ do
                let t = AutoTagged T.empty Map.empty
                PE.encodeMessage t `shouldBe` BS.empty
                PD.decodeMessage (PE.encodeMessage t) `shouldBe` Right t
            , it "two entries round-trip" $ do
                let attrs =
                      Map.fromList
                        [ (T.pack "color", T.pack "red")
                        , (T.pack "size", T.pack "L")
                        ]
                    t = AutoTagged (T.pack "demo") attrs
                PD.decodeMessage (PE.encodeMessage t) `shouldBe` Right t
            ]
      , describe
          "auto-detected oneof (sum of tagged single-arg constructors)"
          $ sequence_
            [ it "no variant set: round-trips empty payload" $ do
                let e = AutoEnvelope T.empty Nothing
                PE.encodeMessage e `shouldBe` BS.empty
                PD.decodeMessage (PE.encodeMessage e) `shouldBe` Right e
            , it "AutoUrl variant round-trips" $ do
                let e =
                      AutoEnvelope
                        (T.pack "x")
                        (Just (AutoUrl (T.pack "https://example/x")))
                PD.decodeMessage (PE.encodeMessage e) `shouldBe` Right e
            , it "AutoSeed variant round-trips" $ do
                let e = AutoEnvelope T.empty (Just (AutoSeed 42))
                PD.decodeMessage (PE.encodeMessage e) `shouldBe` Right e
            , it "later variant on wire wins (proto3 oneof semantics)" $ do
                let urlE = AutoEnvelope T.empty (Just (AutoUrl (T.pack "old")))
                    seedE = AutoEnvelope T.empty (Just (AutoSeed 7))
                    combined = PE.encodeMessage urlE `BS.append` PE.encodeMessage seedE
                PD.decodeMessage combined `shouldBe` Right seedE
            ]
      , describe
          "auto-detected Vector Int32 — packed encoding by default"
          $ sequence_
            [ it "empty vector encodes to 0 bytes" $ do
                let p = AutoPackedNums T.empty V.empty
                PE.encodeMessage p `shouldBe` BS.empty
            , it "Vector [1,2,3] encodes as a packed length-delimited block" $ do
                -- field 1 (tag) is empty (default), so all bytes belong
                -- to field 2's packed block. Wire shape:
                --   tag = (2<<3)|2 = 0x12
                --   len = 3 (three 1-byte varints)
                --   payload = 0x01 0x02 0x03
                let p = AutoPackedNums T.empty (V.fromList [1, 2, 3])
                PE.encodeMessage p `shouldBe` BS.pack [0x12, 0x03, 0x01, 0x02, 0x03]
                -- And the decoder accepts it.
                PD.decodeMessage (PE.encodeMessage p) `shouldBe` Right p
            , it "decoder accepts unpacked encoding (proto3 spec)" $ do
                -- Hand-craft an unpacked stream of three int32s and
                -- assert the decoder still produces the same Vector.
                let unpacked =
                      BS.pack
                        [ 0x10
                        , 0x01 -- field 2 varint, value 1
                        , 0x10
                        , 0x02 -- field 2 varint, value 2
                        , 0x10
                        , 0x03 -- field 2 varint, value 3
                        ]
                    expected = AutoPackedNums T.empty (V.fromList [1, 2, 3])
                PD.decodeMessage unpacked `shouldBe` Right expected
            , it "Vector with a tag field also round-trips" $ do
                let p = AutoPackedNums (T.pack "k") (V.fromList [42, 99, 1])
                PD.decodeMessage (PE.encodeMessage p) `shouldBe` Right p
            , it "packed bytes have only one tag for the whole vector" $ do
                let p = AutoPackedNums T.empty (V.fromList [1, 2, 3, 4, 5])
                    bs = PE.encodeMessage p
                -- Count occurrences of the tag byte 0x12. Packed: exactly 1.
                -- Unpacked would emit 5 (one per element).
                (if (countOf 0x12 bs == 1) then pure () else expectationFailure ("expected exactly one 0x12 tag, got " <> show (countOf 0x12 bs)))
            ]
      ]


countOf :: Word8 -> BS.ByteString -> Int
countOf b = BS.length . BS.filter (== b)
