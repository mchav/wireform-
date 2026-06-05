{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the metadata satellite instances 'loadProto' now
emits in addition to the wire codecs:

  * 'ProtoMessage' — schema metadata.
  * 'Aeson.ToJSON' \/ 'Aeson.FromJSON' — proto3 canonical JSON.
  * 'Hashable' — recursive structural hash.
  * 'ProtoEnum' — enum metadata + numeric \<-\> name conversion.

The fixtures we exercise here are the same ones the
'topenum_regression.proto' splice already generates for the
top-level enum tests, plus a couple of small derived messages.
-}
module Test.Proto.Derive.Metadata (tests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Hashable (hash, hashWithSalt)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Proto.Schema qualified as PS
import Test.Proto.Derive.TopEnumInstances (
  Account (..),
  PackedBag (..),
  Status (..),
  defaultAccount,
  defaultPackedBag,
 )
import Test.Syd


tests :: Spec
tests =
  describe
    "loadProto satellite instances" $ sequence_
    [ describe
        "ProtoMessage" $ sequence_
        [ it "Account name + package + default" $ do
            PS.protoMessageName (Proxy :: Proxy Account) `shouldBe` "Account"
            PS.protoPackageName (Proxy :: Proxy Account) `shouldBe` ""
            PS.protoDefaultValue `shouldBe` defaultAccount
        , it "Account field descriptors expose names + numbers" $ do
            let fields = PS.protoFieldDescriptors (Proxy :: Proxy Account)
            Map.keys fields `shouldBe` [1, 2]
            PS.messageFieldNumbers (Proxy :: Proxy Account) `shouldBe` [1, 2]
            let names = PS.messageFieldNames (Proxy :: Proxy Account)
            names `shouldBe` ["acct_name", "acct_status"]
        , it "Account field descriptor by name returns acct_status" $ do
            case PS.lookupFieldDescriptor "acct_status" (Proxy :: Proxy Account) of
              Just (PS.SomeField fd) -> do
                PS.fdNumber fd `shouldBe` 2
                -- Top-level enum fields appear as EnumType in the
                -- schema descriptor (not MessageType — that was the
                -- whole point of the top-level-enum loadProto fix).
                PS.fdTypeDesc fd `shouldBe` PS.EnumType "Status"
              Nothing -> error "lookupFieldDescriptor returned Nothing"
        , it "PackedBag schema describes a repeated int32" $ do
            case PS.lookupFieldDescriptor "bag_nums" (Proxy :: Proxy PackedBag) of
              Just (PS.SomeField fd) -> do
                PS.fdLabel fd `shouldBe` PS.LabelRepeated
                PS.fdTypeDesc fd `shouldBe` PS.ScalarType PS.Int32Field
              Nothing -> error "PackedBag bag_nums descriptor missing"
        ]
    , describe
        "ProtoEnum (Status)" $ sequence_
        [ it "primary names round-trip through to/fromProtoEnumValue" $ do
            PS.toProtoEnumValue Status'StatusUnspecified `shouldBe` 0
            PS.toProtoEnumValue Status'StatusActive `shouldBe` 1
            PS.toProtoEnumValue Status'StatusRetired `shouldBe` 2
            PS.toProtoEnumValue Status'StatusBanned `shouldBe` 3
            PS.fromProtoEnumValue 0 `shouldBe` Just Status'StatusUnspecified
            PS.fromProtoEnumValue 1 `shouldBe` Just Status'StatusActive
            -- Open-enum representation: an unknown wire value
            -- now round-trips through the synthetic 'Unknown'
            -- variant rather than disappearing as 'Nothing'.
            PS.fromProtoEnumValue 99 `shouldBe` Just (Status'Unknown 99)
        , it "protoEnumValues lists every declared value" $ do
            let values = PS.protoEnumValues (Proxy :: Proxy Status)
            values
              `shouldBe` [ ("STATUS_UNSPECIFIED", 0)
                  , ("STATUS_ACTIVE", 1)
                  , ("STATUS_RETIRED", 2)
                  , ("STATUS_BANNED", 3)
                  ]
        , it "fully qualified protoEnumName" $ do
            PS.protoEnumName (Proxy :: Proxy Status) `shouldBe` "Status"
        ]
    , describe
        "Aeson.ToJSON / FromJSON for messages (camelCase keys)" $ sequence_
        [ it "default Account encodes to {} (proto3 canonical: defaults skipped)" $ do
            let a = defaultAccount
            let v = Aeson.toJSON a
            case v of
              Aeson.Object km -> do
                -- Proto3 canonical JSON omits fields at default value
                -- (empty string for acctName, STATUS_UNSPECIFIED for
                -- acctStatus). The result is an empty object.
                (not (hasKey "acctName" km)) `shouldBe` True
                (not (hasKey "acctStatus" km)) `shouldBe` True
              _ -> error "expected Object"
        , it "non-default Account emits both fields with camelCase keys" $ do
            let a =
                  defaultAccount
                    { accountAcctName = T.pack "ada"
                    , accountAcctStatus = Status'StatusActive
                    }
            let v = Aeson.toJSON a
            case v of
              Aeson.Object km -> do
                (hasKey "acctName" km) `shouldBe` True
                (hasKey "acctStatus" km) `shouldBe` True
              _ -> error "expected Object"
        , it "Account ToJSON / FromJSON round-trip" $ do
            let a =
                  defaultAccount
                    { accountAcctName = T.pack "alice"
                    , accountAcctStatus = Status'StatusActive
                    }
            let v = Aeson.toJSON a
            case Aeson.fromJSON v of
              Aeson.Success a' -> a' `shouldBe` a
              Aeson.Error e -> error ("fromJSON failed: " <> e)
        , it "Status enum encodes as its primary name string" $ do
            Aeson.toJSON Status'StatusActive `shouldBe` Aeson.String "STATUS_ACTIVE"
            Aeson.toJSON Status'StatusBanned `shouldBe` Aeson.String "STATUS_BANNED"
        , it "Status enum FromJSON accepts both name and number" $ do
            Aeson.fromJSON (Aeson.String "STATUS_RETIRED") `shouldBe` Aeson.Success Status'StatusRetired
            Aeson.fromJSON (Aeson.Number 1) `shouldBe` Aeson.Success Status'StatusActive
        , it "PackedBag ToJSON / FromJSON round-trip" $ do
            let p = defaultPackedBag {packedBagBagNums = V.fromList [1, 2, 3, 4, 5]}
            case Aeson.fromJSON (Aeson.toJSON p) of
              Aeson.Success p' -> p' `shouldBe` p
              Aeson.Error e -> error ("fromJSON failed: " <> e)
        , it "PackedBag with empty bagNums encodes to {} (proto3-canonical)" $ do
            let p = defaultPackedBag
            case Aeson.toJSON p of
              Aeson.Object km -> (not (hasKey "bagNums" km)) `shouldBe` True
              _ -> error "expected Object"
        ]
    , describe
        "Hashable" $ sequence_
        [ it "equal Accounts hash equal" $ do
            let a1 = defaultAccount {accountAcctName = T.pack "x", accountAcctStatus = Status'StatusActive}
                a2 = defaultAccount {accountAcctName = T.pack "x", accountAcctStatus = Status'StatusActive}
            hash a1 `shouldBe` hash a2
        , it "different Accounts hash differently (sanity)" $ do
            let a1 = defaultAccount {accountAcctName = T.pack "x", accountAcctStatus = Status'StatusActive}
                a2 = defaultAccount {accountAcctName = T.pack "y", accountAcctStatus = Status'StatusActive}
            (hash a1 /= hash a2) `shouldBe` True
        , it "Status hashes match its proto wire number" $ do
            -- The enum Hashable implementation hashes the proto
            -- wire number, so this is observable.
            hashWithSalt 0 Status'StatusUnspecified `shouldBe` hashWithSalt 0 (0 :: Int)
            hashWithSalt 0 Status'StatusActive `shouldBe` hashWithSalt 0 (1 :: Int)
        , it "PackedBag hashes survive vector permutation differences" $ do
            let p1 = defaultPackedBag {packedBagBagNums = V.fromList [1, 2, 3]}
                p2 = defaultPackedBag {packedBagBagNums = V.fromList [3, 2, 1]}
            (hash p1 /= hash p2) `shouldBe` True
        ]
    ]


hasKey :: T.Text -> AesonKM.KeyMap Aeson.Value -> Bool
hasKey k = AesonKM.member (AesonKey.fromText k)
