{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the metadata satellite instances 'loadProto' now
-- emits in addition to the wire codecs:
--
--   * 'ProtoMessage' — schema metadata.
--   * 'Aeson.ToJSON' \/ 'Aeson.FromJSON' — proto3 canonical JSON.
--   * 'Hashable' — recursive structural hash.
--   * 'ProtoEnum' — enum metadata + numeric \<-\> name conversion.
--
-- The fixtures we exercise here are the same ones the
-- 'topenum_regression.proto' splice already generates for the
-- top-level enum tests, plus a couple of small derived messages.
module Test.Proto.Derive.Metadata (tests) where

import Data.Hashable (hash, hashWithSalt)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Proto.Schema as PS

import Test.Proto.Derive.TopEnumInstances
  ( Account (..)
  , PackedBag (..)
  , Status (..)
  , defaultAccount
  , defaultPackedBag
  )

tests :: TestTree
tests = testGroup "loadProto satellite instances"
  [ testGroup "ProtoMessage"
      [ testCase "Account name + package + default" $ do
          PS.protoMessageName  (Proxy :: Proxy Account) @?= "Account"
          PS.protoPackageName  (Proxy :: Proxy Account) @?= ""
          PS.protoDefaultValue @?= defaultAccount

      , testCase "Account field descriptors expose names + numbers" $ do
          let fields = PS.protoFieldDescriptors (Proxy :: Proxy Account)
          Map.keys fields @?= [1, 2]
          PS.messageFieldNumbers (Proxy :: Proxy Account) @?= [1, 2]
          let names = PS.messageFieldNames (Proxy :: Proxy Account)
          names @?= ["acct_name", "acct_status"]

      , testCase "Account field descriptor by name returns acct_status" $ do
          case PS.lookupFieldDescriptor "acct_status" (Proxy :: Proxy Account) of
            Just (PS.SomeField fd) -> do
              PS.fdNumber fd @?= 2
              -- Top-level enum fields appear as EnumType in the
              -- schema descriptor (not MessageType — that was the
              -- whole point of the top-level-enum loadProto fix).
              PS.fdTypeDesc fd @?= PS.EnumType "Status"
            Nothing -> error "lookupFieldDescriptor returned Nothing"

      , testCase "PackedBag schema describes a repeated int32" $ do
          case PS.lookupFieldDescriptor "bag_nums" (Proxy :: Proxy PackedBag) of
            Just (PS.SomeField fd) -> do
              PS.fdLabel fd    @?= PS.LabelRepeated
              PS.fdTypeDesc fd @?= PS.ScalarType PS.Int32Field
            Nothing -> error "PackedBag bag_nums descriptor missing"
      ]

  , testGroup "ProtoEnum (Status)"
      [ testCase "primary names round-trip through to/fromProtoEnumValue" $ do
          PS.toProtoEnumValue StatusUnspecified @?= 0
          PS.toProtoEnumValue StatusActive      @?= 1
          PS.toProtoEnumValue StatusRetired     @?= 2
          PS.toProtoEnumValue StatusBanned      @?= 3
          PS.fromProtoEnumValue 0 @?= Just StatusUnspecified
          PS.fromProtoEnumValue 1 @?= Just StatusActive
          PS.fromProtoEnumValue 99 @?= (Nothing :: Maybe Status)

      , testCase "protoEnumValues lists every declared value" $ do
          let values = PS.protoEnumValues (Proxy :: Proxy Status)
          values @?= [ ("STATUS_UNSPECIFIED", 0)
                     , ("STATUS_ACTIVE",      1)
                     , ("STATUS_RETIRED",     2)
                     , ("STATUS_BANNED",      3)
                     ]

      , testCase "fully qualified protoEnumName" $ do
          PS.protoEnumName (Proxy :: Proxy Status) @?= "Status"
      ]

  , testGroup "Aeson.ToJSON / FromJSON for messages (camelCase keys)"
      [ testCase "default Account encodes empty fields with camelCase keys" $ do
          let a = defaultAccount
          -- proto3 JSON spec: present, but empty values. Our
          -- emitter follows the pure-text codegen and writes
          -- every field unconditionally — the bytes-vs-JSON
          -- skip-default rules differ.
          let v = Aeson.toJSON a
          case v of
            Aeson.Object km -> do
              -- Both keys are present, in camelCase form.
              assertBool "acctName key present"   (hasKey "acctName" km)
              assertBool "acctStatus key present" (hasKey "acctStatus" km)
            _ -> error "expected Object"

      , testCase "Account ToJSON / FromJSON round-trip" $ do
          let a = defaultAccount
                { accountAcctName   = T.pack "alice"
                , accountAcctStatus = StatusActive
                }
          let v = Aeson.toJSON a
          case Aeson.fromJSON v of
            Aeson.Success a' -> a' @?= a
            Aeson.Error e    -> error ("fromJSON failed: " <> e)

      , testCase "Status enum encodes as its primary name string" $ do
          Aeson.toJSON StatusActive  @?= Aeson.String "STATUS_ACTIVE"
          Aeson.toJSON StatusBanned  @?= Aeson.String "STATUS_BANNED"

      , testCase "Status enum FromJSON accepts both name and number" $ do
          Aeson.fromJSON (Aeson.String "STATUS_RETIRED") @?= Aeson.Success StatusRetired
          Aeson.fromJSON (Aeson.Number 1) @?= Aeson.Success StatusActive

      , testCase "PackedBag ToJSON / FromJSON round-trip" $ do
          let p = defaultPackedBag { packedBagBagNums = V.fromList [1, 2, 3, 4, 5] }
          case Aeson.fromJSON (Aeson.toJSON p) of
            Aeson.Success p' -> p' @?= p
            Aeson.Error e    -> error ("fromJSON failed: " <> e)
      ]

  , testGroup "Hashable"
      [ testCase "equal Accounts hash equal" $ do
          let a1 = defaultAccount { accountAcctName = T.pack "x", accountAcctStatus = StatusActive }
              a2 = defaultAccount { accountAcctName = T.pack "x", accountAcctStatus = StatusActive }
          hash a1 @?= hash a2

      , testCase "different Accounts hash differently (sanity)" $ do
          let a1 = defaultAccount { accountAcctName = T.pack "x", accountAcctStatus = StatusActive }
              a2 = defaultAccount { accountAcctName = T.pack "y", accountAcctStatus = StatusActive }
          assertBool "names differ -> hashes should differ"
            (hash a1 /= hash a2)

      , testCase "Status hashes match its proto wire number" $ do
          -- The enum Hashable implementation hashes the proto
          -- wire number, so this is observable.
          hashWithSalt 0 StatusUnspecified @?= hashWithSalt 0 (0 :: Int)
          hashWithSalt 0 StatusActive      @?= hashWithSalt 0 (1 :: Int)

      , testCase "PackedBag hashes survive vector permutation differences" $ do
          let p1 = defaultPackedBag { packedBagBagNums = V.fromList [1, 2, 3] }
              p2 = defaultPackedBag { packedBagBagNums = V.fromList [3, 2, 1] }
          assertBool "vector order matters in the hash"
            (hash p1 /= hash p2)
      ]
  ]

hasKey :: T.Text -> AesonKM.KeyMap Aeson.Value -> Bool
hasKey k km = AesonKM.member (AesonKey.fromText k) km
