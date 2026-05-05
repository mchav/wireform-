-- | Tests for the top-level enum + packed scalar 'loadProto'
-- regression.
module Test.Proto.Derive.TopEnum (tests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Proto.Decode as PD
import qualified Proto.Encode as PE

import Test.Proto.Derive.TopEnumInstances
  ( Account (..)
  , PackedBag (..)
  , Status (..)
  , defaultAccount
  , defaultPackedBag
  )

tests :: TestTree
tests = testGroup "Proto.TH top-level enum + packed scalar"
  [ testGroup "top-level enum field encodes as varint, not submessage"
      [ testCase "default Status (UNSPECIFIED = 0) is skipped" $ do
          let a = defaultAccount
          PE.encodeMessage a @?= BS.empty

      , testCase "Status = STATUS_ACTIVE encodes as varint 1, not as a submessage" $ do
          let a = defaultAccount { acctStatus = StatusActive }
          -- field 2 (varint) tag = (2<<3)|0 = 0x10; payload = 1.
          -- If the bridge had wrongly chosen PFSubmessage we'd
          -- see 0x12 ((2<<3)|2) followed by a length prefix here.
          PE.encodeMessage a @?= BS.pack [0x10, 0x01]

      , testCase "Status = STATUS_BANNED round-trips" $ do
          let a = defaultAccount
                { acctName   = T.pack "anon"
                , acctStatus = StatusBanned
                }
          PD.decodeMessage (PE.encodeMessage a) @?= Right a

      , testCase "all four Status values round-trip" $ do
          let go st = do
                let a = defaultAccount { acctStatus = st }
                PD.decodeMessage (PE.encodeMessage a) @?= Right a
          mapM_ go [ StatusUnspecified, StatusActive
                   , StatusRetired,    StatusBanned ]
      ]

  , testGroup "packed scalar (proto3 spec default for repeated int32)"
      [ testCase "empty bag encodes to 0 bytes" $ do
          PE.encodeMessage defaultPackedBag @?= BS.empty

      , testCase "Vector [1,2,3] is one length-delimited block" $ do
          let p = defaultPackedBag { bagNums = V.fromList [1, 2, 3] }
          PE.encodeMessage p @?= BS.pack [0x0A, 0x03, 0x01, 0x02, 0x03]
          PD.decodeMessage (PE.encodeMessage p) @?= Right p

      , testCase "decoder still accepts unpacked encoding" $ do
          -- Hand-craft an unpacked stream, assert it produces
          -- the same Vector.
          let unpacked = BS.pack
                [ 0x08, 0x01, 0x08, 0x02, 0x08, 0x03 ]
              expected = defaultPackedBag { bagNums = V.fromList [1, 2, 3] }
          PD.decodeMessage unpacked @?= Right expected

      , testCase "encoded length stays small as element count grows" $ do
          -- Strongest packed-vs-unpacked check that doesn't depend
          -- on counting tag bytes (which can collide with payload
          -- values for small ints). Unpacked would emit
          --   1 tag byte + 1 payload byte = 2 bytes per element.
          -- Packed emits
          --   1 tag byte + 1 length byte + 1 payload byte each.
          -- So 100 single-byte values should fit in well under
          -- the 200 bytes an unpacked stream would take.
          let p = defaultPackedBag { bagNums = V.fromList [1 .. 100] }
              bs = PE.encodeMessage p
          assertBool ("expected packed encoding (≤ 110 bytes), got "
                       <> show (BS.length bs))
            (BS.length bs <= 110)
      ]
  ]
