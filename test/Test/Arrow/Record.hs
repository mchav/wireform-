{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Round-trip tests for the "Arrow.Record" combinator API and
-- the "Arrow.Record.Generic" deriver.
--
-- Covers both styles of defining a 'Table':
--
-- * Hand-written combinators ('fieldE' + 'columnD'): full
--   control over column names + encoders, can adapt newtypes via
--   'contramap' / 'fmap'.
-- * 'genericTable': zero-boilerplate default for records whose
--   field types already have 'HasEncoder' / 'HasDecoder'
--   instances.
module Test.Arrow.Record (arrowRecordTests) where

import Data.Functor.Contravariant (contramap)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Test.Syd

import qualified Arrow.Types as AT
import Arrow.Record
  ( Table
  , columnD
  , decodeTable
  , encodeTable
  , fieldE
  , int32D
  , int32E
  , int64D
  , int64E
  , nullable
  , nullableD
  , table
  , tableSchema
  , utf8D
  , utf8E
  )
import Arrow.Record.Generic
  ( HasDecoder (..)
  , HasEncoder (..)
  , genericTable
  )
import Test.Arrow.RecordTHTypes
  ( TradeTH (..)
  , TradeTHSnake (..)
  , sampleTradesTH
  , tradeTH
  , tradeTHSnake
  )

arrowRecordTests :: Spec
arrowRecordTests = describe "Arrow.Record" $ sequence_
  [ it "combinator API round-trip" $ do
      let (sch, cols) = encodeTable tradeTable sampleTrades
      case decodeTable tradeTable sch cols of
        Left  e  -> expectationFailure $ "decodeTable: " ++ e
        Right rs -> rs `shouldBe` sampleTrades

  , it "tableSchema produces the expected fields" $ do
      let sch = tableSchema tradeTable
          names = V.map (\f -> (AT.fieldName f, AT.fieldNullable f))
                        (AT.arrowFields sch)
      names `shouldBe` V.fromList
        [ ("sym",  False)
        , ("qty",  False)
        , ("note", True)
        ]

  , it "Generic deriver round-trip" $ do
      -- 'tradeGen' uses genericTable; field names come from the
      -- record selectors (tradeSym / tradeQty / tradeNote),
      -- encoders from the HasEncoder instances on their types.
      let (sch, cols) = encodeTable tradeGen sampleTradesGen
      case decodeTable tradeGen sch cols of
        Left  e  -> expectationFailure $ "decodeTable (generic): " ++ e
        Right rs -> rs `shouldBe` sampleTradesGen

  , it "TH deriveTable round-trip" $ do
      let (sch, cols) = encodeTable tradeTH sampleTradesTH
      case decodeTable tradeTH sch cols of
        Left  e  -> expectationFailure $ "TH decodeTable: " ++ e
        Right rs -> rs `shouldBe` sampleTradesTH

  , it "TH deriveTableWith renames column names" $ do
      let sch = tableSchema tradeTHSnake
          names = V.map AT.fieldName (AT.arrowFields sch)
      -- deriveTableWith (map toLower) lowercases each selector.
      names `shouldBe` V.fromList ["tradesymsnake", "tradeqtysnake", "tradenotesnake"]

  , it "newtype via contramap / fmap" $ do
      -- UserId wraps Int64 but serialises as the underlying
      -- column. The HasEncoder / HasDecoder instances below make
      -- this work with genericTable too.
      let rs = V.fromList [UserRow (UserId 1) "alice", UserRow (UserId 2) "bob"]
      let (sch, cols) = encodeTable userTable rs
      case decodeTable userTable sch cols of
        Left  e    -> expectationFailure $ "decodeTable (newtype): " ++ e
        Right rs'  -> rs' `shouldBe` rs
  ]

-- ============================================================
-- Hand-written combinator table
-- ============================================================

data Trade = Trade { sym :: !Text, qty :: !Int32, note :: !(Maybe Text) }
  deriving stock (Eq, Show)

tradeTable :: Table Trade
tradeTable = table enc dec
  where
    enc = fieldE "sym"  sym  utf8E
       <> fieldE "qty"  qty  int32E
       <> fieldE "note" note (nullable utf8E)
    dec = Trade
       <$> columnD "sym"  utf8D
       <*> columnD "qty"  int32D
       <*> columnD "note" (nullableD utf8D)

sampleTrades :: V.Vector Trade
sampleTrades = V.fromList
  [ Trade "AAPL" 100 (Just "initial")
  , Trade "GOOG"  50 Nothing
  , Trade "MSFT"  75 (Just "closed")
  ]

-- ============================================================
-- Generic-derived table
-- ============================================================

data TradeGen = TradeGen
  { tradeSym  :: !Text
  , tradeQty  :: !Int32
  , tradeNote :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)

tradeGen :: Table TradeGen
tradeGen = genericTable

sampleTradesGen :: V.Vector TradeGen
sampleTradesGen = V.fromList
  [ TradeGen "AAPL" 100 (Just "initial")
  , TradeGen "GOOG"  50 Nothing
  , TradeGen "MSFT"  75 (Just "closed")
  ]

-- ============================================================
-- Newtype / contramap demo
-- ============================================================

newtype UserId = UserId { unUserId :: Int64 }
  deriving stock (Eq, Show)

instance HasEncoder UserId where
  hasEncoder = contramap unUserId int64E
instance HasDecoder UserId where
  hasDecoder = UserId <$> int64D

data UserRow = UserRow { userId :: !UserId, userName :: !Text }
  deriving stock (Eq, Show, Generic)

userTable :: Table UserRow
userTable = genericTable

-- TH-derived tables (TradeTH + TradeTHSnake) live in
-- Test.Arrow.RecordTHTypes; see the module docs there for why.
