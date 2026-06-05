{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client.SerdeContextSpec (tests) where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Test.Syd

import qualified Kafka.Client.SerdeContext as SC

tests :: Spec
tests = describe "SerdeContext (KIP-492)" $ sequence_
  [ it "liftSerdeCtx ignores the context"
      lift_ignores
  , it "context-aware encoder sees topic"
      context_visible
  , it "withTopic / withHeaders helpers"
      builders
  ]

lift_ignores :: IO ()
lift_ignores = do
  let s = SC.liftSerdeCtx
            (\(x :: Int) -> if x == 1 then "1" else "0")
            (\bs -> Right (BS.length bs))
      ctx = SC.withTopic "t" False
  SC.csSerialize s ctx (1 :: Int) `shouldBe` "1"
  SC.csDeserialize s ctx "12345"  `shouldBe` Right 5

context_visible :: IO ()
context_visible = do
  let s :: SC.CtxSerde Int
      s = SC.CtxSerde
        { SC.csSerialize = \c _ -> TE.encodeUtf8 (SC.scTopic c)
        , SC.csDeserialize = \c _bs -> Right (BS.length (TE.encodeUtf8 (SC.scTopic c)))
        }
      ctx = SC.withTopic "events" True
  SC.csSerialize s ctx (42 :: Int) `shouldBe` TE.encodeUtf8 "events"
  case SC.csDeserialize s ctx "raw" of
    Right n  -> n `shouldBe` BS.length (TE.encodeUtf8 "events")
    Left err -> error err

builders :: IO ()
builders = do
  let ctx = SC.withTopic "x" True
  SC.scTopic ctx     `shouldBe` "x"
  SC.scIsKey ctx     `shouldBe` True
  let ctx2 = SC.withHeaders ctx [("k", "v")]
  SC.scHeaders ctx2  `shouldBe` [("k", "v")]
