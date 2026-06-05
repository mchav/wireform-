{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sanity.NoIsLabel (tests) where

#if !defined(TEST_NO_ISLABEL)

import Test.Syd

tests :: Spec
tests = describe "Test.Sanity.NoIsLabel" $ sequence_ [
      pendingWith "Data.ProtoLens.Labels not in scope"
        "Skipped (requires proto-lens-protobuf-types >= 0.7.2.3)"
    ]

#else

import Data.Proxy
import Data.String
import GHC.OverloadedLabels
import GHC.TypeLits

import Network.GRPC.Common.Protobuf ()
import Network.GRPC.Common.Protobuf.Any ()

-- | Too-polymorphic instance of 'IsLabel'
--
-- \"Good\" 'IsLabel' instances should have a concrete type, rather than just a
-- variable variable @a@. However, some packages (notably @lens@) define a very
-- general instance; this is problematic if @Data.ProtoLens.Labels@ is in scope.
-- We should therefore avoid importing from this module in @grapesy@, leaving
-- the choice whether or not to use @Data.ProtoLens.Labels@ to the user.
--
-- The point of this test module is to verify that no 'IsLabel' instance is in
-- scope even if we import from @Network.GRPC.Common.Protobuf@.
instance (KnownSymbol symb, IsString q) => IsLabel symb (p -> q) where
 fromLabel = \_ -> fromString (symbolVal (Proxy @symb))

tests :: Spec
tests = describe "Test.Sanity.NoIsLabel" $ sequence_ [
      it "Data.ProtoLens.Labels not in scope" thisShouldCompile
    ]

thisShouldCompile :: IO ()
thisShouldCompile =
    f 5 `shouldBe` "hi"
  where
    f :: Int -> String
    f = #hi

#endif