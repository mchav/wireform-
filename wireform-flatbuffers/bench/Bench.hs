{-# LANGUAGE OverloadedStrings #-}

{- | Microbench for wireform-flatbuffers' encode + decode hot paths.
Operates at the dynamic 'FlatBuffers.Value' level since the
typed `View` interface is per-record-type and doesn't have a
single benchable function for an arbitrary record.
-}
module Main (main) where

import Criterion.Main
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import FlatBuffers.Decode qualified as FBD
import FlatBuffers.Encode qualified as FBE
import FlatBuffers.Value qualified as FB


person :: FB.Value
person =
  FB.VTable $
    V.fromList
      [ Just (FB.VString "Alice")
      , Just (FB.VInt32 30)
      , Just (FB.VString "alice@example.com")
      ]


-- 100 Person tables wrapped in a root container with a vector slot.
people :: FB.Value
people = FB.VTable $
  V.singleton $
    Just $
      FB.VVector $
        V.generate 100 $ \i ->
          FB.VTable $
            V.fromList
              [ Just (FB.VString (T.pack ("user-" <> show i)))
              , Just (FB.VInt32 (fromIntegral (20 + i `mod` 50)))
              , Just (FB.VString (T.pack ("user" <> show i <> "@example.com")))
              ]


main :: IO ()
main =
  defaultMain
    [ bgroup
        "encode"
        [ bench "Person table" $ nf FBE.encode person
        , bench "Person[100] vector" $ nf FBE.encode people
        ]
    , bgroup
        "decode"
        [ env (pure (FBE.encode person)) $ \bs ->
            bench "Person table" $ nf (FBD.decode :: ByteString -> Either String FB.Value) bs
        , env (pure (FBE.encode people)) $ \bs ->
            bench "Person[100] vector" $ nf (FBD.decode :: ByteString -> Either String FB.Value) bs
        ]
    ]
