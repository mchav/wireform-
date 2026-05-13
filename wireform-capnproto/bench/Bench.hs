{-# LANGUAGE OverloadedStrings #-}

{- | Microbench for wireform-capnproto's encode + decode hot paths
at the dynamic 'CapnProto.Value' level.
-}
module Main (main) where

import CapnProto.Decode qualified as CPD
import CapnProto.Encode qualified as CPE
import CapnProto.Value qualified as CP
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Vector qualified as V


person :: CP.Value
person =
  CP.Struct
    (V.fromList [CP.UInt32 30]) -- data section: age
    (V.fromList [CP.Text "Alice", CP.Text "alice@example.com"])


-- pointer section: name, email

people :: CP.Value
people = CP.List $ V.generate 100 $ \i ->
  CP.Struct
    (V.fromList [CP.UInt32 (fromIntegral (20 + i `mod` 50))])
    ( V.fromList
        [ CP.Text (T.pack ("user-" <> show i))
        , CP.Text (T.pack ("user" <> show i <> "@example.com"))
        ]
    )


main :: IO ()
main =
  defaultMain
    [ bgroup
        "encode"
        [ bench "Person struct" $ nf CPE.encode person
        , bench "Person[100]" $ nf CPE.encode people
        ]
    , bgroup
        "decode"
        [ env (pure (CPE.encode person)) $ \bs ->
            bench "Person struct" $ nf (CPD.decode :: ByteString -> Either String CP.Value) bs
        , env (pure (CPE.encode people)) $ \bs ->
            bench "Person[100]" $ nf (CPD.decode :: ByteString -> Either String CP.Value) bs
        ]
    ]
