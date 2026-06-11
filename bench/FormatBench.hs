{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- wireform MessagePack

-- wireform CBOR

import CBOR.Decode qualified as CBD
import CBOR.Encode qualified as CBE
import CBOR.Value qualified as CB
-- competing: msgpack library

-- competing: cborg library

import Codec.CBOR.Encoding qualified as CBOREnc
import Codec.CBOR.Read qualified as CBORRead
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBORWrite
import Control.DeepSeq (NFData (..))
import Criterion.Main
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.MessagePack qualified as OldMP
import Data.Vector qualified as V
import MsgPack.Decode qualified as MPD
import MsgPack.Encode qualified as MPE
import MsgPack.Value qualified as MP


--------------------------------------------------------------------------------
-- wireform MessagePack test value
--------------------------------------------------------------------------------

msgpackValue :: MP.Value
msgpackValue =
  MP.Map $
    V.fromList
      [ (MP.String "name", MP.String "John Doe")
      , (MP.String "age", MP.Int 30)
      , (MP.String "active", MP.Bool True)
      , (MP.String "score", MP.Double 95.5)
      ,
        ( MP.String "tags"
        , MP.Array $
            V.fromList
              [MP.Int 1, MP.Int 2, MP.Int 3, MP.Int 4, MP.Int 5]
        )
      ]


msgpackBytes :: BS.ByteString
msgpackBytes = MPE.encode msgpackValue


--------------------------------------------------------------------------------
-- msgpack library test value (Object)
--------------------------------------------------------------------------------

oldMsgpackValue :: OldMP.Object
oldMsgpackValue =
  OldMP.ObjectMap $
    V.fromList
      [ (OldMP.ObjectStr "name", OldMP.ObjectStr "John Doe")
      , (OldMP.ObjectStr "age", OldMP.ObjectInt 30)
      , (OldMP.ObjectStr "active", OldMP.ObjectBool True)
      , (OldMP.ObjectStr "score", OldMP.ObjectDouble 95.5)
      ,
        ( OldMP.ObjectStr "tags"
        , OldMP.ObjectArray $
            V.fromList
              [ OldMP.ObjectInt 1
              , OldMP.ObjectInt 2
              , OldMP.ObjectInt 3
              , OldMP.ObjectInt 4
              , OldMP.ObjectInt 5
              ]
        )
      ]


oldMsgpackBytes :: BS.ByteString
oldMsgpackBytes = BL.toStrict $ OldMP.pack oldMsgpackValue


--------------------------------------------------------------------------------
-- wireform CBOR test value
--------------------------------------------------------------------------------

cborValue :: CB.Value
cborValue =
  CB.Map $
    V.fromList
      [ (CB.TextString "name", CB.TextString "John Doe")
      , (CB.TextString "age", CB.UInt 30)
      , (CB.TextString "active", CB.Bool True)
      , (CB.TextString "score", CB.Float64 95.5)
      ,
        ( CB.TextString "tags"
        , CB.Array $
            V.fromList
              [CB.UInt 1, CB.UInt 2, CB.UInt 3, CB.UInt 4, CB.UInt 5]
        )
      ]


cborBytes :: BS.ByteString
cborBytes = CBE.encode cborValue


--------------------------------------------------------------------------------
-- cborg library test value (Term)
--------------------------------------------------------------------------------

cborgTerm :: CBOR.Term
cborgTerm =
  CBOR.TMap
    [ (CBOR.TString "name", CBOR.TString "John Doe")
    , (CBOR.TString "age", CBOR.TInt 30)
    , (CBOR.TString "active", CBOR.TBool True)
    , (CBOR.TString "score", CBOR.TDouble 95.5)
    ,
      ( CBOR.TString "tags"
      , CBOR.TList
          [ CBOR.TInt 1
          , CBOR.TInt 2
          , CBOR.TInt 3
          , CBOR.TInt 4
          , CBOR.TInt 5
          ]
      )
    ]


cborgEncoding :: CBOREnc.Encoding
cborgEncoding = CBOR.encodeTerm cborgTerm


cborgBytes :: BS.ByteString
cborgBytes = CBORWrite.toStrictByteString cborgEncoding


--------------------------------------------------------------------------------
-- Encode/decode wrappers (NOINLINE to prevent Criterion optimizing away)
--------------------------------------------------------------------------------

mpEncode :: MP.Value -> BS.ByteString
mpEncode = MPE.encode
{-# NOINLINE mpEncode #-}


mpDecode :: BS.ByteString -> Either String MP.Value
mpDecode = MPD.decode
{-# NOINLINE mpDecode #-}


oldMpPack :: OldMP.Object -> BL.ByteString
oldMpPack = OldMP.pack
{-# NOINLINE oldMpPack #-}


oldMpUnpack :: BL.ByteString -> Maybe OldMP.Object
oldMpUnpack = OldMP.unpack
{-# NOINLINE oldMpUnpack #-}


cbEncode :: CB.Value -> BS.ByteString
cbEncode = CBE.encode
{-# NOINLINE cbEncode #-}


cbDecode :: BS.ByteString -> Either String CB.Value
cbDecode = CBD.decode
{-# NOINLINE cbDecode #-}


cborgEncode :: CBOR.Term -> BS.ByteString
cborgEncode = CBORWrite.toStrictByteString . CBOR.encodeTerm
{-# NOINLINE cborgEncode #-}


cborgDecode :: BS.ByteString -> Either CBORRead.DeserialiseFailure (BL.ByteString, CBOR.Term)
cborgDecode = CBORRead.deserialiseFromBytes CBOR.decodeTerm . BL.fromStrict
{-# NOINLINE cborgDecode #-}


--------------------------------------------------------------------------------
-- NFData orphan for cborg Term (needed by Criterion)
--------------------------------------------------------------------------------

instance NFData CBOR.Term where
  rnf (CBOR.TInt i) = rnf i
  rnf (CBOR.TInteger i) = rnf i
  rnf (CBOR.TBytes bs) = rnf bs
  rnf (CBOR.TBytesI bs) = rnf bs
  rnf (CBOR.TString t) = rnf t
  rnf (CBOR.TStringI t) = rnf t
  rnf (CBOR.TList xs) = rnf xs
  rnf (CBOR.TListI xs) = rnf xs
  rnf (CBOR.TMap xs) = rnf xs
  rnf (CBOR.TMapI xs) = rnf xs
  rnf (CBOR.TTagged w t) = rnf w `seq` rnf t
  rnf (CBOR.TBool b) = rnf b
  rnf CBOR.TNull = ()
  rnf (CBOR.TSimple w) = rnf w
  rnf (CBOR.THalf f) = rnf f
  rnf (CBOR.TFloat f) = rnf f
  rnf (CBOR.TDouble d) = rnf d


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn $ "MessagePack payload: " ++ show (BS.length msgpackBytes) ++ " bytes (wireform)"
  let !oldMsgpackBytesLazy = BL.fromStrict oldMsgpackBytes
  putStrLn $ "MessagePack payload: " ++ show (BS.length oldMsgpackBytes) ++ " bytes (msgpack)"
  putStrLn $ "CBOR payload:        " ++ show (BS.length cborBytes) ++ " bytes (wireform)"
  putStrLn $ "CBOR payload:        " ++ show (BS.length cborgBytes) ++ " bytes (cborg)"

  defaultMain
    [ bgroup
        "MessagePack"
        [ bgroup
            "encode"
            [ bench "wireform" $ nf mpEncode msgpackValue
            , bench "msgpack" $ nf oldMpPack oldMsgpackValue
            ]
        , bgroup
            "decode"
            [ bench "wireform" $ nf mpDecode msgpackBytes
            , bench "msgpack" $ nf oldMpUnpack oldMsgpackBytesLazy
            ]
        ]
    , bgroup
        "CBOR"
        [ bgroup
            "encode"
            [ bench "wireform" $ nf cbEncode cborValue
            , bench "cborg" $ nf cborgEncode cborgTerm
            ]
        , bgroup
            "decode"
            [ bench "wireform" $ nf cbDecode cborBytes
            , bench "cborg" $ nf cborgDecode cborgBytes
            ]
        ]
    ]
