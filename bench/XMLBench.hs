{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Control.DeepSeq (force)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- wireform XML
import qualified XML.Decode as XD
import qualified XML.SAX as XS
import qualified XML.Encode as XE
import XML.Value (Document(..))

-- xml-conduit
import qualified Text.XML as Conduit
import Text.XML (def)

-- xeno
import qualified Xeno.SAX as Xeno

-- hexml
import qualified Text.XML.Hexml as Hexml

--------------------------------------------------------------------------------
-- Test documents
--------------------------------------------------------------------------------

smallXML :: BS.ByteString
smallXML = BS8.pack $ unlines
  [ "<?xml version=\"1.0\"?>"
  , "<person>"
  , "  <name>John Doe</name>"
  , "  <age>30</age>"
  , "  <email>john@example.com</email>"
  , "  <address>"
  , "    <street>123 Main St</street>"
  , "    <city>Springfield</city>"
  , "    <state>IL</state>"
  , "  </address>"
  , "</person>"
  ]

mediumXML :: BS.ByteString
mediumXML =
  let header = "<?xml version=\"1.0\"?>\n<catalog>\n"
      footer = "</catalog>\n"
      mkItem :: Int -> String
      mkItem i = concat
        [ "  <item id=\"", show i, "\">\n"
        , "    <name>Product ", show i, "</name>\n"
        , "    <price>", show (fromIntegral i * 9.99 :: Double), "</price>\n"
        , "    <description>This is the description for product number "
        , show i, " in our catalog</description>\n"
        , "    <category>Category ", show (i `mod` 10), "</category>\n"
        , "    <inStock>", if even i then "true" else "false", "</inStock>\n"
        , "  </item>\n"
        ]
      items = concatMap mkItem [1..100 :: Int]
  in BS8.pack (header ++ items ++ footer)

--------------------------------------------------------------------------------
-- wireform DOM document (pre-built for encode benchmarks)
--------------------------------------------------------------------------------

smallDoc :: Document
smallDoc = case XD.decode smallXML of
  Right d -> d
  Left  e -> error $ "Failed to parse smallXML: " ++ e

smallConduitDoc :: Conduit.Document
smallConduitDoc = case Conduit.parseLBS def (BL.fromStrict smallXML) of
  Right d -> d
  Left  e -> error $ "Failed to parse smallXML with conduit: " ++ show e

--------------------------------------------------------------------------------
-- NOINLINE wrappers
--------------------------------------------------------------------------------

xmlDecode :: BS.ByteString -> Either String Document
xmlDecode = XD.decode
{-# NOINLINE xmlDecode #-}

xmlSAX :: BS.ByteString -> Either String (V.Vector XS.SAXEvent)
xmlSAX = XS.parseSAX
{-# NOINLINE xmlSAX #-}

xmlEncode :: Document -> BS.ByteString
xmlEncode = XE.encode
{-# NOINLINE xmlEncode #-}

conduitParse :: BS.ByteString -> Either String Conduit.Document
conduitParse bs = case Conduit.parseLBS def (BL.fromStrict bs) of
  Left  e -> Left (show e)
  Right d -> Right d
{-# NOINLINE conduitParse #-}

conduitRender :: Conduit.Document -> BL.ByteString
conduitRender = Conduit.renderLBS def
{-# NOINLINE conduitRender #-}

hexmlParse :: BS.ByteString -> Either BS.ByteString Hexml.Node
hexmlParse = Hexml.parse
{-# NOINLINE hexmlParse #-}

-- hexml Node is opaque (FFI), so we force by inspecting children
hexmlForce :: Either BS.ByteString Hexml.Node -> Int
hexmlForce (Left _) = 0
hexmlForce (Right n) = length (Hexml.children n)
{-# NOINLINE hexmlForce #-}

xenoSAX :: BS.ByteString -> Either String Int
xenoSAX bs =
  case Xeno.fold
         (\n _     -> n + 1)  -- open tag
         (\n _ _   -> n)      -- attribute (name + value)
         (\n _     -> n)      -- end open tag
         (\n _     -> n)      -- text
         (\n _     -> n + 1)  -- end tag
         (\n _     -> n)      -- cdata
         (0 :: Int)
         bs
  of
    Left  e -> Left (show e)
    Right n -> Right n
{-# NOINLINE xenoSAX #-}


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn $ "Small XML:  " ++ show (BS.length smallXML) ++ " bytes"
  putStrLn $ "Medium XML: " ++ show (BS.length mediumXML) ++ " bytes"

  -- Verify all parsers work
  case xmlDecode smallXML of
    Left e  -> putStrLn $ "WARNING: wireform DOM failed on small: " ++ e
    Right _ -> putStrLn "wireform DOM: OK (small)"
  case xmlSAX smallXML of
    Left e  -> putStrLn $ "WARNING: wireform SAX failed on small: " ++ e
    Right v -> putStrLn $ "wireform SAX: OK (small, " ++ show (V.length v) ++ " events)"
  case conduitParse smallXML of
    Left e  -> putStrLn $ "WARNING: xml-conduit failed on small: " ++ show e
    Right _ -> putStrLn "xml-conduit: OK (small)"
  case hexmlParse smallXML of
    Left e  -> putStrLn $ "WARNING: hexml failed on small: " ++ show e
    Right _ -> putStrLn "hexml: OK (small)"
  case xenoSAX smallXML of
    Left e  -> putStrLn $ "WARNING: xeno SAX failed on small: " ++ e
    Right n -> putStrLn $ "xeno SAX: OK (small, " ++ show n ++ " tags)"

  let !_ = force smallDoc
  let !_ = force smallConduitDoc

  defaultMain
    [ bgroup "Small XML"
        [ bgroup "DOM parse"
            [ bench "wireform"    $ nf xmlDecode smallXML
            , bench "xml-conduit" $ nf conduitParse smallXML
            , bench "hexml"       $ whnf (hexmlForce . hexmlParse) smallXML
            ]
        , bgroup "SAX parse"
            [ bench "wireform" $ nf xmlSAX smallXML
            , bench "xeno"     $ nf xenoSAX smallXML
            ]
        , bgroup "DOM encode"
            [ bench "wireform"    $ nf xmlEncode smallDoc
            , bench "xml-conduit" $ nf conduitRender smallConduitDoc
            ]
        ]
    , bgroup "Medium XML"
        [ bgroup "DOM parse"
            [ bench "wireform"    $ nf xmlDecode mediumXML
            , bench "xml-conduit" $ nf conduitParse mediumXML
            , bench "hexml"       $ whnf (hexmlForce . hexmlParse) mediumXML
            ]
        , bgroup "SAX parse"
            [ bench "wireform" $ nf xmlSAX mediumXML
            , bench "xeno"     $ nf xenoSAX mediumXML
            ]
        ]
    ]
