-- | Example: parse and render EDN (Extensible Data Notation).
--
-- Run with: cabal run example-edn
module Main where

import qualified Data.Vector as V
import qualified EDN.Value as E
import EDN.Encode (encode)
import EDN.Decode (decode)

main :: IO ()
main = do
  let person = E.Map $ V.fromList
        [ (E.Keyword Nothing "name", E.String "Eve")
        , (E.Keyword Nothing "age", E.Integer 35)
        , (E.Keyword Nothing "roles", E.Set $ V.fromList
            [ E.Keyword Nothing "admin"
            , E.Keyword Nothing "user"
            ])
        ]

  let text = encode person
  putStrLn $ "Rendered: " ++ show text

  case decode text of
    Right val -> putStrLn $ "Parsed: " ++ show val
    Left err  -> putStrLn $ "Error: " ++ err

  let tagged = E.Tagged "" "inst" (E.String "2024-01-15T10:30:00Z")
  putStrLn $ "Tagged: " ++ show (encode tagged)
  case decode (encode tagged) of
    Right val -> putStrLn $ "Tagged parsed: " ++ show val
    Left err  -> putStrLn $ "Tagged error: " ++ err
