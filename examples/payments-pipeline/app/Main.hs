{- |
Module      : Main
Description : CLI dispatcher for the payments gRPC + Kafka Streams demo.

Subcommands:

  * @demo@   — run the streams fan-out in-process (no broker, default).
  * @server@ — run the gRPC PaymentService, emitting events to Kafka.
  * @client@ — fire one CreatePayment call at a running server.

@
payments-pipeline demo
payments-pipeline server [PORT] [BROKER]
payments-pipeline client [HOST] [PORT]
@
-}
module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import Payments.Client (runClient)
import Payments.Demo (runDemo)
import Payments.Server (runPaymentServer)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    [] -> runDemo
    ["demo"] -> runDemo
    ("server" : rest) -> do
      let port = argInt 50051 (atMay rest 0)
          brokers = argBrokers (atMay rest 1)
      runPaymentServer port brokers
    ("client" : rest) -> do
      let host = maybe "localhost" id (atMay rest 0)
          port = argInt 50051 (atMay rest 1)
      runClient host port
    _ -> usage


usage :: IO ()
usage =
  mapM_
    putStrLn
    [ "payments-pipeline — gRPC payments -> Kafka event log -> derived views"
    , ""
    , "Usage:"
    , "  payments-pipeline demo                  in-process streams demo (no broker)"
    , "  payments-pipeline server [PORT] [BROKER]  run gRPC server (default :50051, localhost:9092)"
    , "  payments-pipeline client [HOST] [PORT]    send one CreatePayment call"
    ]


atMay :: [a] -> Int -> Maybe a
atMay xs i
  | i >= 0 && i < length xs = Just (xs !! i)
  | otherwise = Nothing


argInt :: Int -> Maybe String -> Int
argInt dflt = maybe dflt (\s -> maybe dflt id (readMaybeInt s))


readMaybeInt :: String -> Maybe Int
readMaybeInt s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing


argBrokers :: Maybe String -> [Text]
argBrokers = maybe ["localhost:9092"] (pure . T.pack)
