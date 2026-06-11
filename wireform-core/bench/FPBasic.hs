{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FPBasic (
  runSexp,
  runLongws,
  runNumcsv,
  runTm,
) where

import Common
import Data.ByteString qualified as B
import FlatParse.Basic


ws, open, close, ident, sexp, src :: Parser () ()
ws = skipMany $(switch [|case _ of " " -> pure (); "\n" -> pure ()|])
open = $(char '(') >> ws
close = $(char ')') >> ws
ident = skipSome (skipSatisfyAscii isLatinLetter) >> ws
sexp = branch open (skipSome sexp >> close) ident
src = sexp >> eof


runSexp :: B.ByteString -> Result () ()
runSexp = runParser src


longw, longws :: Parser () ()
longw = $(string "thisisalongkeyword")
longws = skipSome (longw >> ws) >> eof


runLongws :: B.ByteString -> Result () ()
runLongws = runParser longws


numeral, comma, numcsv :: Parser () ()
numeral = skipSome (skipSatisfyAscii isDigit) >> ws
comma = $(char ',') >> ws
numcsv = numeral >> skipMany (comma >> numeral) >> eof


runNumcsv :: B.ByteString -> Result () ()
runNumcsv = runParser numcsv


ident' :: Parser () B.ByteString
ident' = byteStringOf (skipSome (skipSatisfyAscii \c -> isLatinLetter c || isDigit c)) <* ws


equal, semi, dot, addOp, mulOp, parl, parr :: Parser () ()
equal = $(string "=") <* ws
semi = $(string ";") <* ws
dot = $(string ".") <* ws
addOp = $(string "+") <* ws
mulOp = $(string "*") <* ws
parl = $(string "(") <* ws
parr = $(string ")") <* ws


add :: Parser () Tm
add = chainl Add mul (addOp *> mul)


mul :: Parser () Tm
mul = chainl Mul spine (mulOp *> spine)


spine :: Parser () Tm
spine = chainl App atom atom


atom :: Parser () Tm
atom =
  (Int <$> (anyAsciiDecimalInt <* ws))
    <|> (Var <$> ident')
    <|> (parl *> tm <* parr)


tm :: Parser () Tm
tm =
  $( switch
       [|
         case _ of
           "fun" -> do ws; x <- ident'; dot; t <- tm; pure (Lam x t)
           "let" -> do ws; x <- ident'; equal; t <- tm; semi; u <- tm; pure (Let x t u)
           _ -> add
         |]
   )


runTm :: B.ByteString -> Result () Tm
runTm = runParser (ws *> tm <* eof)
