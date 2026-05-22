{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

-- | Wireform equivalents of flatparse's benchmark parsers.
-- These test the same parse patterns (sexp, long keyword, numeric csv,
-- lambda term) using wireform's combinator API.
module WFBasic
  ( runSexp
  , runLongws
  , runNumcsv
  , runTm
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Wireform.Parser
import Wireform.Parser.Driver (parseByteString)

import Common (Tm (..))

type P = Parser ()

------------------------------------------------------------------------
-- sexp
------------------------------------------------------------------------

ws :: P ()
ws = skipMany (satisfyAscii (\c -> c == ' ' || c == '\n') *> pure ())
{-# INLINE ws #-}

open :: P ()
open = word8 0x28 >> ws  -- '('
{-# INLINE open #-}

close :: P ()
close = word8 0x29 >> ws  -- ')'
{-# INLINE close #-}

ident :: P ()
ident = skipSome (skipSatisfyAscii isLatinLetter) >> ws
{-# INLINE ident #-}

sexp :: P ()
sexp = branch open (skipSome sexp >> close) ident

src :: P ()
src = sexp >> eof

runSexp :: ByteString -> Either (ParseError ()) ()
runSexp = parseByteString src

------------------------------------------------------------------------
-- long keyword
------------------------------------------------------------------------

longw :: P ()
longw = byteString "thisisalongkeyword"
{-# INLINE longw #-}

longws :: P ()
longws = skipSome (longw >> ws) >> eof

runLongws :: ByteString -> Either (ParseError ()) ()
runLongws = parseByteString longws

------------------------------------------------------------------------
-- numeral csv
------------------------------------------------------------------------

numeral :: P ()
numeral = skipSome (skipSatisfyAscii isDigit) >> ws
{-# INLINE numeral #-}

comma :: P ()
comma = word8 0x2C >> ws  -- ','
{-# INLINE comma #-}

numcsv :: P ()
numcsv = numeral >> skipMany (comma >> numeral) >> eof

runNumcsv :: ByteString -> Either (ParseError ()) ()
runNumcsv = parseByteString numcsv

------------------------------------------------------------------------
-- lambda term
------------------------------------------------------------------------

ident' :: P ByteString
ident' = byteStringOf (skipSome (skipSatisfyAscii \c -> isLatinLetter c || isDigit c)) <* ws
{-# INLINE ident' #-}

equal, semi, dot, addOp, mulOp, parl, parr :: P ()
equal = byteString "=" >> ws
{-# INLINE equal #-}
semi  = byteString ";" >> ws
{-# INLINE semi #-}
dot   = byteString "." >> ws
{-# INLINE dot #-}
addOp = byteString "+" >> ws
{-# INLINE addOp #-}
mulOp = byteString "*" >> ws
{-# INLINE mulOp #-}
parl  = byteString "(" >> ws
{-# INLINE parl #-}
parr  = byteString ")" >> ws
{-# INLINE parr #-}

add :: P Tm
add = chainl Add mul (addOp *> mul)

mul :: P Tm
mul = chainl Mul spine (mulOp *> spine)

spine :: P Tm
spine = chainl App atom atom

atom :: P Tm
atom =
        (Int <$> (anyAsciiDecimalInt <* ws))
    <|> (Var <$> ident')
    <|> (parl *> tm <* parr)

tm :: P Tm
tm =     (byteString "fun" *> ws *> do { x <- ident'; dot; t <- tm; pure (Lam x t) })
     <|> (byteString "let" *> ws *> do { x <- ident'; equal; t <- tm; semi; u <- tm; pure (Let x t u) })
     <|> add

runTm :: ByteString -> Either (ParseError ()) Tm
runTm = parseByteString (ws *> tm <* eof)
