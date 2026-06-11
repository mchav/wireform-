{-# LANGUAGE StrictData #-}

module Common where

import Data.ByteString qualified as B


type Name = B.ByteString


data Tm
  = Var Name
  | App Tm Tm
  | Lam Name Tm
  | Let Name Tm Tm
  | Int Int
  | Add Tm Tm
  | Mul Tm Tm
  deriving (Show)
