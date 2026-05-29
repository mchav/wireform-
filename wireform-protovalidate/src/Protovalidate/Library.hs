{-# LANGUAGE OverloadedStrings #-}

-- | The protovalidate CEL extension library.
--
-- Reference protovalidate implementations extend the base CEL environment with
-- a handful of functions that the standard constraints (and user-written
-- custom constraints) depend on. This module registers the same set onto a
-- 'CEL.Environment.Env' via 'CEL.Environment.addFunction':
--
--   * @isNan(double) -> bool@, @isInf(double) -> bool@, @isInf(double, int) -> bool@
--   * @unique(list) -> bool@ (and the @list.unique()@ receiver form)
--   * @string.isHostname() -> bool@, @string.isEmail() -> bool@
--   * @string.isHostAndPort(bool) -> bool@
--   * @string.isIp() -> bool@, @string.isIp(int) -> bool@
--   * @string.isIpPrefix(...) -> bool@ (optional version and \"strict\" args)
--   * @string.isUri() -> bool@, @string.isUriRef() -> bool@
module Protovalidate.Library
  ( withLibrary
  , libraryEnv
  ) where

import Data.Text (Text)
import qualified Data.Vector as V

import CEL.Environment (Env, Overload, addFunction, emptyEnv)
import CEL.Error (CelError)
import CEL.Value (Value (..), valueEq)
import qualified Protovalidate.Format as F

-- | A CEL environment containing only the protovalidate extension functions.
libraryEnv :: Env
libraryEnv = withLibrary emptyEnv

-- | Register every protovalidate extension function onto an environment.
withLibrary :: Env -> Env
withLibrary env = foldr ($) env registrations
  where
    registrations =
      [ addFunction "isNan" ovIsNan
      , addFunction "isInf" ovIsInf
      , addFunction "unique" ovUnique
      , addFunction "isHostname" (strBool F.isHostname)
      , addFunction "isEmail" (strBool F.isEmail)
      , addFunction "isUri" (strBool F.isUri)
      , addFunction "isUriRef" (strBool F.isUriRef)
      , addFunction "isHostAndPort" ovHostAndPort
      , addFunction "isIp" ovIsIp
      , addFunction "isIpPrefix" ovIsIpPrefix
      ]

ok :: Bool -> Maybe (Either CelError Value)
ok = Just . Right . VBool

-- | A single-argument @string -> bool@ predicate (receiver-style).
strBool :: (Text -> Bool) -> Overload
strBool f args = case args of
  [VString s] -> ok (f s)
  _ -> Nothing

ovIsNan :: Overload
ovIsNan args = case args of
  [VDouble d] -> ok (isNaN d)
  _ -> Nothing

ovIsInf :: Overload
ovIsInf args = case args of
  [VDouble d] -> ok (isInfinite d)
  [VDouble d, VInt sign] -> ok (matchInf d sign)
  _ -> Nothing
  where
    matchInf d sign
      | not (isInfinite d) = False
      | sign > 0 = d > 0
      | sign < 0 = d < 0
      | otherwise = True

ovUnique :: Overload
ovUnique args = case args of
  [VList xs] -> ok (allUnique (V.toList xs))
  _ -> Nothing
  where
    allUnique [] = True
    allUnique (x : rest) = not (any (valueEq x) rest) && allUnique rest

ovHostAndPort :: Overload
ovHostAndPort args = case args of
  [VString s, VBool portReq] -> ok (F.isHostAndPort s portReq)
  _ -> Nothing

ovIsIp :: Overload
ovIsIp args = case args of
  [VString s] -> ok (F.isIp Nothing s)
  [VString s, VInt v] -> ok (F.isIp (Just (fromIntegral v)) s)
  _ -> Nothing

ovIsIpPrefix :: Overload
ovIsIpPrefix args = case args of
  [VString s] -> ok (F.isIpPrefix Nothing False s)
  [VString s, VInt v] -> ok (F.isIpPrefix (Just (fromIntegral v)) False s)
  [VString s, VBool strict] -> ok (F.isIpPrefix Nothing strict s)
  [VString s, VInt v, VBool strict] -> ok (F.isIpPrefix (Just (fromIntegral v)) strict s)
  _ -> Nothing
