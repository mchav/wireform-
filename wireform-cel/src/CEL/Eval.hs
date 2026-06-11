{-# LANGUAGE BangPatterns #-}

{- | The CEL evaluator, structured as per-node /combinators/.

Each AST node compiles to a 'Compiled' closure @'Env' -> 'Either' 'CelError'
'Value'@ built from the 'Compiled' closures of its children
('compileExpr'). 'evalIn' / 'eval' just run that closure. This has two
benefits over a @case@-over-AST interpreter:

  * the AST is walked once ('compileExpr'); reuse the result to evaluate the
    same program against many environments with no re-dispatch; and
  * the combinators are exactly what "CEL.TH" emits at compile time, so the
    compile-time-compiled code and the runtime evaluator share one source of
    truth for semantics (error-absorbing @&&@/@||@, short-circuit @?:@, the
    comprehension macros, and longest-prefix name resolution).
-}
module CEL.Eval (
  eval,
  evalIn,
  compileExpr,
  Compiled,

  -- * Node combinators (used by "CEL.TH"; not generally needed directly)
  cLit,
  cName,
  cSelect,
  cIndex,
  cList,
  cMapLit,
  cStruct,
  cCond,
  cAnd,
  cOr,
  cNot,
  cNeg,
  cArith,
  cRel,
  cCall,
  cHas,
  cHasInvalid,
  cAll,
  cExists,
  cExistsOne,
  cFilter,
  cMapMacro,
  cAll2,
  cExists2,
  cExistsOne2,
  cTransformList,
  cTransformMap,
) where

import CEL.Environment
import CEL.Error
import CEL.Stdlib
import CEL.Syntax
import CEL.Value
import Control.Monad (foldM)
import Data.List (inits)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V


-- | A compiled expression: a function from the binding environment to a result.
type Compiled = Env -> Either CelError Value


-- | Evaluate an expression in the empty environment.
eval :: Expr -> Either CelError Value
eval = evalIn emptyEnv


{- | Evaluate an expression in the given environment. (Equivalent to
@'compileExpr' expr env@; reuse 'compileExpr' to avoid recompiling.)
-}
evalIn :: Env -> Expr -> Either CelError Value
evalIn env expr = compileExpr expr env


{- | Compile an expression to a reusable 'Compiled' closure by structural
recursion over the combinators.
-}
compileExpr :: Expr -> Compiled
compileExpr expr = case expr of
  ELit l -> cLit l
  EIdent root name -> cName root [name]
  ESelect e f -> case identPath (ESelect e f) of
    Just (root, segs) -> cName root segs
    Nothing -> cSelect (compileExpr e) f
  EIndex e i -> cIndex (compileExpr e) (compileExpr i)
  EList es -> cList (map compileExpr es)
  EMap entries -> cMapLit (map (\(k, v) -> (compileExpr k, compileExpr v)) entries)
  EStruct _ segs _ -> cStruct segs
  ECond c t e -> cCond (compileExpr c) (compileExpr t) (compileExpr e)
  EAnd a b -> cAnd (compileExpr a) (compileExpr b)
  EOr a b -> cOr (compileExpr a) (compileExpr b)
  ENot e -> cNot (compileExpr e)
  ENeg e -> cNeg (compileExpr e)
  EArith op a b -> cArith op (compileExpr a) (compileExpr b)
  ERel op a b -> cRel op (compileExpr a) (compileExpr b)
  ECall recv name args -> compileCall recv name args


{- | Compile a call, recognizing the @has@ and comprehension macros at
compile time (so macro dispatch is not repeated per evaluation).
-}
compileCall :: Maybe Expr -> Text -> [Expr] -> Compiled
compileCall Nothing "has" [ESelect e f] = cHas (compileExpr e) f
compileCall Nothing "has" [_] = cHasInvalid
compileCall (Just recv) name args
  | Just c <- compileMacro recv name args = c
compileCall recv name args =
  cCall (fmap compileExpr recv) name (map compileExpr args)


{- | Recognize a comprehension macro shape and compile it; 'Nothing' if the
call is an ordinary function.
-}
compileMacro :: Expr -> Text -> [Expr] -> Maybe Compiled
compileMacro recv name args = case (name, args) of
  ("all", [EIdent _ v, p]) -> Just (cAll (compileExpr recv) v (compileExpr p))
  ("exists", [EIdent _ v, p]) -> Just (cExists (compileExpr recv) v (compileExpr p))
  ("exists_one", [EIdent _ v, p]) -> Just (cExistsOne (compileExpr recv) v (compileExpr p))
  ("existsOne", [EIdent _ v, p]) -> Just (cExistsOne (compileExpr recv) v (compileExpr p))
  ("filter", [EIdent _ v, p]) -> Just (cFilter (compileExpr recv) v (compileExpr p))
  ("map", [EIdent _ v, t]) -> Just (cMapMacro (compileExpr recv) v Nothing (compileExpr t))
  ("map", [EIdent _ v, p, t]) -> Just (cMapMacro (compileExpr recv) v (Just (compileExpr p)) (compileExpr t))
  ("all", [EIdent _ a, EIdent _ b, p]) -> Just (cAll2 (compileExpr recv) a b (compileExpr p))
  ("exists", [EIdent _ a, EIdent _ b, p]) -> Just (cExists2 (compileExpr recv) a b (compileExpr p))
  ("exists_one", [EIdent _ a, EIdent _ b, p]) -> Just (cExistsOne2 (compileExpr recv) a b (compileExpr p))
  ("existsOne", [EIdent _ a, EIdent _ b, p]) -> Just (cExistsOne2 (compileExpr recv) a b (compileExpr p))
  ("transformList", [EIdent _ a, EIdent _ b, t]) -> Just (cTransformList (compileExpr recv) a b Nothing (compileExpr t))
  ("transformList", [EIdent _ a, EIdent _ b, p, t]) -> Just (cTransformList (compileExpr recv) a b (Just (compileExpr p)) (compileExpr t))
  ("transformMap", [EIdent _ a, EIdent _ b, t]) -> Just (cTransformMap (compileExpr recv) a b Nothing (compileExpr t))
  ("transformMap", [EIdent _ a, EIdent _ b, p, t]) -> Just (cTransformMap (compileExpr recv) a b (Just (compileExpr p)) (compileExpr t))
  _ -> Nothing


----------------------------------------------------------------------
-- Leaf / structural combinators
----------------------------------------------------------------------

-- | A literal (precomputed once).
cLit :: Literal -> Compiled
cLit l = let !v = literalValue l in \_ -> Right v


-- | A (possibly dotted) name resolved against the environment.
cName :: Bool -> [Text] -> Compiled
cName root segs env = resolveName env root segs


-- | Field selection on a computed value.
cSelect :: Compiled -> Text -> Compiled
cSelect e f env = e env >>= \v -> selectField v f


-- | Indexing.
cIndex :: Compiled -> Compiled -> Compiled
cIndex e i env = do
  v <- e env
  idx <- i env
  indexValue v idx


-- | List literal.
cList :: [Compiled] -> Compiled
cList cs env = VList . V.fromList <$> mapM ($ env) cs


-- | Map literal.
cMapLit :: [(Compiled, Compiled)] -> Compiled
cMapLit entries env = do
  kvs <- mapM entry entries
  case celMap kvs of
    Left m -> Left (invalidArg m)
    Right cm -> Right (VMap cm)
  where
    entry (kc, vc) = do
      k <- kc env
      validateKey k
      v <- vc env
      Right (k, v)


-- | Message/struct construction (unsupported).
cStruct :: [Text] -> Compiled
cStruct segs _ = Left (unsupported ("message construction is not supported: " <> T.intercalate "." segs))


literalValue :: Literal -> Value
literalValue = \case
  LNull -> VNull
  LBool b -> VBool b
  LInt i -> VInt i
  LUInt u -> VUInt u
  LDouble d -> VDouble d
  LString s -> VString s
  LBytes b -> VBytes b


validateKey :: Value -> Either CelError ()
validateKey v = case v of
  VInt _ -> Right ()
  VUInt _ -> Right ()
  VBool _ -> Right ()
  VString _ -> Right ()
  _ -> Left (invalidArg ("invalid map key type: " <> typeNameText (typeOf v)))


----------------------------------------------------------------------
-- Operators
----------------------------------------------------------------------

-- | Conditional @?:@ (only the taken branch is evaluated).
cCond :: Compiled -> Compiled -> Compiled -> Compiled
cCond c t e env = do
  cv <- c env
  case cv of
    VBool True -> t env
    VBool False -> e env
    _ -> Left (noOverload "_?_:_")


-- | Error-absorbing, commutative logical AND.
cAnd :: Compiled -> Compiled -> Compiled
cAnd a b env = combineAnd (asBool (a env)) (asBool (b env))


-- | Error-absorbing, commutative logical OR.
cOr :: Compiled -> Compiled -> Compiled
cOr a b env = combineOr (asBool (a env)) (asBool (b env))


cNot :: Compiled -> Compiled
cNot e env = e env >>= notValue


cNeg :: Compiled -> Compiled
cNeg e env = e env >>= negateValue


cArith :: ArithOp -> Compiled -> Compiled -> Compiled
cArith op a b env = do
  va <- a env
  vb <- b env
  arith op va vb


cRel :: RelOp -> Compiled -> Compiled -> Compiled
cRel op a b env = do
  va <- a env
  vb <- b env
  relApply op va vb


relApply :: RelOp -> Value -> Value -> Either CelError Value
relApply op va vb = case op of
  Eq -> Right (VBool (valueEq va vb))
  Ne -> Right (VBool (not (valueEq va vb)))
  In -> inOp va vb
  _ -> ordCompare op va vb


asBool :: Either CelError Value -> Either CelError Bool
asBool (Right (VBool b)) = Right b
asBool (Right _) = Left (noOverload "logical operator on non-bool")
asBool (Left e) = Left e


combineAnd :: Either CelError Bool -> Either CelError Bool -> Either CelError Value
combineAnd ea eb = case (ea, eb) of
  (Right False, _) -> Right (VBool False)
  (_, Right False) -> Right (VBool False)
  (Right True, Right True) -> Right (VBool True)
  (Left e, _) -> Left e
  (_, Left e) -> Left e


combineOr :: Either CelError Bool -> Either CelError Bool -> Either CelError Value
combineOr ea eb = case (ea, eb) of
  (Right True, _) -> Right (VBool True)
  (_, Right True) -> Right (VBool True)
  (Right False, Right False) -> Right (VBool False)
  (Left e, _) -> Left e
  (_, Left e) -> Left e


----------------------------------------------------------------------
-- Calls
----------------------------------------------------------------------

-- | Ordinary function / method call (receiver, if any, becomes the first arg).
cCall :: Maybe Compiled -> Text -> [Compiled] -> Compiled
cCall recvC name argCs env = do
  recvVals <- maybe (Right []) (\c -> (: []) <$> c env) recvC
  argVals <- mapM ($ env) argCs
  let allArgs = recvVals ++ argVals
  case lookupFunction name allArgs env of
    Just r -> r
    Nothing -> callFunction name allArgs


-- | @has(e.f)@.
cHas :: Compiled -> Text -> Compiled
cHas ec f env = do
  v <- ec env
  case v of
    VMap m -> Right (VBool (maybe False (const True) (celMapLookup (VString f) m)))
    _ -> Left (noSuchField f)


-- | @has@ applied to a non-selection argument.
cHasInvalid :: Compiled
cHasInvalid _ = Left (invalidArg "has() requires a field selection argument, e.g. has(x.y)")


----------------------------------------------------------------------
-- Comprehension macros
----------------------------------------------------------------------

rangeElems :: Value -> Either CelError [Value]
rangeElems (VList v) = Right (V.toList v)
rangeElems (VMap m) = Right (map fst (celMapEntries m))
rangeElems v = Left (noOverload ("comprehension over " <> typeNameText (typeOf v)))


rangeElems2 :: Value -> Either CelError [(Value, Value)]
rangeElems2 (VList v) = Right (zipWith (\i x -> (VInt (fromIntegral (i :: Int)), x)) [0 ..] (V.toList v))
rangeElems2 (VMap m) = Right (celMapEntries m)
rangeElems2 v = Left (noOverload ("comprehension over " <> typeNameText (typeOf v)))


cAll :: Compiled -> Text -> Compiled -> Compiled
cAll recvC var predC env = do
  v <- recvC env
  els <- rangeElems v
  go els Nothing
  where
    go [] mErr = maybe (Right (VBool True)) Left mErr
    go (el : rest) mErr = case asBool (predC (bindLocal var el env)) of
      Right False -> Right (VBool False)
      Right True -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))


cExists :: Compiled -> Text -> Compiled -> Compiled
cExists recvC var predC env = do
  v <- recvC env
  els <- rangeElems v
  go els Nothing
  where
    go [] mErr = maybe (Right (VBool False)) Left mErr
    go (el : rest) mErr = case asBool (predC (bindLocal var el env)) of
      Right True -> Right (VBool True)
      Right False -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))


cExistsOne :: Compiled -> Text -> Compiled -> Compiled
cExistsOne recvC var predC env = do
  v <- recvC env
  els <- rangeElems v
  go els (0 :: Int)
  where
    go [] n = Right (VBool (n == 1))
    go (el : rest) n = case asBool (predC (bindLocal var el env)) of
      Right True -> go rest (n + 1)
      Right False -> go rest n
      Left e -> Left e


cFilter :: Compiled -> Text -> Compiled -> Compiled
cFilter recvC var predC env = do
  v <- recvC env
  els <- rangeElems v
  VList . V.fromList <$> go els
  where
    go [] = Right []
    go (el : rest) = case asBool (predC (bindLocal var el env)) of
      Right True -> (el :) <$> go rest
      Right False -> go rest
      Left e -> Left e


cMapMacro :: Compiled -> Text -> Maybe Compiled -> Compiled -> Compiled
cMapMacro recvC var mFilter transformC env = do
  v <- recvC env
  els <- rangeElems v
  VList . V.fromList <$> go els
  where
    go [] = Right []
    go (el : rest) =
      let env' = bindLocal var el env
      in case mFilter of
           Nothing -> do r <- transformC env'; (r :) <$> go rest
           Just p -> case asBool (p env') of
             Right True -> do r <- transformC env'; (r :) <$> go rest
             Right False -> go rest
             Left e -> Left e


cAll2 :: Compiled -> Text -> Text -> Compiled -> Compiled
cAll2 recvC v1 v2 predC env = do
  v <- recvC env
  pairs <- rangeElems2 v
  go pairs Nothing
  where
    go [] mErr = maybe (Right (VBool True)) Left mErr
    go ((a, b) : rest) mErr = case asBool (predC (bind2 v1 a v2 b env)) of
      Right False -> Right (VBool False)
      Right True -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))


cExists2 :: Compiled -> Text -> Text -> Compiled -> Compiled
cExists2 recvC v1 v2 predC env = do
  v <- recvC env
  pairs <- rangeElems2 v
  go pairs Nothing
  where
    go [] mErr = maybe (Right (VBool False)) Left mErr
    go ((a, b) : rest) mErr = case asBool (predC (bind2 v1 a v2 b env)) of
      Right True -> Right (VBool True)
      Right False -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))


cExistsOne2 :: Compiled -> Text -> Text -> Compiled -> Compiled
cExistsOne2 recvC v1 v2 predC env = do
  v <- recvC env
  pairs <- rangeElems2 v
  go pairs (0 :: Int)
  where
    go [] n = Right (VBool (n == 1))
    go ((a, b) : rest) n = case asBool (predC (bind2 v1 a v2 b env)) of
      Right True -> go rest (n + 1)
      Right False -> go rest n
      Left e -> Left e


cTransformList :: Compiled -> Text -> Text -> Maybe Compiled -> Compiled -> Compiled
cTransformList recvC v1 v2 mFilter transformC env = do
  v <- recvC env
  pairs <- rangeElems2 v
  VList . V.fromList <$> go pairs
  where
    go [] = Right []
    go ((a, b) : rest) =
      let env' = bind2 v1 a v2 b env
      in case mFilter of
           Nothing -> do r <- transformC env'; (r :) <$> go rest
           Just p -> case asBool (p env') of
             Right True -> do r <- transformC env'; (r :) <$> go rest
             Right False -> go rest
             Left e -> Left e


cTransformMap :: Compiled -> Text -> Text -> Maybe Compiled -> Compiled -> Compiled
cTransformMap recvC kVar vVar mFilter transformC env = do
  v <- recvC env
  case v of
    VMap m -> do
      out <- go (celMapEntries m)
      case celMap out of
        Left msg -> Left (invalidArg msg)
        Right cm -> Right (VMap cm)
    _ -> Left (noOverload ("transformMap over " <> typeNameText (typeOf v)))
  where
    go [] = Right []
    go ((k, val) : rest) =
      let env' = bind2 kVar k vVar val env
      in case mFilter of
           Nothing -> do nv <- transformC env'; ((k, nv) :) <$> go rest
           Just p -> case asBool (p env') of
             Right True -> do nv <- transformC env'; ((k, nv) :) <$> go rest
             Right False -> go rest
             Left e -> Left e


bind2 :: Text -> Value -> Text -> Value -> Env -> Env
bind2 v1 a v2 b = bindLocal v2 b . bindLocal v1 a


----------------------------------------------------------------------
-- Name resolution
----------------------------------------------------------------------

resolveName :: Env -> Bool -> [Text] -> Either CelError Value
resolveName env root segs
  | not root
  , (s0 : rest) <- segs
  , Just v <- lookupLocal s0 env =
      foldM selectField v rest
resolveName env root segs =
  case findVarPrefix of
    Just (val, rest) -> foldM selectField val rest
    Nothing -> case lookupTypeName fullName of
      Just ty -> Right (VType ty)
      Nothing -> Left (undeclared fullName)
  where
    fullName = T.intercalate "." segs
    prefixes = if root then [[]] else containerScopes (envContainer env)
    findVarPrefix = tryK (length segs)
    tryK 0 = Nothing
    tryK k =
      let prefix = take k segs
          rest = drop k segs
          candidates = map (\cp -> T.intercalate "." (cp ++ prefix)) prefixes
      in case firstJust (map (\nm -> fmap (\val -> (val, rest)) (lookupVar nm env)) candidates) of
           Just r -> Just r
           Nothing -> tryK (k - 1)


containerScopes :: Text -> [[Text]]
containerScopes "" = [[]]
containerScopes c = reverse (inits (T.splitOn "." c))


lookupTypeName :: Text -> Maybe CelType
lookupTypeName = \case
  "int" -> Just TyInt
  "uint" -> Just TyUInt
  "double" -> Just TyDouble
  "bool" -> Just TyBool
  "string" -> Just TyString
  "bytes" -> Just TyBytes
  "list" -> Just TyList
  "map" -> Just TyMap
  "null_type" -> Just TyNull
  "type" -> Just TyType
  "google.protobuf.Timestamp" -> Just TyTimestamp
  "google.protobuf.Duration" -> Just TyDuration
  _ -> Nothing


firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : rest) = firstJust rest
