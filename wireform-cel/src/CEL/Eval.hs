{-# LANGUAGE BangPatterns #-}

-- | The CEL evaluator.
--
-- Implements expression evaluation over the 'Env' binding context, including:
--
--   * literals, variables, and (longest-prefix, container-aware) name
--     resolution;
--   * field selection and indexing;
--   * the commutative, error-absorbing logical operators @&&@ / @||@ and the
--     short-circuiting conditional @?:@ (see "Logical Operators" in the spec);
--   * the comprehension macros @has@, @all@, @exists@, @exists_one@, @map@
--     (3- and 4-argument forms), and @filter@;
--   * dispatch of operators and functions to "CEL.Stdlib" and to
--     user-registered extension functions.
module CEL.Eval
  ( eval
  , evalIn
  ) where

import Control.Monad (foldM)
import Data.List (inits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import CEL.Environment
import CEL.Error
import CEL.Stdlib
import CEL.Syntax
import CEL.Value

-- | Evaluate an expression in the empty environment.
eval :: Expr -> Either CelError Value
eval = evalIn emptyEnv

-- | Evaluate an expression in the given environment.
evalIn :: Env -> Expr -> Either CelError Value
evalIn env expr = case expr of
  ELit lit -> Right (literalValue lit)
  EIdent root name -> resolveName env root [name]
  ESelect e f -> case identPath (ESelect e f) of
    Just (root, segs) -> resolveName env root segs
    Nothing -> do
      v <- evalIn env e
      selectField v f
  EIndex e i -> do
    v <- evalIn env e
    idx <- evalIn env i
    indexValue v idx
  EList es -> do
    vs <- mapM (evalIn env) es
    Right (VList (V.fromList vs))
  EMap entries -> do
    kvs <- mapM evalEntry entries
    case celMap kvs of
      Left m -> Left (invalidArg m)
      Right cm -> Right (VMap cm)
    where
      evalEntry (ke, ve) = do
        k <- evalIn env ke
        validateKey k
        v <- evalIn env ve
        Right (k, v)
  EStruct _ segs _ ->
    Left (unsupported ("message construction is not supported: " <> T.intercalate "." segs))
  ECond c t e -> do
    cv <- evalIn env c
    case cv of
      VBool True -> evalIn env t
      VBool False -> evalIn env e
      _ -> Left (noOverload "_?_:_")
  EAnd a b -> evalAnd (asBool (evalIn env a)) (asBool (evalIn env b))
  EOr a b -> evalOr (asBool (evalIn env a)) (asBool (evalIn env b))
  ENot e -> do v <- evalIn env e; notValue v
  ENeg e -> do v <- evalIn env e; negateValue v
  EArith op a b -> do
    va <- evalIn env a
    vb <- evalIn env b
    arith op va vb
  ERel op a b -> evalRel env op a b
  ECall recv name args -> evalCall env recv name args

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
-- Logical operators (commutative, error-absorbing)
----------------------------------------------------------------------

asBool :: Either CelError Value -> Either CelError Bool
asBool (Right (VBool b)) = Right b
asBool (Right _) = Left (noOverload "logical operator on non-bool")
asBool (Left e) = Left e

evalAnd :: Either CelError Bool -> Either CelError Bool -> Either CelError Value
evalAnd ea eb = case (ea, eb) of
  (Right False, _) -> Right (VBool False)
  (_, Right False) -> Right (VBool False)
  (Right True, Right True) -> Right (VBool True)
  (Left e, _) -> Left e
  (_, Left e) -> Left e

evalOr :: Either CelError Bool -> Either CelError Bool -> Either CelError Value
evalOr ea eb = case (ea, eb) of
  (Right True, _) -> Right (VBool True)
  (_, Right True) -> Right (VBool True)
  (Right False, Right False) -> Right (VBool False)
  (Left e, _) -> Left e
  (_, Left e) -> Left e

----------------------------------------------------------------------
-- Relational operators
----------------------------------------------------------------------

evalRel :: Env -> RelOp -> Expr -> Expr -> Either CelError Value
evalRel env op a b = do
  va <- evalIn env a
  vb <- evalIn env b
  case op of
    Eq -> Right (VBool (valueEq va vb))
    Ne -> Right (VBool (not (valueEq va vb)))
    In -> inOp va vb
    _ -> ordCompare op va vb

----------------------------------------------------------------------
-- Calls and macros
----------------------------------------------------------------------

evalCall :: Env -> Maybe Expr -> Text -> [Expr] -> Either CelError Value
evalCall env Nothing "has" [arg] = evalHas env arg
evalCall env (Just recv) name args
  | Just r <- comprehensionMacro env recv name args = r
evalCall env recv name args = do
  recvVals <- maybe (Right []) (\e -> (: []) <$> evalIn env e) recv
  argVals <- mapM (evalIn env) args
  let allArgs = recvVals ++ argVals
  case lookupFunction name allArgs env of
    Just r -> r
    Nothing -> callFunction name allArgs

-- | The @has(e.f)@ macro.
evalHas :: Env -> Expr -> Either CelError Value
evalHas env (ESelect e f) = do
  v <- evalIn env e
  case v of
    VMap m -> Right (VBool (maybe False (const True) (celMapLookup (VString f) m)))
    _ -> Left (noSuchField f)
evalHas _ _ = Left (invalidArg "has() requires a field selection argument, e.g. has(x.y)")

-- | Recognize and evaluate the comprehension macros. Returns 'Nothing' if the
-- call does not match a macro shape, so it can fall through to normal function
-- dispatch.
comprehensionMacro :: Env -> Expr -> Text -> [Expr] -> Maybe (Either CelError Value)
comprehensionMacro env recv name args = case (name, args) of
  ("all", [EIdent _ var, p]) -> Just (macroAll env recv var p)
  ("exists", [EIdent _ var, p]) -> Just (macroExists env recv var p)
  ("exists_one", [EIdent _ var, p]) -> Just (macroExistsOne env recv var p)
  ("filter", [EIdent _ var, p]) -> Just (macroFilter env recv var p)
  ("map", [EIdent _ var, t]) -> Just (macroMap env recv var Nothing t)
  ("map", [EIdent _ var, p, t]) -> Just (macroMap env recv var (Just p) t)
  -- Two-variable comprehension macros (cel-spec "macros2"). For lists the
  -- first variable is the 0-based index and the second the element; for maps
  -- they are the key and the value.
  ("existsOne", [EIdent _ var, p]) -> Just (macroExistsOne env recv var p)
  ("all", [EIdent _ a, EIdent _ b, p]) -> Just (macroAll2 env recv a b p)
  ("exists", [EIdent _ a, EIdent _ b, p]) -> Just (macroExists2 env recv a b p)
  ("exists_one", [EIdent _ a, EIdent _ b, p]) -> Just (macroExistsOne2 env recv a b p)
  ("existsOne", [EIdent _ a, EIdent _ b, p]) -> Just (macroExistsOne2 env recv a b p)
  ("transformList", [EIdent _ a, EIdent _ b, t]) -> Just (macroTransformList env recv a b Nothing t)
  ("transformList", [EIdent _ a, EIdent _ b, p, t]) -> Just (macroTransformList env recv a b (Just p) t)
  ("transformMap", [EIdent _ a, EIdent _ b, t]) -> Just (macroTransformMap env recv a b Nothing t)
  ("transformMap", [EIdent _ a, EIdent _ b, p, t]) -> Just (macroTransformMap env recv a b (Just p) t)
  _ -> Nothing

-- | Extract the elements a comprehension iterates over: list elements, or the
-- keys of a map.
rangeElems :: Value -> Either CelError [Value]
rangeElems (VList v) = Right (V.toList v)
rangeElems (VMap m) = Right (map fst (celMapEntries m))
rangeElems v = Left (noOverload ("comprehension over " <> typeNameText (typeOf v)))

-- | Two-variable iteration: @(index, element)@ for lists, @(key, value)@ for
-- maps.
rangeElems2 :: Value -> Either CelError [(Value, Value)]
rangeElems2 (VList v) = Right (zipWith (\i x -> (VInt (fromIntegral (i :: Int)), x)) [0 ..] (V.toList v))
rangeElems2 (VMap m) = Right (celMapEntries m)
rangeElems2 v = Left (noOverload ("comprehension over " <> typeNameText (typeOf v)))

evalPred :: Env -> Text -> Expr -> Value -> Either CelError Value
evalPred env var p el = evalIn (bindLocal var el env) p

evalPred2 :: Env -> Text -> Text -> Expr -> Value -> Value -> Either CelError Value
evalPred2 env v1 v2 p a b = evalIn (bindLocal v2 b (bindLocal v1 a env)) p

macroAll :: Env -> Expr -> Text -> Expr -> Either CelError Value
macroAll env recv var p = do
  v <- evalIn env recv
  els <- rangeElems v
  go els Nothing
  where
    go [] mErr = case mErr of
      Just e -> Left e
      Nothing -> Right (VBool True)
    go (el : rest) mErr = case asBool (evalPred env var p el) of
      Right False -> Right (VBool False)
      Right True -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))

macroExists :: Env -> Expr -> Text -> Expr -> Either CelError Value
macroExists env recv var p = do
  v <- evalIn env recv
  els <- rangeElems v
  go els Nothing
  where
    go [] mErr = case mErr of
      Just e -> Left e
      Nothing -> Right (VBool False)
    go (el : rest) mErr = case asBool (evalPred env var p el) of
      Right True -> Right (VBool True)
      Right False -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))

macroExistsOne :: Env -> Expr -> Text -> Expr -> Either CelError Value
macroExistsOne env recv var p = do
  v <- evalIn env recv
  els <- rangeElems v
  go els (0 :: Int)
  where
    go [] n = Right (VBool (n == 1))
    go (el : rest) n = case asBool (evalPred env var p el) of
      Right True -> go rest (n + 1)
      Right False -> go rest n
      Left e -> Left e

macroFilter :: Env -> Expr -> Text -> Expr -> Either CelError Value
macroFilter env recv var p = do
  v <- evalIn env recv
  els <- rangeElems v
  kept <- go els
  Right (VList (V.fromList kept))
  where
    go [] = Right []
    go (el : rest) = case asBool (evalPred env var p el) of
      Right True -> (el :) <$> go rest
      Right False -> go rest
      Left e -> Left e

macroMap :: Env -> Expr -> Text -> Maybe Expr -> Expr -> Either CelError Value
macroMap env recv var mFilter t = do
  v <- evalIn env recv
  els <- rangeElems v
  out <- go els
  Right (VList (V.fromList out))
  where
    go [] = Right []
    go (el : rest) = case mFilter of
      Nothing -> do
        r <- evalPred env var t el
        (r :) <$> go rest
      Just p -> case asBool (evalPred env var p el) of
        Right True -> do r <- evalPred env var t el; (r :) <$> go rest
        Right False -> go rest
        Left e -> Left e

macroAll2 :: Env -> Expr -> Text -> Text -> Expr -> Either CelError Value
macroAll2 env recv v1 v2 p = do
  v <- evalIn env recv
  pairs <- rangeElems2 v
  go pairs Nothing
  where
    go [] mErr = maybe (Right (VBool True)) Left mErr
    go ((a, b) : rest) mErr = case asBool (evalPred2 env v1 v2 p a b) of
      Right False -> Right (VBool False)
      Right True -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))

macroExists2 :: Env -> Expr -> Text -> Text -> Expr -> Either CelError Value
macroExists2 env recv v1 v2 p = do
  v <- evalIn env recv
  pairs <- rangeElems2 v
  go pairs Nothing
  where
    go [] mErr = maybe (Right (VBool False)) Left mErr
    go ((a, b) : rest) mErr = case asBool (evalPred2 env v1 v2 p a b) of
      Right True -> Right (VBool True)
      Right False -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))

macroExistsOne2 :: Env -> Expr -> Text -> Text -> Expr -> Either CelError Value
macroExistsOne2 env recv v1 v2 p = do
  v <- evalIn env recv
  pairs <- rangeElems2 v
  go pairs (0 :: Int)
  where
    go [] n = Right (VBool (n == 1))
    go ((a, b) : rest) n = case asBool (evalPred2 env v1 v2 p a b) of
      Right True -> go rest (n + 1)
      Right False -> go rest n
      Left e -> Left e

macroTransformList :: Env -> Expr -> Text -> Text -> Maybe Expr -> Expr -> Either CelError Value
macroTransformList env recv v1 v2 mFilter t = do
  v <- evalIn env recv
  pairs <- rangeElems2 v
  out <- go pairs
  Right (VList (V.fromList out))
  where
    go [] = Right []
    go ((a, b) : rest) = case mFilter of
      Nothing -> do r <- evalPred2 env v1 v2 t a b; (r :) <$> go rest
      Just p -> case asBool (evalPred2 env v1 v2 p a b) of
        Right True -> do r <- evalPred2 env v1 v2 t a b; (r :) <$> go rest
        Right False -> go rest
        Left e -> Left e

macroTransformMap :: Env -> Expr -> Text -> Text -> Maybe Expr -> Expr -> Either CelError Value
macroTransformMap env recv kVar vVar mFilter t = do
  v <- evalIn env recv
  case v of
    VMap m -> do
      out <- go (celMapEntries m)
      case celMap out of
        Left msg -> Left (invalidArg msg)
        Right cm -> Right (VMap cm)
    _ -> Left (noOverload ("transformMap over " <> typeNameText (typeOf v)))
  where
    go [] = Right []
    go ((k, val) : rest) = case mFilter of
      Nothing -> do nv <- evalPred2 env kVar vVar t k val; ((k, nv) :) <$> go rest
      Just p -> case asBool (evalPred2 env kVar vVar p k val) of
        Right True -> do nv <- evalPred2 env kVar vVar t k val; ((k, nv) :) <$> go rest
        Right False -> go rest
        Left e -> Left e

----------------------------------------------------------------------
-- Name resolution
----------------------------------------------------------------------

resolveName :: Env -> Bool -> [Text] -> Either CelError Value
resolveName env root segs
  -- Comprehension-local variables take precedence over all package-based
  -- resolution, matched on the leading identifier. A leading '.' (root) skips
  -- locals and resolves in the global/package scope.
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
