-- | The evaluation environment: variable bindings, the resolution container
-- (protobuf package scope), and optional user-supplied extension functions.
module CEL.Environment
  ( Env (..)
  , Overload
  , emptyEnv
  , bind
  , bindAll
  , withContainer
  , addFunction
  , lookupVar
  , lookupFunction
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import CEL.Error (CelError)
import CEL.Value (Value)

-- | A user function overload: given the evaluated arguments, either decline
-- (@Nothing@, so another overload / a standard function may apply) or produce
-- a result / error.
type Overload = [Value] -> Maybe (Either CelError Value)

-- | An evaluation environment.
data Env = Env
  { envVars :: !(Map Text Value)
  -- ^ Bound variables keyed by their (possibly qualified) name.
  , envContainer :: !Text
  -- ^ The container / package used for relative name resolution. Empty for
  -- the root scope.
  , envFuncs :: !(Map Text [Overload])
  -- ^ User-supplied extension functions, tried before the standard library.
  }

-- | An environment with no bindings, no container, and no extension functions.
emptyEnv :: Env
emptyEnv = Env Map.empty "" Map.empty

-- | Bind a single variable.
bind :: Text -> Value -> Env -> Env
bind k v env = env {envVars = Map.insert k v (envVars env)}

-- | Bind several variables at once.
bindAll :: [(Text, Value)] -> Env -> Env
bindAll kvs env = env {envVars = Map.union (Map.fromList kvs) (envVars env)}

-- | Set the resolution container (protobuf package scope).
withContainer :: Text -> Env -> Env
withContainer c env = env {envContainer = c}

-- | Register an extension function overload under a name.
addFunction :: Text -> Overload -> Env -> Env
addFunction name ov env =
  env {envFuncs = Map.insertWith (++) name [ov] (envFuncs env)}

-- | Look up a bound variable by exact name.
lookupVar :: Text -> Env -> Maybe Value
lookupVar k env = Map.lookup k (envVars env)

-- | Try the registered extension functions for a name against the arguments.
lookupFunction :: Text -> [Value] -> Env -> Maybe (Either CelError Value)
lookupFunction name args env =
  case Map.lookup name (envFuncs env) of
    Nothing -> Nothing
    Just ovs -> firstJust (map ($ args) ovs)
  where
    firstJust [] = Nothing
    firstJust (Just x : _) = Just x
    firstJust (Nothing : rest) = firstJust rest
