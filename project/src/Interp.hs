-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M
import Types

-- Helpers: Value <-> Int
asInt :: Value -> Int
asInt (VInt n) = n
asInt v        = error $ "Expected Int, got: " ++ show v

vBool :: Bool -> Value
vBool True  = VInt 1
vBool False = VInt 0

-- Unwind left-nested applications into (function, [args])
unwindApp :: Expr -> (Expr, [Expr])
unwindApp = go []
  where
    go args (EAp f x) = go (x:args) f
    go args f         = (f, args)

-- Top-level evaluator
evalExpr :: Core -> Env -> Expr -> Value

-- literals
evalExpr _    _   (ENum i)       = VInt i

-- constructors: Pack{t,n} evaluates to a constructor value expecting n args
-- constructors: Pack{t,n}
evalExpr _ _ (EPack t n)
  | n == 0    = VPack t []
  | otherwise = VConstr t n []

-- variables (first local env, then globals)
evalExpr prog env (EVar x) =
  case M.lookup x env of
    Just v  -> v
    Nothing ->
      case M.lookup x prog of
        -- zero-arg supercombinator: evaluate its body
        Just (_, [], body)     -> evalExpr prog env body
        -- named global function: expose as a closure (no free vars)
        Just (_, params, body) -> VClosure params body M.empty
        Nothing                -> error $ "Unbound name: " ++ x

-- lambdas -> closures capturing current env
evalExpr _ env (ELam params body) =
  VClosure params body env

-- let / letrec
evalExpr prog env (ELet isRec defs body) =
  if not isRec
    then
      let vals = [(x, evalExpr prog env rhs) | (x, rhs) <- defs]
          env' = M.union (M.fromList vals) env
      in evalExpr prog env' body
    else
      -- tie the knot so rhs can see the new names
      let env' = M.union (M.fromList [(x, evalExpr prog env' rhs) | (x, rhs) <- defs]) env
      in evalExpr prog env' body

-- application (operators, globals, closures, and constructors)
evalExpr prog env e@(EAp _ _) =
  case unwindApp e of
    -- Recognize binary operators by name with exactly two args
    (EVar op, [e1, e2]) | op `elem` ["+", "-", "*", "/"] ->
      let v1 = asInt (evalExpr prog env e1)
          v2 = asInt (evalExpr prog env e2)
      in case op of
           "+" -> VInt (v1 + v2)
           "-" -> VInt (v1 - v2)
           "*" -> VInt (v1 * v2)
           "/" -> if v2 == 0 then error "Division by zero" else VInt (v1 `div` v2)

    (EVar op, [e1, e2]) | op `elem` ["<", "<=", ">", ">=", "==", "/="] ->
      let v1 = asInt (evalExpr prog env e1)
          v2 = asInt (evalExpr prog env e2)
      in case op of
           "<"  -> vBool (v1 <  v2)
           "<=" -> vBool (v1 <= v2)
           ">"  -> vBool (v1 >  v2)
           ">=" -> vBool (v1 >= v2)
           "==" -> vBool (v1 == v2)
           "/=" -> vBool (v1 /= v2)

    (EVar op, [e1, e2]) | op `elem` ["&", "|"] ->
      let v1 = asInt (evalExpr prog env e1)
      in case op of
           "&" -> if v1 == 0
                    then VInt 0
                    else vBool (asInt (evalExpr prog env e2) /= 0)
           "|" -> if v1 /= 0
                    then VInt 1
                    else vBool (asInt (evalExpr prog env e2) /= 0)

    -- General application: evaluate function & args, then apply (supports partial/over-application)
    (funExpr, args) ->
      let funVal  = evalExpr prog env funExpr
          argVals = map (evalExpr prog env) args
      in applyMany prog funVal argVals

-- case expressions: match on Int or constructor Pack values
--   Alt format: (tag, [binders], rhs)
--     * tag >= 0  => constructor tag (or integer literal when scrutinee is an Int)
--     * tag == -1 => default ('_'), binders must be []
evalExpr prog env (ECase scrut alts) =
  case evalExpr prog env scrut of
    VInt n ->
      let matchLit ((tag, bs, rhs):rest)
            | tag >= 0 && tag == n && null bs = Just rhs
            | otherwise                       = matchLit rest
          matchLit [] = Nothing
          matchDefault = [ rhs | (tag, bs, rhs) <- alts, tag == (-1) && null bs ]
      in case matchLit alts of
           Just rhs -> evalExpr prog env rhs
           Nothing  -> case matchDefault of
                         (rhs:_) -> evalExpr prog env rhs
                         []      -> error "Non-exhaustive case on Int: no matching alternative"

    VPack tag fields ->
      let matchConstr ((t, binders, rhs):rest)
            | t == tag && length binders == length fields =
                let binds = M.fromList (zip binders fields)
                    env'  = M.union binds env
                in Just (rhs, env')
            | t == tag = error "Arity mismatch in case alternative binders"
            | otherwise = matchConstr rest
          matchConstr [] = Nothing
          matchDefault = [ (rhs, env) | (t, bs, rhs) <- alts, t == (-1) && null bs ]
      in case matchConstr alts of
           Just (rhs, env') -> evalExpr prog env' rhs
           Nothing          -> case matchDefault of
                                 ((rhs, env'):_) -> evalExpr prog env' rhs
                                 []              -> error "Non-exhaustive case on constructor: no matching alternative"

    v ->
      error $ "Case on non-scrutinee value: " ++ show v

-- Apply a function/constructor value to N arguments (handles partial application)
applyMany :: Core -> Value -> [Value] -> Value
applyMany _    v                          []       = v

-- Apply lambda/global closure
applyMany prog (VClosure ps b cEnv) args =
  case compare (length args) (length ps) of
    EQ ->
      let env' = M.union (M.fromList (zip ps args)) cEnv
      in evalExpr prog env' b
    LT ->
      let (ps1, psRest) = splitAt (length args) ps
          env'          = M.union (M.fromList (zip ps1 args)) cEnv
      in VClosure psRest b env'
    GT ->
      let (argsNow, argsRest) = splitAt (length ps) args
          env'                = M.union (M.fromList (zip ps argsNow)) cEnv
          result              = evalExpr prog env' b
      in applyMany prog result argsRest

-- Apply constructor (Pack{t,n})
applyMany _ (VConstr t n captured) args =
  let total = length captured + length args
  in case compare total n of
       EQ -> VPack t (captured ++ args)
       LT -> VConstr t n (captured ++ args)
       GT -> error "Constructor applied to too many arguments"

-- Not a function
applyMany _ v _ =
  error $ "Cannot apply non-function: " ++ show v

-- Entry point used by app/Main.hs
run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> error "Supercombinator main not defined."
    Just (_, [], mainBody) ->
      case evalExpr prog M.empty mainBody of
        VInt n -> formatResult n
        v      -> error $ "'main' must evaluate to an Int, got: " ++ show v
    Just _ -> error "'main' must take no arguments"

-- Pretty-print boolean-ish Ints the way you had before
formatResult :: Int -> String
formatResult n =
  if n == 0 || n == 1
    then if n == 0 then "/* False */" else "/* True */"
    else show n
