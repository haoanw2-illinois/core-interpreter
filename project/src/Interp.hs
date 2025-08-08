-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M
import Types



unwindApp :: Expr -> (Expr, [Expr])
unwindApp = go []
  where
    go args (EAp f x) = go (x:args) f
    go args f         = (f, args)

-- | Evaluate an expression in the context of the whole program and a local environment
evalExpr :: Core -> Env -> Expr -> Int
evalExpr _    _   (ENum i)    = i
evalExpr prog env (EVar x)    =
  case M.lookup x env of
    Just v  -> v
    Nothing ->
      case M.lookup x prog of
        Just (_, [], body) -> evalExpr prog env body
        Just (_, _ , _)    -> error $ "Function '" ++ x ++ "' applied to wrong number of arguments"
        Nothing            -> error $ "Unbound name: " ++ x

-- Function application (handles both operators and normal calls)
evalExpr prog env e@(EAp _ _) =
  case unwindApp e of
    (EVar name, args) ->
      case (name, args) of
        -- binary operators
        (op, [e1, e2]) | op `elem` ["+", "-", "*", "/"] ->
          let v1 = evalExpr prog env e1
              v2 = evalExpr prog env e2
          in case op of
               "+" -> v1 + v2
               "-" -> v1 - v2
               "*" -> v1 * v2
               "/" -> if v2 == 0 then error "Division by zero" else v1 `div` v2

        (op, [e1, e2]) | op `elem` ["<", "<=", ">", ">=", "==", "/="] ->
          let v1 = evalExpr prog env e1
              v2 = evalExpr prog env e2
          in case op of
               "<"  -> if v1 <  v2 then 1 else 0
               "<=" -> if v1 <= v2 then 1 else 0
               ">"  -> if v1 >  v2 then 1 else 0
               ">=" -> if v1 >= v2 then 1 else 0
               "==" -> if v1 == v2 then 1 else 0
               "/=" -> if v1 /= v2 then 1 else 0

        (op, [e1, e2]) | op `elem` ["&", "|"] ->
          let v1 = evalExpr prog env e1
          in case op of
               "&" -> if v1 == 0 then 0 else let v2 = evalExpr prog env e2 in if v2 /= 0 then 1 else 0
               "|" -> if v1 /= 0 then 1 else let v2 = evalExpr prog env e2 in if v2 /= 0 then 1 else 0

        -- normal function call
        _ ->
          case M.lookup name prog of
            Just (_, params, body) ->
              if length params == length args
                then
                  let argVals = map (evalExpr prog env) args
                      local   = M.fromList (zip params argVals)
                  in evalExpr prog local body
                else error $ "Function '" ++ name ++ "' applied to wrong number of arguments"
            Nothing -> error $ "Unbound function: " ++ name

    (other, _) -> error $ "Cannot apply non-function: " ++ show other


-- Placeholder cases for next phases:
-- Function application (saturated only)

-- let / letrec
evalExpr prog env (ELet isRec defs body) =
  if not isRec
    -- Non-recursive: evaluate each rhs in the current env, then extend
    then
      let vals = [(x, evalExpr prog env rhs) | (x, rhs) <- defs]
          env' = M.union (M.fromList vals) env
      in evalExpr prog env' body
    else
      -- Recursive: tie the knot (env' visible to rhs)
      let env' = M.union (M.fromList [(x, evalExpr prog env' rhs) | (x, rhs) <- defs]) env
      in evalExpr prog env' body

evalExpr _    _   (ELam _ _)  = error "Lambdas not supported in this phase"
-- case over integers; '_' encoded as tag = -1 with no binders
evalExpr prog env (ECase scrut alts) =
  let v = evalExpr prog env scrut
      -- try first literal match
      matchLit ((tag, bs, rhs):rest)
        | tag >= 0 && tag == v && null bs = Just rhs
        | otherwise                       = matchLit rest
      matchLit [] = Nothing

      -- try default: tag = -1, no binders
      matchDefault = [ rhs | (tag, bs, rhs) <- alts, tag == (-1) && null bs ]

  in case matchLit alts of
       Just rhs -> evalExpr prog env rhs
       Nothing  -> case matchDefault of
                     (rhs:_) -> evalExpr prog env rhs
                     []      -> error "Non-exhaustive case: no matching alternative"
-- | Use this function as your top-level entry point so you don't break `app/Main.hs`
run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> error "Supercombinator main not defined."
    Just (_, [], mainBody) -> formatResult (evalExpr prog M.empty mainBody)
    Just _ -> error "'main' must take no arguments"

-- | Format results based on the operation type
formatResult :: Int -> String
formatResult n = 
  case isComparisonResult n of
    True -> formatBool n
    False -> show n

-- | Helper to determine if a result is from a comparison operation
isComparisonResult :: Int -> Bool
isComparisonResult n = n == 0 || n == 1

-- | Format boolean values as True/False comments
formatBool :: Int -> String
formatBool n = if n == 0 
               then "/* False */" 
               else "/* True */"


