module APL.Eval
  ( eval,
    envEmpty,
    VName,
    Val (..),
  )
where

import APL.AST (Exp (..), VName)
import APL.Monad

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  | ValSet [Val]
  deriving (Eq, Ord, Show)

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

evalIntBinOp :: Env -> (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp env f e1 e2 = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: Env -> (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' env f e1 e2 =
  evalIntBinOp env f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Env -> Exp -> EvalM Val
eval _ (CstInt x) = pure $ ValInt x
eval _ (CstBool b) = pure $ ValBool b
eval env (Var v) = do
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval env (Add e1 e2) = evalIntBinOp' env (+) e1 e2
eval env (Sub e1 e2) = evalIntBinOp' env (-) e1 e2
eval env (Mul e1 e2) = evalIntBinOp' env (*) e1 e2
eval env (Div e1 e2) = evalIntBinOp env checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval env (Pow e1 e2) = evalIntBinOp env checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval env (Eql e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval env (If cond e1 e2) = do
  cond' <- eval env cond
  case cond' of
    ValBool True -> eval env e1
    ValBool False -> eval env e2
    _ -> failure "Non-boolean conditional."
eval env (Let var e1 e2) = do
  v1 <- eval env e1
  eval (envExtend var v1 env) e2
eval env (ForLoop (loopparam, initial) (iv, bound) body) = do
  initial_v <- eval env initial
  bound_v <- eval env bound
  case bound_v of
    ValInt bound_int ->
      loop 0 bound_int initial_v
    _ ->
      failure "Non-integral loop bound"
  where
    loop i bound_int loop_v
      | i >= bound_int = pure loop_v
      | otherwise = do
          let env' = envExtend iv (ValInt i) $ envExtend loopparam loop_v env
          loop_v' <- eval env' body
          loop (succ i) bound_int loop_v'
eval env (Lambda var body) =
  pure $ ValFun env var body
eval env (Apply e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      eval (envExtend var arg f_env) body
    (_, _) ->
      failure "Cannot apply non-function"


eval env (Set exps) = do
  vals <- mapM (eval env) exps
  pure $ ValSet vals
eval env (Union e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (ValSet s1, ValSet s2) ->
      pure $ ValSet (s1 ++ s2)
    (_, _) ->
      failure "Union applied to non-sets"
eval env (Without e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (ValSet s1, ValSet s2) ->
      pure $ ValSet (filter (`notElem` s2) s1)
    (_, _) ->
      failure "Without applied to non-sets"
eval env (Elem e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case v2 of
    ValSet s2 ->
      pure $ ValBool (v1 `elem` s2)
    _ ->
      failure "Elem applied to non-set"


eval _ Meet =
  ValInt . fromIntegral <$> meet
eval _ Groom =
  ValBool <$> groom
eval _ Ignore =
  ValBool <$> ignore
