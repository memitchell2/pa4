{-# LANGUAGE InstanceSigs #-}

module Eval (
  runEval,
  TermEnv,
  Value(..),
  emptyTmenv
) where

import Syntax

import Control.Monad.Identity ( Identity(runIdentity) )
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr TermEnv
  -- TODO-1: Create a way to store arrays (VArray)
  | VArray [Value]
  -- TODO-2: Edit VClosure to store a list of patterns and expressions

type TermEnv = Map.Map String Value
type Interpreter t = Identity t

instance MonadFail Identity where
  fail :: String -> Identity a
  fail = error

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

instance Show Value where
  show :: Value -> String
  show (VInt n) = show n
  show (VBool n) = show n
  show VClosure{} = "<<closure>>"
  -- TODO-1: Show VArr
  show (VArray vals) = "[" ++ (concatMap (\v -> show v ++ ", ") vals) ++ "]"

-- TODO-2: add a checkeq function to compare literals and values
-- checkeq :: Lit -> Value -> Bool

-- TODO-2: Add a match function to handle pattern matching
-- match :: [(Pattern, Expr)] -> Value -> (Expr, TermEnv)
-- When matching against a pattern, you can check:
-- 1. Is the pattern a PVar? -> always match, this is the generic case
-- 2. Is the pattern a PLit? -> match if the argument is equivalent to the literal
-- 3. Is the pattern a (x:xs) structure? -> match if the argument is a non-empty list
-- 4. Otherwise, check another pattern


eval :: TermEnv -> Expr -> Interpreter Value
eval env expr = case expr of
  Lit (LInt k)  -> return $ VInt k
  Lit (LBool k) -> return $ VBool k

  -- TODO-1: Handle evaluating arrays
  -- Suggestion: Use a recursive call to evaluate each element one at a time
  Lit (LArray exprs) -> do
    vals <- mapM (eval env) exprs
    return (VArray vals)

  Var x -> do
    let Just v = Map.lookup x env
    return v

  -- TODO-1: Add the Cons Operator
  -- Suggestion: Create a separate handler for this case
  --             because Cons is not the same type as other binop
  Op Cons head tail -> do
    headVal <- eval env head
    tailVal <- eval env tail
    case tailVal of
      VArray vals -> return $ VArray (headVal : vals)
      _           -> error "Cons operator applied to a non-array type"

  -- TODO-CAT: Add the Concat Operator
  Op Concat left right -> do
    leftVal <- eval env left
    rightVal <- eval env right
    case (leftVal, rightVal) of
      (VArray lVals, VArray rVals) -> return $ VArray (lVals ++ rVals)
      _                             -> error "Concat operator applied to non-array types"



  Op op a b -> do
    VInt a' <- eval env a
    VInt b' <- eval env b
    return $ binop op a' b'

  Lam x body ->
    -- TODO-2: Change VClosure to store a list of patterns and expressions
    return (VClosure x body env)

  App fun arg -> do
    -- TODO-2: Implement pattern matching in App
    VClosure x body clo <- eval env fun
    argv <- eval env arg
    let nenv = Map.insert x argv clo
    eval nenv body

  Let x e body -> do
    e' <- eval env e
    let nenv = Map.insert x e' env
    eval nenv body

  If cond tr fl -> do
    VBool br <- eval env cond
    if br
    then eval env tr
    else eval env fl

  Fix e -> do
    eval env (App e (Fix e))

binop :: Binop -> Integer -> Integer -> Value
binop Add a b = VInt $ a + b
binop Mul a b = VInt $ a * b
binop Sub a b = VInt $ a - b
binop Eql a b = VBool $ a == b

-- TODO-2: Make sure that when you have a new definition for a function, you append the 
--         (pattern, body) to the environment instead of overwriting it
runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex =
  let res = runIdentity (eval env ex) in
  (res, Map.insert nm res env)
