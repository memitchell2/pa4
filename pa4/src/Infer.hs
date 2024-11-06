{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Infer where

import Prelude hiding (foldr)

import Type
import Syntax
import Debug.Trace (trace)

import Control.Monad.State
    ( MonadState(put, get), foldM, replicateM, evalState, State )
import Control.Monad.Except
    ( MonadError(throwError), runExceptT, ExceptT )

import Data.List (nub)
import Data.Foldable (foldr)
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype TypeEnv = TypeEnv (Map.Map Var Scheme)
  deriving (Monoid, Semigroup)

newtype Unique = Unique { count :: Int }

type Infer = ExceptT TypeError (State Unique)
type Subst = Map.Map TVar Type

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique { count = 0 }

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> Var -> Maybe Type.Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

-- TODO-1: Implement the Substitutable instance for Type 
--         (all cases are done except TArray)
instance Substitutable Type where
  apply :: Subst -> Type -> Type
  apply s t@(TCon a) = trace ("Applying substitution to TCon: " ++ show t ++ " with " ++ show s) t
  apply s t@(TVar a) = trace ("Applying substitution to TVar: " ++ show t ++ " with " ++ show s ++ ", result: " ++ show (Map.findWithDefault t a s)) $ Map.findWithDefault t a s
  apply s (t1 `TArrow` t2) = trace ("Applying substitution to TArrow with " ++ show s) $ apply s t1 `TArrow` apply s t2
  -- TODO-1: apply a substitution to an array
  apply s (TArray t) = trace ("Applying substitution to TArray with " ++ show s ++ " for element type " ++ show t) $ TArray (apply s t)

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArrow` t2) = ftv t1 `Set.union` ftv t2
  -- TODO-1: calculate the free variables of an array
  ftv (TArray t) = ftv t

instance Substitutable Scheme where
  apply :: Subst -> Scheme -> Scheme
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv :: Scheme -> Set.Set TVar
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply :: Substitutable a => Subst -> [a] -> [a]
  apply = fmap . apply
  ftv :: Substitutable a => [a] -> Set.Set TVar
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply :: Subst -> TypeEnv -> TypeEnv
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv :: TypeEnv -> Set.Set TVar
  ftv (TypeEnv env) = ftv $ Map.elems env


nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = trace ("Composing substitutions: " ++ show s1 ++ " with " ++ show s2) $
                Map.map (apply s1) s2 `Map.union` s1

unify ::  Type -> Type -> Infer Subst
unify (l `TArrow` r) (l' `TArrow` r') = do
  trace ("Unifying arrow types: " ++ show l ++ " with " ++ show l') (return ())
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  trace ("Resulting substitution in arrow types: s1=" ++ show s1 ++ ", s2=" ++ show s2) (return ())
  return (s2 `compose` s1)

unify (TVar a) t = trace ("Unifying TVar " ++ show a ++ " with type " ++ show t) $ bind a t
unify t (TVar a) = trace ("Unifying type " ++ show t ++ " with TVar " ++ show a) $ bind a t
unify (TCon a) (TCon b) | a == b = trace ("Unifying matching TCons " ++ show a ++ " and " ++ show b) $ return nullSubst
-- TODO-1: Unify the TArray type
unify (TArray t1) (TArray t2) = do
  s <- unify t1 t2
  return s
unify t1 t2 = trace ("Unification failed for: " ++ show t1 ++ " with " ++ show t2) $
              throwError $ UnificationFail t1 t2

bind :: TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = trace ("Binding " ++ show a ++ " to itself, no substitution") $ return nullSubst
  | occursCheck a t = trace ("Infinite type detected when trying to bind " ++ show a ++ " to " ++ show t) $ throwError $ InfiniteType a t
  | otherwise       = trace ("Binding " ++ show a ++ " to type " ++ show t) $ return $ Map.singleton a t

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Binop -> Type
ops Add = typeInt `TArrow` typeInt `TArrow` typeInt
ops Mul = typeInt `TArrow` typeInt `TArrow` typeInt
ops Sub = typeInt `TArrow` typeInt `TArrow` typeInt
ops Eql = typeInt `TArrow` typeInt `TArrow` typeBool

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of

  Var x -> lookupEnv env x

  -- TODO-2: Handle the different pattern values of `x`
  --         Each has its own implications for typing
  Lam x e -> do
    tv <- fresh
    let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArrow` t1)

  App e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3       <- unify (apply s2 t1) (TArrow t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t'   = generalize env' t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s2 `compose` s1, t2)

  If cond tr fl -> do
    tv <- fresh
    inferPrim env [cond, tr, fl] (typeBool `TArrow` tv `TArrow` tv `TArrow` tv)

  Fix e1 -> do
    tv <- fresh
    inferPrim env [e1] ((tv `TArrow` tv) `TArrow` tv)

  -- TODO-1: Handle the Cons operator
  -- Suggestion: Separate this from the other ops because the constraint
  --             is more generic than the other ops
  Op Cons head tail -> do
    -- Infer the types of `head` (element) and `tail` (list)
    (s1, headType) <- infer env head
    (s2, tailType) <- infer (apply s1 env) tail
    
    -- Unify tailType with a list of headType to enforce consistent types
    s3 <- unify (TArray headType) tailType
    
    -- Return the final type of the list with updated substitutions
    return (s3 `compose` s2 `compose` s1, TArray headType)

  
  -- TODO-CAT: Handle the Concat operator
  Op Concat left right -> do
    -- Infer the types of `left` and `right` lists
    (s1, leftType) <- infer env left
    (s2, rightType) <- infer (apply s1 env) right
    
    -- Create a fresh type variable for the element type of both lists
    elemType <- fresh
    
    -- Unify both leftType and rightType as lists of the same element type
    s3 <- unify (TArray elemType) (apply s2 leftType)
    s4 <- unify (TArray elemType) (apply (s3 `compose` s2) rightType)
    
    -- Return the final composed substitution and resulting list type
    let finalSubst = s4 `compose` s3 `compose` s2 `compose` s1
    return (finalSubst, TArray (apply finalSubst elemType))

  Op op e1 e2 -> do
    inferPrim env [e1, e2] (ops op)

  Lit (LInt _)  -> return (nullSubst, typeInt)
  Lit (LBool _) -> return (nullSubst, typeBool)
  -- TODO-1: Handle an Array literal
  -- Suggestion: Use foldM with a folding function that unifies 
  --             the result of infering on each element of the array
  Lit (LArray elements) -> do
    -- Create a fresh type variable for the array's element type
    elemType <- fresh

    -- Fold over each element in the array to enforce they all share the same type
    (s, _) <- foldM inferArrayElem (nullSubst, elemType) elements

    -- Trace the final unification result and return the array type
    trace ("Array literal unification result: " ++ show s ++ ", element type: " ++ show (apply s elemType)) (return ())
    return (s, TArray (apply s elemType))
  where
    inferArrayElem :: (Subst, Type) -> Expr -> Infer (Subst, Type)
    inferArrayElem (subst, elemType) expr = do
      -- Infer the type of the current array element
      (s', t) <- infer (apply subst env) expr

      -- Trace information about the inferred type and target element type
      trace ("Inferring array element: expr=" ++ show expr ++ ", inferred type=" ++ show t ++ ", target elemType=" ++ show elemType) (return ())

      -- Unify the inferred element type `t` with `elemType` to ensure consistency
      s'' <- unify elemType t
      trace ("Unification result for array element: " ++ show s'') (return ())

      -- Compose substitutions and return updated `elemType`
      return (s'' `compose` s' `compose` subst, apply s'' elemType)


inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
  inferStep (s, tf) exp = do
    (s', t) <- infer (apply s env) exp
    return (s' `compose` s, tf . TArrow t)

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = do
  ty <- inferExpr env ex
  inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TVar a)   = [a]
    fv (TArrow a b) = fv a ++ fv b
    fv (TCon _)   = []
    -- TODO-1: Handle TArray
    fv (TArray t) = fv t

    normtype (TArrow a b) = TArrow (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"
    -- TODO-1: Handle TArray
    normtype (TArray t) = TArray (normtype t)
