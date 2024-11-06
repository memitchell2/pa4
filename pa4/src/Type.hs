module Type where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TArrow Type Type -- Note: an "Arrow" is a function type, e.g. Int -> Bool
  -- TODO-1: Add a type for arrays (TArray)

  | TArray Type 
  
  deriving (Show, Eq, Ord)

infixr `TArrow`

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

typeInt :: Type
typeInt  = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"
