module Syntax where

type Var = String

-- TODO-2: Change lambdas to use Pattern struct
data Expr
  = Var Var
  | App Expr Expr
  | Lam Var Expr
  | Let Var Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

-- TODO-1: Add an array literal (LArray)
data Lit
  = LInt Integer
  | LBool Bool
  | LArray [Expr]
  deriving (Show, Eq, Ord)

-- TODO-1: Add a Cons operator
-- TODO-CAT: Add a Concat operator
data Binop = Add | Sub | Mul | Eql | Cons | Concat
  deriving (Eq, Ord, Show)

-- TODO-2: Add a Pattern struct, capture the following cases:
-- let f x = ...      -- any identifier
-- let f 0 = ...      -- any int literal
-- let f True = ...   -- any bool literal
-- let f [] = ...     -- empty list
-- let f (x:xs) = ... -- non empty list

type Decl = (String, Expr)

data Program = Program [Decl] Expr deriving (Show, Eq)
