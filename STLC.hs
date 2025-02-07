module STLC where

import Control.Monad

data Term
  = Lit Literal
  | Var String
  | Lam String Type Term
  | App Term Term

data Literal
  = LInt Int
  | LBool Bool
  deriving Eq

data Type
  = TInt
  | TBool
  | TArr Type Type
  deriving Eq

instance Show Term where
  show t = case t of
    Lit l -> show l
    Var n -> n
    Lam n ty t' -> "\\" ++ n ++ ": " ++ show ty ++ ". " ++ show t'
    App t1@(App _ _) t2@(Lam {}) -> "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"
    App t1@(App _ _) t2          -> "(" ++ show t1 ++ ") " ++ show t2
    App t1           t2@(Lam {}) -> show t1 ++ " (" ++ show t2 ++ ")"
    App t1           t2          -> show t1 ++ " " ++ show t2 

instance Show Literal where
  show l = case l of
    LInt n -> show n
    LBool b -> show b

instance Show Type where
  show ty = case ty of
    TInt -> "int"
    TBool -> "bool"
    TArr ty1 ty2@(TArr _ _) -> show ty1 ++ "-> (" ++ show ty2 ++ ")"
    TArr ty1 ty2 -> show ty1 ++ " -> " ++ show ty2

type Env = [(String, Type)]

{-------------------------------------------------------------------------------}

check :: Env -> Term -> Maybe Type
check env term = case term of
  Lit (LInt _) -> return TInt
  Lit (LBool _) -> return TBool

  Var name -> lookup name env

  Lam name ty term -> do
    result <- check ((name, ty):env) term
    return $ TArr ty result

  App t1 t2 -> do
    ty1 <- check env t1
    ty2 <- check env t2
    case ty1 of
      TArr tyin tyout | tyin == ty2 -> return tyout
      _ -> fail "Applying term to non-function type"
