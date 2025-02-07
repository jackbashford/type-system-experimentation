module STLC where

type Name = String

data Term
  = Lit Literal
  | Var Name
  | Lam Name Type Term
  | App Term Term

data Literal
  = LInt Int
  | LBool Bool
  deriving Eq

data Type
  = TInt
  | TBool
  | TArr Type Type
  | TVar Int
  | Untyped
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
    TVar n -> '_':show n
    Untyped -> "?"

data EnvItem = V Name | TV Name Int deriving (Show, Eq)
type Env = [(EnvItem, Type)]

newVarIdx :: Env -> Int
newVarIdx = go 0
  where
    go :: Int -> Env -> Int
    go n [] = n
    go n ((V _, _):xs) = go n xs
    go n ((TV _ m, _):xs) = go (max m n + 1) xs

{-------------------------------------------------------------------------------}
-- Type-related stuff (mine :D)

check :: Env -> Term -> Either String Type
check env term = snd <$> check' env term

check' :: Env -> Term -> Either String (Env, Type)
check' env term = case term of
  Lit (LInt _) -> return (env, TInt)
  Lit (LBool _) -> return (env, TBool)

  Var name -> case lookup (V name) env of
    Just ty -> return (env, ty)
    Nothing -> let ty = newVarIdx env in return ((V name, TVar ty):env, TVar (newVarIdx env))

  Lam name Untyped term -> do
    let i = newVarIdx env
    result <- check' ((TV name i, TVar i):env) term
    undefined

  Lam name ty term -> do
    (env', result) <- check' ((V name, ty):env) term
    return (env', TArr ty result)

  App t1 t2 -> do
    (env', ty1) <- check' env t1
    (env'', ty2) <- check' env' t2
    case ty1 of
      TArr tyin tyout | tyin == ty2 -> return (env'', tyout)
                      | otherwise -> Left $ "Expected `" ++ show t2 ++ "` to have type `" ++ show tyin ++ "` but got `" ++ show ty2 ++ "`"
      _ -> Left $ "Applying term of type `" ++ show ty2 ++ "` to non-function type `" ++ show ty1 ++ "`"
