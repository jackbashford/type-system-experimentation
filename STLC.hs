module STLC where

import Data.List

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
    TArr ty1@(TArr _ _) ty2 -> "(" ++ show ty1 ++ ") -> " ++ show ty2
    TArr ty1 ty2 -> show ty1 ++ " -> " ++ show ty2
    TVar n -> '_':show n
    Untyped -> "?"

data EnvItem = V Name | TV Name Int deriving Eq
newtype Env = E [(EnvItem, Type)]

extend :: Env -> (EnvItem, Type) -> Env
extend (E e) i = E (i:e)

inEnv :: EnvItem -> Env -> Maybe Type
inEnv i (E e) = lookup i e

nameFromEnv :: Name -> Env -> Maybe (EnvItem, Type)
nameFromEnv _ (E []) = Nothing
nameFromEnv name (E (e:es)) = case e of
  (TV n _, _) | n == name -> Just e
  (V n, _) | n == name -> Just e
  _ -> nameFromEnv name (E es)

instance Show Env where
  show (E e) = intercalate ", " (map showItem e)
    where
      showItem (V n, ty) = n ++ ": " ++ show ty
      showItem (TV n i, ty) = n ++ " (" ++ show i ++ "): " ++ show ty

newtype CheckResult = Check (Either String (Env, Type))

instance Show CheckResult where
  show (Check (Left msg)) = msg
  show (Check (Right (e, ty))) = show e ++ " |- " ++ show ty

newVarIdx :: Env -> Int
newVarIdx (E e) = go 0 e
  where
    go :: Int -> [(EnvItem, Type)]-> Int
    go n [] = n
    go n ((V _, _):xs) = go n xs
    go n ((TV _ m, _):xs) = go (max m n + 1) xs

replaceTVarIn :: Env -> Type -> Type -> Env
replaceTVarIn (E []) _ _ = E []
replaceTVarIn (E (i:e)) (TVar n) toReplace = case i of
  (TV name idx, TVar m) | m == n -> E $ (TV name idx, toReplace) : e
  _ -> extend (replaceTVarIn (E e) (TVar n) toReplace) i
replaceTVarIn _ _ _ = error "Tried to replace non-type-variable type"

{-------------------------------------------------------------------------------}
-- Type-related stuff (mine :D)

check :: Env -> Term -> CheckResult
check env term = Check (check' env term)

check' :: Env -> Term -> Either String (Env, Type)
check' env t = case t of
  Lit (LInt _) -> return (env, TInt)
  Lit (LBool _) -> return (env, TBool)

  Var name -> case nameFromEnv name env of
    Just (V n, ty) -> return (env, ty)
    Just (TV n i, ty) -> return (env, ty)
    Nothing -> do
      let i = newVarIdx env
      let ty = TVar i
      let env' = env `extend` (TV name i, ty)
      return (env', ty)

  Lam name Untyped term -> do
    let i = newVarIdx env
    let env' = env `extend` (TV name i, TVar i)
    (env'', result) <- check' env' term
    return (env'', TArr (TVar i) result)

  Lam name ty term -> do
    let env' = env `extend` (V name, ty)
    (env'', result) <- check' env' term
    return (env'', TArr ty result)

  App t1 t2 -> do
    (env', ty1) <- check' env t1
    (env'', ty2) <- check' env' t2
    case ty1 of
      TArr tyin tyout | tyin == ty2 -> return (env'', tyout)
                      | otherwise -> Left $ "Expected `" ++ show t2 ++ "` to have type `" ++ show tyin ++ "` but got `" ++ show ty2 ++ "`"
      TVar i -> let env''' = replaceTVarIn env'' ty1 (TArr ty2 Untyped) in return (env''', TArr ty2 Untyped)
      _ -> Left $ "Applying term of type `" ++ show ty2 ++ "` to non-function type `" ++ show ty1 ++ "`. Environments are: " ++ show env ++ " ---- " ++ show env' ++ " ---- " ++ show env''

-- idea for scoping: have a 'same name depth' attached to each name, and when a new binding for that one is encountered, a) increment all other variables of the same name b) add this new one c) typecheck the internal expression d) decrement all variables of that name e) pop the bound one off and use its value as needed
