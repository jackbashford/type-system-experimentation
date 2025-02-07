module STLCEval where

import STLC
import Data.List

-- The following yoinked from COMP1130 Lambda Calculus

free :: Term -> [Name]
free term = case term of
  Var x -> [x]
  App t u -> free t `union` free u
  Lam x _ t -> delete x (free t)

genFresh :: Term -> Name
genFresh = gFHelper "0"
  where
    gFHelper :: Name -> Term -> Name
    gFHelper x t
      | x `notElem` free t = x
      | otherwise = gFHelper ('0':x) t

replace :: Term -> Name -> Name -> Term
replace term oldName newName = case term of
  Var x
    | x == oldName -> Var newName
    | otherwise    -> term
  App t u -> App (replace t oldName newName) (replace u oldName newName)
  Lam x ty t
    | x == oldName -> Lam newName ty (replace t oldName newName)
    | otherwise    -> Lam x ty (replace t oldName newName)


subst :: Term -> Name -> Term -> Term
subst term name inTerm = case term of
  Var x
    | x == name -> inTerm
    | otherwise -> term
  App t u -> App (subst t name inTerm) (subst u name inTerm)
  Lam x ty t
    | x == name || elem x (free inTerm)
        -> subst (Lam fresh ty (replace t x fresh)) name inTerm
    | otherwise -> Lam x ty (subst t name inTerm)
    where
      fresh = genFresh (App t inTerm)

eval :: (a -> Maybe a) -> a -> a
eval step x = case step x of
  Nothing -> x
  Just y  -> eval step y

lazy :: Term -> Maybe Term
lazy term = case term of
  Var _   -> Nothing
  Lam {} -> Nothing
  App (Lam x ty t) u -> Just (subst t x u)
  App t u -> case lazy t of
    Nothing -> Nothing
    Just t' -> Just (App t' u)

lazyReduce :: Term -> Term
lazyReduce = eval lazy

strict :: Term -> Maybe Term
strict term = case term of
  Var _   -> Nothing
  Lam {} -> Nothing
  App (Lam x ty t) u -> case strict u of
    Nothing -> Just (subst t x u)
    Just u' -> Just (App (Lam x ty t) u')
  App t u -> case strict t of
    Nothing -> Nothing
    Just t' -> Just (App t' u)

strictReduce :: Term -> Term
strictReduce = eval strict
