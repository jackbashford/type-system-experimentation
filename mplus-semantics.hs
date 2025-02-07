module MPlus_Semantics where

import Control.Monad

data Expr = Val Int | Plus Expr Expr | Times Expr Expr deriving Show

-- evaluation: stepwise reduction until done
eval e = done e `mplus` (step e >>= eval)

-- normal form
done e = do
  Val i <- return e;
  return (Val i)

-- reduction rule
plus e = do
  (Plus (Val i) (Val j)) <- return e;
  return (Val (i+j))

times e = do
  (Times (Val i) (Val j)) <- return e;
  return (Val (i*j))

-- one-step reduction: apply reduction rule in evaluation context
step e = case e of
  Plus {} -> ctxtPlus plus 

-- evaluation contexts, parameterized by rule to apply
ctxt rule e = rule e `mplus` leftPlus (ctxt rule) e `mplus` rightPlus (ctxt rule) e

leftPlus act e = do
  (Plus l r) <- return e;
  lv <- act l;
  return (Plus lv r)

rightPlus act e = do
  (Plus lv@(Val _) r) <- return e;
  rv <- act r;
  return (Plus lv rv)
