module LetZL where

data Expr = Z Int
          | Nil
          | Cons Expr Expr
          | Car Expr
          | Cdr Expr
          | Var Int
          | Let Int Expr Expr -- binding, substitution, target
          | Plus Expr Expr
          | Mul Expr Expr
          deriving Show

data Val where
  ZVal :: Int -> Val
  NilVal :: Val
  ConsVal :: Val -> Val -> Val
  deriving Show

toVal :: Expr -> Maybe Val
toVal e = case e of
  Nil -> Just NilVal
  Z n -> Just (ZVal n)
  Cons e1 e2 -> do
    v1 <- toVal e1
    v2 <- toVal e2
    return $ ConsVal v1 v2
  _ -> Nothing

fromVal :: Val -> Expr
fromVal v = case v of
  ZVal n -> Z n
  NilVal -> Nil
  ConsVal v1 v2 -> Cons (fromVal v1) (fromVal v2)

data Ctx = Empty
         | ConsL Ctx Expr
         | ConsR Val Ctx
         | PlusL Ctx Expr
         | PlusR Val Ctx
         | MulL Ctx Expr
         | MulR Val Ctx
         | Car' Ctx
         | Cdr' Ctx
         | Let' Int Ctx Expr
         deriving Show

eval :: Expr -> Val
eval e | Just v <- toVal e = v
       | otherwise = eval' $ ctxify e

eval' c = case c of

ctxify :: Expr -> Ctx
ctxify e = case e of
  Car e' -> Car' (ctxify e')
  Cdr e' -> Cdr' (ctxify e')
  Let n e' t -> Let' n (ctxify e') t
  Plus a b
    | Just a' <- toVal a -> PlusR a' (ctxify b)
    | otherwise -> PlusL (ctxify a) b
  Mul a b
    | Just a' <- toVal a -> MulR a' (ctxify b)
    | otherwise -> MulL (ctxify a) b
  Cons h t
    | Just h' <- toVal h -> ConsR h' (ctxify t)
    | otherwise -> ConsL (ctxify h) t
  _ -> Empty

-- fromVal :: Val -> Expr
-- fromVal v = case v of
--   Z' n -> Z n
--   Nil' -> Nil
--   Cons' v1 v2 -> Cons (fromVal v1) (fromVal v2)

-- toVal :: Expr -> Maybe Val
-- toVal e = case e of
--   Z n -> Just (Z' n)
--   Nil -> Just Nil'
--   Cons e1 e2 -> do
--     v1 <- toVal e1
--     v2 <- toVal e2
--     Just (Cons' v1 v2)
--   _ -> Nothing

-- eval :: Expr -> Val
-- eval e = case e of
--   Z n -> Z' n
--   Nil -> Nil'
--   Plus (Z n) (Z m) -> Z' (n + m)
--   Mul (Z n) (Z m) -> Z' (n * m)
--   Cons e1 e2
--     | (Just v) <- toVal e -> v
--     | (Just v1) <- toVal e1 -> eval $ Cons e1 (fromVal $ eval e2)
--     | otherwise -> eval $ Cons (fromVal $ eval e1) e2

