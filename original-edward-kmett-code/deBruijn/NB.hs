
{-# language StrictData #-}

data Expr
  = Var Int
  | Lam Expr
  | App Expr Expr
  deriving (Eq,Show)

type Env = [Val]

data Val 
  = Clo Env Expr -- (Val -> Val)
  | N Neutral

data Neutral
  = NVar Int
  | NApp Neutral Val

nth :: Int -> [a] -> a
nth 0 (x:_) = x
nth n (_:xs) = nth (n-1) xs

eval :: Env -> Expr -> Val
eval e (Var x) = nth x e
eval e (App f x) = app (eval e f) (eval e x)
--eval e (Lam n b) = Clo n \v -> eval (v:e) b
eval e (Lam b) = Clo e b

app :: Val -> Val -> Val
--app (Clo _ f) v = f v
app (Clo e b) v = eval (v:e) b
app (N n) v = N (NApp n v)

uneval :: Int -> Val -> Expr
uneval d (Clo e b) = Lam (uneval (d+1) (eval (N (NVar d):e) b))
uneval d (N n) = unevalN d n

unevalN :: Int -> Neutral -> Expr
unevalN d (NVar n) = Var (d-n-1)
unevalN d (NApp f x) = App (unevalN d f) (uneval d x)

nf :: Env -> Expr -> Expr
nf e t = uneval 0 (eval e t)

main = do
  let id_ = eval [] (Lam $ Var 0)
  let const_ = eval [] (Lam $ Lam $ Var 1)
  print $ nf [id_,const_] $ App (Var 1) (Var 0)

