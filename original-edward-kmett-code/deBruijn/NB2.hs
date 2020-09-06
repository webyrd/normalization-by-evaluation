
{-# language StrictData #-}

data Expr
  = Var Int
  | Lam Expr
  | App Expr Expr
  deriving Show

type Env = [Val]

data Val 
  = VLam Env Expr
  | VVar Int Spine

data Spine
  = SNil
  | SApp Spine Val

nth :: Int -> [a] -> a
nth 0 (x:_) = x
nth n (_:xs) = nth (n-1) xs

eval :: Env -> Expr -> Val
eval e (Var x) = nth x e
eval e (App f x) = app (eval e f) (eval e x)
eval e (Lam b) = VLam e b

app :: Val -> Val -> Val
app (VLam e b) v = eval (v:e) b
app (VVar n s) v = VVar n (SApp s v)

uneval :: Int -> Val -> Expr
uneval d (VLam e b) = Lam (uneval (d+1) (eval (VVar d SNil:e) b))
uneval d (VVar n s) = unevalSp d (Var (d-n-1)) s

unevalSp :: Int -> Expr -> Spine -> Expr
unevalSp d e SNil = e
unevalSp d e (SApp xs x) = App (unevalSp d e xs) (uneval d x)

nf :: Env -> Expr -> Expr
nf e t = uneval 0 (eval e t)

main = do
  let id_ = eval [] (Lam $ Var 0)
  let const_ = eval [] (Lam $ Lam $ Var 1)
  print $ nf [id_,const_] $ App (Var 1) (Var 0)

