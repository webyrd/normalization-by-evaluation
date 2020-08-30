type Name = String

data Expr 
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  deriving Show

type Env = [(Name,Value)]

data Value
  = Closure Env Name Expr
  | N Neutral
  deriving Show

data Neutral
  = NVar Name
  | NApp Neutral Value
  deriving Show
  
eval :: MonadFail m => Env -> Expr -> m Value
eval e (Var x) = case lookup x e of
  Nothing -> fail "uhoh"
  Just v -> pure v
eval e (App f x) = do
  vf <- eval e f
  vx <- eval e x
  apply vf vx
eval e (Lam x b) = pure $ Closure e x b

apply :: MonadFail m => Value -> Value -> m Value
apply (Closure e x b) v = eval ((x,v):e) b
apply (N n) v = pure (N (NApp n v))

fresh :: [Name] -> Name -> Name
fresh xs x
  | elem x xs = fresh xs (x++"'")
  | otherwise = x

uneval :: MonadFail m => [Name] -> Value -> m Expr
uneval xs (Closure e x b) = do
  let x' = fresh xs x 
  bv <- eval ((x,N $ NVar x'):e) b 
  b' <- uneval (x':xs) bv 
  pure (Lam x' b')
uneval xs (N n) = unevalN xs n

unevalN :: MonadFail m => [Name] -> Neutral -> m Expr
unevalN _ (NVar x') = pure $ Var x'
unevalN xs (NApp n v) = do
  ne <- unevalN xs n 
  ve <- uneval xs v
  pure (App ne ve)

nf :: MonadFail m => Env -> Expr -> m Expr
nf e t = do
  v <- eval e t 
  uneval [] v

main = do
  id_ <- eval [] (Lam "x" (Var "x"))
  const_ <- eval [] (Lam "x" (Lam "y" (Var "x")))
  result <- eval [("id",id_),("const",const_)] (App (Var "const") (Var "id"))
  print result
