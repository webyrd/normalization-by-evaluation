import Control.Monad.Fail as Monad

import Control.Monad (unless)
import Data.Maybe

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr

  | LitBool Bool
  | If Expr Expr Expr

  | Zero
  | Succ Expr
  | Rec Type Expr Expr Expr

  | Ann Expr Type
  deriving Show

data Type
  = Bool
  | Nat
  | Type :-> Type
  deriving (Show, Eq)

type Env a = [(Name,a)]

data Value
  = Closure (Env Value) Name Expr
  | N Neutral
  | VLitBool Bool
  | VZero
  | VSucc Value
  deriving Show

data Neutral
  = NVar Name
  | NApp Neutral Value
  | NRec Type Neutral Value Value
  | NIf Neutral Value Value
  deriving Show

fresh :: [Name] -> Name -> Name
fresh xs x
  | elem x xs = fresh xs (x ++ "'")
  | otherwise = x

extend :: Name -> a -> Env a -> Env a
extend x v xs = (x,v):xs

lookupVar :: MonadFail m => Name -> Env a -> m a
lookupVar x xs = case lookup x xs of
  Nothing -> Monad.fail "bad var"
  Just v -> pure v

infer :: MonadFail m => Env Type -> Expr -> m Type
infer e (Var x) = lookupVar x e
infer e (App f x) = do
  σ :-> τ <- infer e f
  τ <$ check e x σ
infer _ Zero = pure Nat
infer _ (LitBool _) = pure Bool
infer e (If c tb eb) = do
  check e c Bool
  tt <- infer e tb
  tt <$ check e eb tt
infer e (Ann t τ) = τ <$ check e t τ
infer e (Succ n) = Nat <$ check e n Nat
infer e (Rec τ n z s) = do
  check e n Nat
  check e z τ
  τ <$ check e s (τ :-> τ)
infer _ t = Monad.fail $ "unable to infer type for " ++ show t

check :: MonadFail m => Env Type -> Expr -> Type -> m ()
check e (Lam x b) (σ :-> τ) = check (extend x σ e) b τ
check e tm τ = do
  τ' <- infer e tm
  unless (τ == τ') $ Monad.fail "type mismatch"

eval :: Env Value -> Expr -> Value
eval e (Var x) = fromJust $ lookupVar x e
eval e (App f x) = apply (eval e f) (eval e x)
eval e (Lam x b) = Closure e x b
eval _ (LitBool b) = VLitBool b
eval _ Zero = VZero
eval e (Succ t) = VSucc (eval e t)
eval e (If c tb eb) = if_ e (eval e c) tb eb
eval e (Rec τ n z s) = rec e τ (eval e n) (eval e z) s

apply :: Value -> Value -> Value
apply (Closure e x b) v = eval (extend x v e) b
apply (N n) v = N (NApp n v)

if_ :: Env Value -> Value -> Expr -> Expr -> Value
if_ e (VLitBool c) tb eb = eval e $ if c then tb else eb
if_ e (N n) tb eb = N $ NIf n (eval e tb) (eval e eb)

rec :: Env Value -> Type -> Value -> Value -> Expr -> Value
rec e _ VZero z _ = z
rec e τ (VSucc n) z s = rec e τ n (apply (eval e s) z) s
rec e τ (N n) z s = N $ NRec τ n z (eval e s)

uneval :: [Name] -> Value -> Expr
uneval xs VZero = Zero
uneval xs (VSucc n) = Succ $ uneval xs n
uneval xs (VLitBool b) = LitBool b
uneval xs (Closure e x b) =
  let x' = fresh xs x in
  Lam x' $ uneval (x':xs) $ eval (extend x (N $ NVar x') e) b
uneval xs (N n) = go n where
  go (NVar x) = Var x
  go (NApp n v) = App (go n) (uneval xs v)
  go (NRec τ n z s) = Rec τ (go n) (uneval xs z) (uneval xs s)
  go (NIf n tv ev) = If (go n) (uneval xs tv) (uneval xs ev)

program :: MonadFail m => Env Expr -> m (Env Type, Env Value)
program = go [] [] where
  go ctx e [] = pure (ctx,e)
  go ctx e ((x,tm):xs) = do
    ty <- infer ctx tm
    let v = eval e tm
    go (extend x ty ctx) (extend x v e) xs

nf :: Env Value -> Expr -> Expr
nf e t = uneval [] (eval e t)

example :: MonadFail m => m Expr
example = do
  (ctx,e) <- program
    [ ("id", Ann (Lam "x" $ Var "x") $ Nat :-> Nat)
    , ("const", Ann (Lam "x" $ Lam "y" $ Var "x") $ (Nat :-> Nat) :-> Bool :-> (Nat :-> Nat))
    ]
  let tm = App (Var "const") (Var "id")
  infer ctx tm
  pure $ nf e tm

main :: IO ()
main = example >>= print
