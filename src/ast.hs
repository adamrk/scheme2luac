module AST where

data Expr a = Lit Int | Add a a | Mult a a | Subr a a deriving (Eq, Show)

data Inst = OpLoad Int | OpAdd | OpMult | OpSubr deriving (Eq, Show)

instance Functor Expr where
  fmap f (Add a b) = Add (f a) (f b)
  fmap f (Mult a b) = Mult (f a) (f b)
  fmap f (Subr a b) = Subr (f a) (f b)
  fmap _ (Lit x) = Lit x

data Fix f = Fix (f (Fix f))

foo :: Fix Expr
foo = Fix (Add (Fix $ Lit 5) (Fix $ Mult (Fix $ Lit 3) (Fix $ Lit 2)))

calculate :: Fix Expr -> Int
calculate (Fix (Lit x)) = x
calculate (Fix (Add a b)) = calculate a + calculate b
calculate (Fix (Mult a b)) = calculate a * calculate b
calculate (Fix (Subr a b)) = calculate a - calculate b

inst :: Fix Expr -> [Inst]
inst (Fix (Lit x)) = [OpLoad x]
inst (Fix (Add a b)) = inst a ++ inst b ++ [OpAdd]
inst (Fix (Mult a b)) = inst a ++ inst b ++ [OpMult]
inst (Fix (Subr a b)) = inst a ++ inst b ++ [OpSubr]