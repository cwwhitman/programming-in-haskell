{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving Show

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Node l x r) = Node l' x' r'
        where
            l' = fmap f l
            x' = f x
            r' = fmap f r

tree = Node (Node Leaf 5 Leaf) 7 (Node (Node Leaf 1 Leaf) 5 (Node Leaf 9 Leaf))

-- instance Functor ((->) a) where
--     fmap :: (b -> c) -> (a -> b) -> (a -> c)
--     fmap = (.)

-- instance Applicative ((->) a) where
--     pure x = \(_ :: a) -> x
--     (<*>) :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
--     f <*> g = \x -> f x (g x)

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
    pure :: a -> ZipList a
    pure x = Z (repeat x)

    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z ([g x | (g, x) <- zip gs xs])

{-
pure id <*> x = x

pure id :: f id
id :: a -> a
x :: f a

pure (g x) = pure g <*> pure x

x :: a
g :: a -> b
pure (g x) :: f b

x <*> pure y = pure (\g -> g y) <*> x

x :: f (a -> b)
y :: a
lhs = f b
(\g -> g y) :: (a -> b) -> b
pure (\g -> g y) = f ((a -> b) -> b)


x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

z :: f a
y :: f (a -> b)
x :: f (b -> c)
lhs :: f c

pure (.) :: f (.)                   v--does this have to be a?
pure (.) <*> x               :: f ((a -> b) -> a -> c)
pure (.) <*> x <*> y         :: f (a -> c)
(pure (.) <*> x <*> y) <*> z :: f c
-}

-- instance Monad ((->) a) where
--     (>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
--     f >>= g = g . f

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
    deriving Show

instance Functor Expr where
    fmap :: (a -> b) -> Expr a -> Expr b
    fmap _ (Val x)   = Val x
    fmap f (Add l r) = Add (fmap f l) (fmap f r)
    fmap f (Var x)   = Var (f x)

instance Applicative Expr where
    pure :: a -> Expr a
    pure = Var

    (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    (Val x)   <*> _ = Val x
    (Add l r) <*> y = Add (l <*> y) (r <*> y)
    (Var g)   <*> y = fmap g y

-- ^-- this solution will double y's Vals if the lhs is Add,
-- i guess the correct solution is just to leave the val as is
-- this doesn't seem entirely obvious like monad and functor
-- but clearly my solution is wrong

instance Monad Expr where
    (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (Val x)   >>= _ = Val x
    (Add l r) >>= f = Add (l >>= f) (r >>= f)
    (Var x)   >>= f = f x

-- >>= takes an Expr e and f, and substitutes the Vars of e 
-- using f for an arbitrary Expr
-- For example:

find k kvs = head [v | (k', v) <- kvs, k == k']
e = Add (Var 'a') (Add (Val 5) (Var 'b'))
values = [('a', Val 7), ('b', Add (Val 1) (Var 'c'))]

e' = e >>= (\v -> find v values)

type State = Int
newtype ST a = S (State -> (a,State))
app (S st) x = st x

instance Functor ST where
    fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do x <- st
                   return (g x)

instance Applicative ST where
    pure :: a -> ST a
    pure x = S (\s -> (x,s))

    (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do f <- stf
                     x <- stx
                     return (f x)

instance Monad ST where
    (>>=) :: ST a -> (a -> ST b) -> ST b 
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')