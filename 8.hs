data Nat = Zero | Succ Nat deriving Show

add Zero n = n
add (Succ m) n = Succ (add m n)

mult Zero n = Zero
mult (Succ m) n = add n (mult m n)

-- data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- occurs x (Leaf y) = x == y
-- occurs x (Node l y r) = case compare x y of
--                           LT -> occurs x l
--                           EQ -> True
--                           GT -> occurs x r

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

countleaves (Leaf _)   = 1
countleaves (Node l r) = countleaves l + countleaves r

balanced (Leaf _)   = True
balanced (Node l r) = abs (countleaves l - countleaves r) <= 1
-- whoops, misread >~<

halve as = splitAt (length as `div` 2) as

balance [x] = Leaf x
balance xs  = Node (balance l) (balance r)
                where (l, r) = halve xs

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- eval = folde id (+)
size = folde (const 1) (+)

-- instance Eq a => Eq (Maybe a) where
--     Nothing == Nothing = True
--     Just x  == Just y  = x == y

-- instance Eq a => Eq [a] where
--     []     == []     = True
--     (x:xs) == (y:ys) = x == y && xs == ys
--     _      == _      = False

data Prop = Const Bool 
            | Var Char
            | Not Prop
            | And Prop Prop
            | Or Prop Prop
            | Imply Prop Prop
            | Eqv Prop Prop

type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

find :: Eq t => t -> [(t, b)] -> b
find k ((k', v):ss) = if k == k' then v else find k ss

eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Eqv p q)   = eval s p == eval s q

vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Eqv p q)   = vars p ++ vars q


bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (x /=) xs)

substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut p = and [eval s p | s <- substs p]