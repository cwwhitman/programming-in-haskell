-- 1 + (2*3)
-- 1 + (2*3) is the outermost redex
--     (2*3) is the innermost redex

-- (1+2) * (2+3) 
-- (1+2) * (2+3) is the outermost redex
--  1+2          is the innermost leftmost redex
--          2+3  is an innermost redex

-- fst (1+2, 2+3)
-- fst (1+2, 2+3) is the outermost redex
--      1+2       is the innermost leftmost redex
--           2+3  is an innermost redex

-- (\x -> 1 + x) (2*3)
--                2*3  is the innermost leftmost redex

-- lol so bad

-- if doing innermost you would do two additions and
-- throw away the result of one of them, whereas outermost
-- lets you do only as much work as you need to

-- mult = \x -> (\y -> x * y)
--
-- mult 3 4
-- (\x -> (\y -> x * y)) 3 4
-- (\y -> 3 * y) 4
-- 3 * 4
-- 12

fibs :: [Integer]
fibs = [0, 1] ++ [a+b | (a, b) <- zip fibs (tail fibs)]

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

repeatt :: a -> Tree a
repeatt x = Node (repeatt x) x (repeatt x)

taket :: Int -> Tree a -> Tree a
taket 0 _            = Leaf
taket _ Leaf         = Leaf
taket n (Node l x r) = Node (taket (n-1) l) x (taket (n-1) r)

replicatet :: Int -> a -> Tree a
replicatet n = taket n . repeatt

newtonsqrt :: Double -> Double
newtonsqrt n = head [b | (a, b) <- zip aproxs (tail aproxs), abs (a-b) < 0.00001]
    where aproxs = iterate (\x -> (x + n/x)/2) 1