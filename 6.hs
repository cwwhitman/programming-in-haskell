fac 0 = 1
fac n | n > 0 = n * fac (n-1)

sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- (^) n 0 = 1
-- (^) n m = n * (^) n (m-1)

--   (^) 2 3
-- = 2 * (^) 2 2
-- = 2 * (2 * (^) 2 1)
-- = 2 * (2 * (2 * (^) 2 0))
-- = 2 * (2 * (2 * (1)))
-- = 

euclid :: (Ord t, Num t) => t -> t -> t
euclid a b | a == b    = a
           | otherwise = if a < b then euclid a (b-a)
                                  else euclid (a-b) b

andc :: [Bool] -> Bool
andc [] =  True
andc (b:bs) | b         = andc bs
            | otherwise = False

concatc :: [[a]] -> [a]
concatc [] = []
concatc (l:ls) = l ++ concat ls

replicatec :: (Eq t1, Num t1) => t1 -> t2 -> [t2]
replicatec 0 x = []
replicatec n x = x : replicatec (n-1) x

pick :: (Eq t, Num t) => [a] -> t -> a
pick (x:_)  0 = x
pick (_:xs) n = pick xs (n-1)

elemc :: Eq t => t -> [t] -> Bool
elemc _ [] = False
elemc t (x:xs) | t == x    = True
               | otherwise = elemc t xs

merge :: Ord a => [a] -> [a] -> [a]
-- merge []     []     = []
-- merge xs     []     = xs
-- merge []     ys     = ys
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys)
                               else y : merge (x:xs) ys
-- what if we removed the first three and just put 
merge xs ys = xs ++ ys

halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
            where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort former) (msort latter)
                where (former, latter) = halve xs