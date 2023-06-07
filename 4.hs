halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
            where n = length xs `div` 2

third1 xs = head (tail (tail xs))
third2 xs = xs !! 2
third3 (_:_:x:_) = x

safetail1 xs = if null xs then [] else tail xs
safetail2 xs | null xs   = []
             | otherwise = tail xs
safetail3 [] = []
safetail3 xs = tail xs

-- True  || True  = True
-- True  || False = True
-- False || True  = True
-- False || False = False

-- False || False = False
-- _ || _ = True

-- False || b = b
-- True || _ = True

b || c | b == c    = b
       | otherwise = True

-- a && b = if a then
--             if b then True else False
--          else False

-- a && b = if a then b else False

mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble :: Int -> Int
luhnDouble x | x > 4     = 2 * x - 9
             | otherwise = 2 * x

luhn x y z w = (luhnDouble x + y + luhnDouble z + w) `mod` 10 == 0
