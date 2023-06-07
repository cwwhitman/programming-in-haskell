import Data.Char

hundredsq = sum [x^2 | x <- [1..100]]

grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

square n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate1 n x = [x | _ <- [1..n]]

pyths n = [(x, y, z) | z <- [1..n], 
                       x <- [1..z],
                       y <- [1..z], 
                       x^2 + y^2 == z^2]

factors n = [d | d <- [1..n], n `mod` d == 0]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

doublet = [(x,y) | x <- [1,2], y <- [3,4]]
singlet = concat [[(x, y) | x <- [1,2]] | y <- [3,4]]

find xs k = [v | (k', v) <- xs, k' == k]
positions xs x = find (zip xs [0..])

scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]