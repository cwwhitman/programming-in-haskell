double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

mylast (xs) = head (reverse xs)
melast [] = -1
melast [x] = x
melast (x:xs) = melast xs

myinit [] = []
myinit [x] = []
myinit (x:xs) = [x] ++ myinit xs

meinit xs = reverse (tail (reverse xs))