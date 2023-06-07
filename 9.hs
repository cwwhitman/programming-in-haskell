subs [] = [[]]
subs (x:xs) = subs' ++ map (x:) subs'
              where subs' = subs xs

interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices xs = [zs | ys <- subs xs, zs <- perms ys]

removeFirst _ [] = []
removeFirst x (y:ys) | x == y     = ys
                     | otherwise  = y : removeFirst x ys

isChoice _  []     = True
isChoice [] _      = False
isChoice (x:xs) ys = isChoice xs (removeFirst x ys)

-- would cause an infinite loop since you'd keep repeated the right side gets all terms case

