import Data.Char
type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

one f p = map f . filter p

allc :: (a ->  Bool) -> [a] -> Bool
allc p = foldr (\x -> (p x &&)) True

anyc :: (a ->  Bool) -> [a] -> Bool
anyc p = foldr (\x -> (p x ||)) False

takeWhilec :: (a -> Bool) -> [a] -> [a]
takeWhilec p (x:xs) = if p x then x : takeWhilec p xs
                             else []

dropWhilec :: (a -> Bool) -> [a] -> [a]
dropWhilec p (x:xs) = if p x then dropWhilec p xs
                             else x:xs
                             
mapc f = foldr (\x -> (f x :)) []
filterc p = foldr (\x -> if p x then (x:) else id) []

dec2int :: [Int] -> Int
dec2int = foldl (\x -> (10*x +)) 0
-- this style feels stupid now...

curryc :: ((a, b) -> c) -> a -> b -> c
curryc f first second = f (first, second)

uncurryc :: (a -> b -> c) -> (a, b) -> c
uncurryc f (first, second) = f first second

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 = unfold null (take 8) (drop 8)
mapu f = unfold null (f . head) tail
iterateu = unfold (\_ -> False) id

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Bit
parity bs = sum bs `mod` 2

addparity bs = bs ++ [parity bs]

encode = concat . map (addparity . make8 . int2bin . ord)

chop9 = unfold null (take 9) (drop 9)
verifyparity bs = if parity bs == 0 then init bs
                                    else error "incorrect parity"

decode = map (chr . bin2int . verifyparity) . chop9


faultychannel :: [Bit] -> [Bit]
faultychannel = tail

transmit = decode . faultychannel . encode

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

luhnDouble :: Int -> Int
luhnDouble x | x > 4     = 2 * x - 9
             | otherwise = 2 * x

luhn :: [Int] -> Bool
luhn bs = (sum . altMap luhnDouble id) bs `mod` 10 == 0

digits :: [Char] -> [Int]
digits = map (read . (:[]))