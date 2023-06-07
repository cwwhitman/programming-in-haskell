import Data.Char (digitToInt)
import System.IO

putStrc :: String -> IO ()
putStrc xs = sequence_ [putChar x | x <- xs]

putStrcLn xs = do putStrc xs
                  putChar '\n'

type Board = [Int]

putRow row num = do putStrc (show row)
                    putStrc ": "
                    putStrcLn (concat (replicate num "* "))

-- putBoard :: Board -> IO ()
-- putBoard = putBoardRows 1

-- putBoardRows _  []         = return ()
-- putBoardRows rn (row:rows) = do putRow rn row
--                                 putBoardRows (rn+1) rows

putBoard rows = sequence_ [putRow rn row | (rn, row) <- zip [1..] rows]

-- adder = adderStr 0 0

-- adderStr n s = do c <- getChar
--                   if c == '\n' then
--                      if n == 0 then
--                         do putStrc "The total is "
--                            putStrcLn (show s)
--                      else
--                         adderStr 0 (s+n)
--                   else
--                      adderStr (10*n + digitToInt c) s
-- i misread but i think this is more useful
-- though its just cause i didn't have the functions i should've...

getLinec = do c <- getChar
              if c == '\n' then
                 return []
              else 
                do cs <- getLinec
                   return (c : cs)


adder = do putStrc "How many numbers? "
           n <- getLinec
           xs <- sequence [readLine | _ <- [1..(read n)]]
           putStrc "The total is "
           putStrcLn (show (sum (map read xs)))

getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True 
           return x

readLine = do c <- getCh
              if c == '\n' then do
                 putChar '\n'
                 return []
              else if c == '\DEL' then do
                 putStrc "\b \b"
                 cs <- readLine
                 return ('\DEL' : cs)
              else do
                 putChar c
                 cs <- readLine
                 case cs of
                    ('\DEL':cs') -> return cs'
                    _ -> return (c : cs)

