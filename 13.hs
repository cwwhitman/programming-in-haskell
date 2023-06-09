-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parsing (module Parsing, module Control.Applicative) where

import System.IO
import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- Expression parser

-- expr :: Parser Int
-- expr = do t <- term
--           do symbol "+"
--              e <- expr
--              return (t + e)
--            <|> do symbol "-"
--                   e <- expr
--                   return (t - e)
--            <|> return t

-- term :: Parser Int
-- term = do f <- factor
--           do symbol "*"
--              t <- term
--              return (f * t)
--            <|> do symbol "/"
--                   t <- term
--                   return (f `div` t)
--            <|> return f

-- factor :: Parser Int
-- factor = do symbol "("
--             e <- expr
--             symbol ")"
--             return e
--           <|> integer

-- IO utilities

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: (Int,Int) -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Calculator

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra
          where
             standard = "qcd=123+456-789*0()/"
             extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

display xs = do writeat (3,2) (replicate 13 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
                 process c xs
             else
                 do beep
                    calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"    = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n"       = eval xs
             | elem c "cC"        = clear
             | otherwise          = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
             [(n,[])] -> calc (show n)
             _        -> do beep
                            calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

-- Exercises

-- we have all these laws, is there any way to actually
-- make sure they're upheld?

-- probably a better way to do this

commentStart = string "--" <|>
            do sat ('\n'/=)
               commentStart


comment :: Parser ()
comment = do commentStart
             many (sat ('\n'/=))
             char '\n'
             return ()


-- parse trees done w/ pen and paper..

-- in the example of parsing one number, you'd always try both
-- branches of expr and term, so you'd be doing almost quadruple
-- the work! the simplification, for example in expr, essentially
-- allows you to not reparse a term if you already found it in
-- the first branch

-- newtype Expr = E (Term, Maybe Expr) deriving Show
-- newtype Term = T (Factor, Maybe Term) deriving Show
-- data Factor = Paren Expr | Nat Int deriving Show

-- expr :: Parser Expr
-- expr = do t <- term
--           do symbol "+"
--              e <- expr
--              return (E (t, Just e))
--            <|> return (E (t, Nothing))

-- term :: Parser Term
-- term = do f <- factor
--           do symbol "*"
--              t <- term
--              return (T (f, Just t))
--            <|> return (T (f, Nothing))

-- factor :: Parser Factor
-- factor = do symbol "("
--             e <- expr
--             symbol ")"
--             return (Paren e)
--           <|> (Nat <$> natural)

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> do symbol "-"
                  e <- expr
                  return (t - e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> do symbol "/"
                  t <- term
                  return (f `div` t)
           <|> return f

factor :: Parser Int
factor = do b <- base
            do symbol "^"
               p <- factor
               return (b ^ p)
             <|> return b


base :: Parser Int
base = do symbol "("
          e <- expr
          symbol ")"
          return e
        <|> integer

-- expr ::= nat (- expr | e)
-- nat ::= 0 | 1 | 2 | ...

-- expr :: Parser Int
-- expr = do n <- natural
--              do symbol "-"
--                 e <- expr
--                 return (n - e)
--               <|> return n

-- we recurse to the right, and if we implement left recursion
-- in the usual way we get recurse forever

negexpr :: Parser Int
negexpr = do n <- integer
             ns <- many (do symbol "-"
                            integer)
             return (foldl (-) n ns)