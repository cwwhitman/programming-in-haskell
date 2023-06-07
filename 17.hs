{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Control.Applicative

data Expr = Val Int
          | Add Expr Expr
          | Throw
          | Catch Expr Expr

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval Throw = Nothing
eval (Add x y) = (+) <$> eval x <*> eval y
eval (Catch x h) = eval x <|> eval h

type Stack = [Maybe Int]

-- exec (comp e) s = eval e : s
-- exec (comp’ e c) s = exec c (eval e : s)

{-
  exec (comp’ (Val n) c) s
= exec c (eval (Val n) : s)
= exec c ((Just n) : s)
= exec (PUSH (Just n) c) s

exec (PUSH n c) s = exec c (n : s)
comp’ (Val n) c = PUSH (Just n) c

  exec (comp’ Throw c) s
= exec c (eval Throw : s)
= exec c (Nothing : s)
= exec (PUSH Nothing c) s

comp’ (Throw) c = PUSH Nothing c

  exec (comp’ (Add x y) c) s
= exec c (eval (Add x y) : s)
= exec c ((+) <$> eval x <*> eval y : s)
= exec (ADD c) ((eval y : eval x : s)
= exec (comp’ x (comp’ y (ADD c))) s

exec (ADD c) (m : n : s) = exec c ((+) <$> n <*> m : s)
comp’ (Add x y) c = comp’ x (comp’ y (ADD c))

  exec (comp’ (Catch x h) c) s
= exec c (eval (Catch x h) : s)
= exec c (eval x <|> eval h : s)
= exec (CATCH (comp h) c) (eval x : s)
= exec (comp' x (CATCH (comp h) c)) s

comp’ (Catch x h) c = comp' x (CATCH (comp h) c)
exec (CATCH h c) (Nothing : s) = exec (h:c) s
exec (CATCH h c) (Just x : s) = exec c (Just x : s)

eval x is Nothing
  exec (comp' x (CATCH (comp h) c)) s
= exec (CATCH (comp h) c) (Nothing : s)
= exec (comp' h c) s 
= exec c (eval h : s)

eval x is Just x'
  exec (comp' x (CATCH (comp h) c)) s
= exec (CATCH (comp h) c) (eval x : s)
= exec c (eval x : s)

exec (comp’ h (CATCH h c)) s

-}

data Code = HALT | PUSH (Maybe Int) Code | ADD Code | CATCH Code Code deriving Show

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH (Just n) c
comp' Throw c = PUSH Nothing c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Catch x h) c = comp' x (CATCH (comp' h c) c)

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c (((+) <$> n <*> m) : s)
exec (CATCH h _) (Nothing : s) = exec h s
exec (CATCH _ c) (Just x : s) = exec c (Just x : s)