module HuttonsRazor where


data Expr =
   Lit Integer
 | Add Expr Expr



eval :: Expr -> Integer
eval (Lit a) = a
eval (Add x y) = eval x + eval y
