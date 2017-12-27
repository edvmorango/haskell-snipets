module HuttonsRazor where


data Expr =
   Lit Integer
 | Add Expr Expr

eval :: Expr -> Integer
eval (Lit a) = a
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit a) = show a
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
