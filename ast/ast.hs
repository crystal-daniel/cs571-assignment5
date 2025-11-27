module Ast where

data Expr = Plus Expr Expr | Minus Expr Expr | Times Expr Expr | Div Expr Expr
    | Literal Float

eval :: Expr -> Float
eval (Literal x) = x
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2

eq :: Expr -> Expr -> Bool
eq (Literal x) (Literal y) = x == y
eq (Plus e1 e2) (Plus e3 e4) = eq e1 e3 && eq e2 e4
eq (Minus e1 e2) (Minus e3 e4) = eq e1 e3 && eq e2 e4
eq (Times e1 e2) (Times e3 e4) = eq e1 e3 && eq e2 e4
eq (Div e1 e2) (Div e3 e4) = eq e1 e3 && eq e2 e4
eq _ _ = False

-- Should eval to "5.0"
test1 = Plus (Literal 3.0) (Literal 2.0)

-- Should eval to "3.5"
test2 = Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0))

-- Should eval to "15.5"
test3 = Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0))

