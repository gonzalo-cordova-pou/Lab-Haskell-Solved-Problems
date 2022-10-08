
-- PROBLEM: https://jutge.org/problems/70540_en


data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add a b) = eval1(a) + eval1(b)
eval1 (Sub a b) = eval1(a) - eval1(b)
eval1 (Mul a b) = eval1(a) * eval1(b)
eval1 (Div a b) = eval1(a) `div` eval1(b)


operate f x y = do xval <- x
                   yval <- y
                   return $ f xval yval

eval2 :: Expr -> Maybe Int
eval2 (Val x) = Just(x)
eval2 (Add a b) = operate (+) (eval2 a) (eval2 b)
eval2 (Sub a b) = operate (-) (eval2 a) (eval2 b)
eval2 (Mul a b) = operate (*) (eval2 a) (eval2 b)
eval2 (Div a b) = do xval <- eval2 a
                     yval <- eval2 b
                     if yval == 0
                         then Nothing
                         else operate (div) (eval2 a) (eval2 b)

eval3 :: Expr -> Either String Int
eval3 (Val x) = Right x
eval3 (Add a b) = operate (+) (eval3 a) (eval3 b)
eval3 (Sub a b) = operate (-) (eval3 a) (eval3 b)
eval3 (Mul a b) = operate (*) (eval3 a) (eval3 b)
eval3 (Div a b) = do xval <- eval3 a
                     yval <- eval3 b
                     if yval == 0
                         then Left "div0"
                         else operate (div) (eval3 a) (eval3 b)