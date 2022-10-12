data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = (eval1 x) + (eval1 y)
eval1 (Sub x y) = (eval1 x) - (eval1 y)
eval1 (Mul x y) = (eval1 x) * (eval1 y)
eval1 (Div x y )= div (eval1 x) (eval1 y)

-- operate :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> a
operate f x y = do xval <- x
                   yval <- y
                   return $ f xval yval

eval2 :: Expr -> Maybe Int
eval2 (Val x) = (Just x)
eval2 (Add x y) = operate (+) (eval2 x) (eval2 y)
eval2 (Sub x y) = operate (-) (eval2 x) (eval2 y)
eval2 (Mul x y) = operate (*) (eval2 x) (eval2 y)
eval2 (Div x y) = do xval <- eval2 x
                     yval <- eval2 y
                     if yval == 0 then Nothing else Just(div xval yval)

eval3 :: Expr -> Either String Int
eval3 (Val x) = (Right x)
eval3 (Add x y) = operate (+) (eval3 x) (eval3 y)
eval3 (Sub x y) = operate (-) (eval3 x) (eval3 y)
eval3 (Mul x y) = operate (*) (eval3 x) (eval3 y)
eval3 (Div x y) = do xval <- eval3 x
                     yval <- eval3 y
                     if yval == 0 then Left "div0" else Right (div xval yval)