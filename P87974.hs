main = do
    x <- getLine
    let salut =
                if (head x == 'A') || (head x == 'a')
                    then "Hello!"
                    else "Bye!"
    putStrLn salut