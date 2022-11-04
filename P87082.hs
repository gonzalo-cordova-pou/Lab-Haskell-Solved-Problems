
main = do
    x <- getLine
    if x /= "*" then do
        let [name, weight, height] = words x
        let bmi = (read weight) / ((read height) * (read height :: Float))
        putStrLn $ name ++ ": " ++ interpretation bmi
        main
    else
        return ()

interpretation :: Float -> String
interpretation x
    | x < 18.0 = "underweight"
    | x < 25.0 = "normal weight"
    | x < 30.0 = "overweight"
    | x < 40.0 = "obese"
    | otherwise = "severely obese"
