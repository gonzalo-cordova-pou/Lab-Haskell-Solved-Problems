fizzBuzz :: [Either Int String]
fizzBuzz = map aux [0..]
    where aux a
            | (a `mod` 3 == 0) && (a `mod` 5 == 0) = Right "FizzBuzz"
            | a `mod` 3 == 0 = Right "Fizz"
            | a `mod` 5 == 0 = Right "Buzz"
            | otherwise = Left a