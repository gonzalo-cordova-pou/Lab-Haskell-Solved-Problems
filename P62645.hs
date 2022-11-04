
main = do
    all <- getContents
    let linies = lines all
    let enters_s = concat [words l | l <- linies] 
    let enters = map read enters_s
    putStrLn $ show $ sum enters
    return () 