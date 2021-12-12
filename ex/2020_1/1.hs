main = do
    nums <- map (\x -> read x :: Int) <$> ( lines <$> readFile "input.txt")
    let (a,b,c) =  head [(a,b,c) | a <- nums, b <- nums, c <- nums, a+b+c==2020]
    return $ a*b*c