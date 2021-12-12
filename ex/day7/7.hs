main = do
    dat <- splitWhen (==',') <$> readFile "input.txt"
    let nums = map read dat
    let meed = meedian nums
    return $ sum $ map (\s -> abs (s-meed)) nums


splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f xs = splitWhen' f xs [] where
    splitWhen' f []     acc             = [reverse acc]
    splitWhen' f (x:xs) acc | f x       = reverse acc : splitWhen' f xs []
                            | otherwise = splitWhen' f xs (x:acc)

meedian :: [Int] -> Int
meedian ns | isSorted ns = ns    !! div (length ns) 2
           | otherwise   = qs ns !! div (length ns) 2
    where
        isSorted ns = and $ zipWith (<) ns (drop 1 ns)

qs :: (Ord a, Eq a) => [a] -> [a]
qs []     = []
qs (x:xs) = qs [n | n <- xs, n < x] ++ x : qs [b | b <- xs, b >= x]