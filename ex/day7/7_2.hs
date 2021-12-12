main = do
    dat <- splitWhen (==',') <$> readFile "input.txt"
    let nums = map read dat
    let mid = avg nums
    return $ sum $ map (toDrive . (\s -> abs (s-mid))) nums

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f xs = splitWhen' f xs [] where
    splitWhen' f []     acc             = [reverse acc]
    splitWhen' f (x:xs) acc | f x       = reverse acc : splitWhen' f xs []
                            | otherwise = splitWhen' f xs (x:acc)


toDrive :: Int -> Int
toDrive n = (n*n+n) `div` 2

fromDrive :: Int -> Int
fromDrive d = tstDrive d 0 where
    tstDrive d n | toDrive n < d = tstDrive d (n+1)
                 | otherwise     = n

avg :: [Int] -> Int
avg is = sum is `div` length is

qs :: (Ord a, Eq a) => [a] -> [a]
qs []     = []
qs (x:xs) = qs [n | n <- xs, n < x] ++ x : qs [b | b <- xs, b >= x]