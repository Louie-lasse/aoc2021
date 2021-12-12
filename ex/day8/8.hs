main = do
    lns <- lines <$> readFile "input.txt"
    let ress = [drop 2 (dropWhile (/= '|') l) | l <- lns]
    let lens = map length $ concatMap (`splitWhen` ' ') ress
    return $ length $ filter (\i -> i<5 || i==7) lens

splitWhen :: Eq a => [a] -> a -> [[a]]
splitWhen [] _ = []
splitWhen el m = first : splitWhen (drop 1 rest) m where
    (first,rest) = break (==m) el