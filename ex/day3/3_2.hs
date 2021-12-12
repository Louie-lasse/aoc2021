main = do
    strs <- lines <$> readFile "input.txt"
    let bools = map (map (== '1')) strs
    let ox = binToInt $ parseOx bools
    let co2 = binToInt $ parseCarb bools
    return $ ox*co2

parse :: [[Char]] -> [[Int]]
parse []     = []
parse (s:ss) = parse' s : parse ss where
    parse' []     = []
    parse' (c:cs) = case c of
        '1' -> 1 : parse' cs
        _   -> 0 : parse' cs

transpose :: [[a]] -> [[a]]
transpose []     = []
transpose as = tops as : transpose (filter (not . null) (map (drop 1) as)) where
    tops el = map head $ filter (not . null) el

parseOx :: [[Bool]] -> [Bool]
parseOx []     = []
parseOx ns     = common : parseOx rest where
    rest = filter (not . null) $ map (drop 1) $ filter (\n -> head n == common) ns
    common = mostCommon (head $ transpose ns)

parseCarb :: [[Bool]] -> [Bool]
parseCarb [bs] = bs
parseCarb ns = leastC : parseCarb rest where
    rest = filter (not . null) $ map (drop 1) $ filter (\n -> head n == leastC) ns
    leastC = not $ mostCommon (head $ transpose ns)

mostCommon :: [Bool] -> Bool
mostCommon bs = length (filter id bs) >= length (filter not bs)

binToInt :: [Bool] -> Int
binToInt []     = 0
binToInt (b:bs) = fromEnum b * pow 2 (length bs) + binToInt bs where
    pow n 0 = 1
    pow n e = n * pow n (e-1)