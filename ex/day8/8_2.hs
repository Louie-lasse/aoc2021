main = do
    lns <- lines <$> readFile "input.txt"
    let codes = map splitCode lns
    return $ sum $ map (comb . calcVals) codes

type Code = ([String],[String])

splitWhen :: Eq a => [a] -> a -> [[a]]
splitWhen [] _ = []
splitWhen el m = first : splitWhen (drop 1 rest) m where
    (first,rest) = break (==m) el

splitCode :: String -> Code
splitCode s = (c `splitWhen` ' ', drop 1 $ r `splitWhen` ' ') where
    (c,r) = break (=='|') s

comb :: [Int] -> Int
comb [] = 0
comb (n:ns) = n * 10 `pow` length ns + comb ns where
    pow _ 0 = 1
    pow e n = e * e `pow` (n-1)

calcVals :: Code -> [Int]
calcVals (i,o) = o `findVals` [one,two,three,four,five,six,seven,eight,nine,zero] where
    one = (head $ filter (\s -> 2 == length s) i,1)
    two = (head $ filter (\s -> not (null (s `deleteAll` fst nine))) $ filter (\s -> 5 == length s) i,2)
    three = (head $ filter (fst one `allIn`) $ filter (\s -> 5 == length s) i,3)
    four = (head $ filter (\s -> 4 == length s) i,4)
    five = (head $ filter (\s -> null (s `deleteAll` fst six)) $ filter (\s -> 5 == length s) i,5)
    six = (head $ filter (not . allIn (fst one)) $ filter (\s -> 6 == length s) i,6)
    seven = (head $ filter (\s -> 3 == length s) i,7)
    eight = (head $ filter (\s -> 7 == length s) i,8)
    nine = (head $ filter (fst four `allIn` ) $ filter (\s -> 6 == length s) i,9)
    zero = (head $ filter (not . allIn (fst four `deleteAll` fst one)) $ filter (\s -> 6 == length s) i,0)

findVals :: [String] -> [(String, Int)] -> [Int]
findVals []     _ = []
findVals (s:ss) p = (s `findIn` p) : findVals ss p where
    findIn :: String -> [(String,Int)] -> Int
    findIn s []                              = error $ "could not find" ++ show s
    findIn s ((code,v):ps) | s `isSame` code = v
                           | otherwise     = s `findIn` ps

isSame :: String -> String -> Bool
isSame []     []                 = True
isSame (c:cs) s | s `contains` c = isSame cs $ s `delete` c
isSame _      _                  = False

deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll i []     = i
deleteAll i (x:xs) = (i `delete` x) `deleteAll` xs

delete :: Eq a => [a] -> a -> [a]
delete []     _             = []
delete (i:is) m | i == m    = is
                | otherwise = i : is `delete` m

contains :: Eq a => [a] -> a -> Bool
contains []     _             = False
contains (x:xs) m | x == m    = True
                  | otherwise = xs `contains` m

allIn :: String -> String -> Bool
allIn s1 s2 = and [s2 `contains` c | c <- s1]

u = undefined