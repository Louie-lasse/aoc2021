import Prelude hiding (splitAt)


main :: IO ()
main = readFile "input.txt" >>= print . sumFish . age 256 . reverse . readGroups . map read . ( `splitAt` ',')

splitAt :: Eq a => [a] -> a -> [[a]]
splitAt [] _  = []
splitAt xs m  = fst : drop 1 lst `splitAt` m where
    (fst,lst) = break (==m) xs

sumFish :: [(a, Int)] -> Int
sumFish = foldr ((+) . snd) 0

readGroups :: [Int] -> [(Int,Int)]
readGroups fs = read' 8 fs where
    read' n fs | n < 0     = []
               | otherwise = (n, length $ filter (==n) fs) : read' (n-1) fs

age :: Int -> [(Int,Int)] -> [(Int,Int)]
age 0 fs = fs
age n fs = age (n-1) $ oneDay fs

oneDay :: [(Int,Int)] -> [(Int,Int)]
oneDay fs = birth $ decAll fs

decAll :: Num a => [(a, b)] -> [(a, b)]
decAll []         = []
decAll ((d,n):fs) = (d-1,n) : decAll fs

birth :: [(Int,Int)] -> [(Int,Int)]
birth []                     = []
birth ((n,a):fs) | n >= 0    = (n,a):fs
                 | otherwise = birth' a fs where
                     birth' b [] = [(8,b)]
                     birth' b ((n,a):fs) | n == 6 = (n,a+b) : birth' b fs
                                         | n == 8 = [(n,a+b)]
                                         | otherwise        = (n,a)   : birth' b fs