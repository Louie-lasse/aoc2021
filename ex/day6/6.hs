import Prelude hiding (splitAt)

main :: IO ()
main = readFile "input.txt" >>= print . length . age 256 . map (read :: String -> Int) . ( `splitAt` ',')

age :: Int -> [Int] -> [Int]
age 0 fs = fs
age d fs = age (d-1) (oneDay fs)

oneDay :: [Int] -> [Int]
oneDay fs = birth $ map (\f -> f-1) fs

birth :: [Int] -> [Int]
birth []                 = []
birth (f:fs) | f < 0     = 6:8:birth fs
             | otherwise = f:birth fs

splitAt :: Eq a => [a] -> a -> [[a]]
splitAt [] _  = []
splitAt xs m  = fst : drop 1 lst `splitAt` m where
    (fst,lst) = break (==m) xs