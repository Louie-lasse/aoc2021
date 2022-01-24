import Prelude hiding (and,foldr)

main = do
    depths <-  lines <$> readFile "input.txt"
    return $ increases $ trippleZip $ map toInt depths

monadlessMain = readFile "input.txt" >>= print . increases . trippleZip . map toInt . lines

trippleZip :: [Int] -> [Int]
trippleZip is = zipWith (+) (tail $ tail is) $ zipWith (+) is $ tail is

toInt :: String -> Int
toInt s = read s :: Int

increases :: [Int] -> Int
increases is = length [b | b <- zipWith (<) is $ tail is, b]