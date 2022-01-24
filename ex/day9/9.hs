import Data.Char (digitToInt)

main = do
    lns <- lines <$> readFile "input.txt"
    let mtrx = map (map digitToInt) lns
    return $ sum $ map ((1+) . val) $ truePoints $ createPoints mtrx

data Point = P { val   :: Int
               , left  :: Bool
               , right :: Bool
               , up    :: Bool
               , down  :: Bool
               }
               deriving Show

data Tst = T { i  :: Int
             , b  :: Bool
             , b2 :: Bool
             }
    deriving (Show)

truePoints :: [Point] -> [Point]
truePoints []                  = []
truePoints (p:ps) | allValid p = p : truePoints ps
                  | otherwise  = truePoints ps

allValid :: Point -> Bool
allValid (P i l r u d) = and [l,r,u,d]

createPoints :: [[Int]] -> [Point]
createPoints ints = ans where
    ans  = applyAll t2   $ concat $ sTDown  ints
    t2   = applyAll t1   $ concat $ sTUp    ints
    t1   = applyAll t0   $ concat $ sTRight ints
    t0   = applyAll cons $ concat $ sTLeft ints
    cons = applyAll [P | n <- [1..length $ concat ints]] $ concat ints

sTDown :: [[Int]] -> [[Bool]]
sTDown mtrx = transpose $ map sTNext $ transpose mtrx

sTUp :: [[Int]] -> [[Bool]]
sTUp mtrx = transpose $ map sTLast $ transpose mtrx

sTRight :: [[Int]] -> [[Bool]]
sTRight = map sTNext

sTLeft :: [[Int]] -> [[Bool]]
sTLeft = map sTLast

sTNext :: [Int] -> [Bool]
sTNext r = zipWith (<) r (tail r) ++ [True]

sTLast :: [Int] -> [Bool]
sTLast r = True : zipWith (>) r (tail r)

applyAll :: [a->b] -> [a] -> [b]
applyAll (f:fs) (x:xs) = f x : applyAll fs xs
applyAll _      _      = []

transpose :: [[a]] -> [[a]]
transpose mtrx = transpose' (length mtrx) mtrx where
    transpose' s []     = replicate s []
    transpose' s (r:rs) = zipWith (:) r $ transpose' s rs