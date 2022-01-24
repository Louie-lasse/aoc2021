import Data.Char (digitToInt)

main = do
    mtrx <- readFile "input.txt" >>= (return . map (map digitToInt)) . lines
    let len = length mtrx
    return $ product $ take 3 $ reverse $ qs $ basinSizes (localLows len $ createPoints mtrx) mtrx

data Point = P { val   :: Int
               , left  :: Bool
               , right :: Bool
               , up    :: Bool
               , down  :: Bool
               }
               deriving Show

type Map = [[Bool]]

type World = [[Int]]

type Coord = (Int,Int)

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

localLows :: Int -> [Point] -> [Coord]
localLows s ps = filterByList (map allValid ps) [(x,y) | y <- [0..s-1], x <- [0..s-1]]

filterByList :: [Bool] -> [a] -> [a]
filterByList (b:bs) (x:xs) | b         = x : filterByList bs xs
                           | otherwise = filterByList bs xs
filterByList _      _                  = []

fullMap :: Int -> Map
fullMap s = replicate s $ replicate s True

basinSizes :: [Coord] -> World -> [Int]
basinSizes cs w = basins cs $ fullMap $ length w where
    basins :: [Coord] -> Map -> [Int]
    basins []     _   = []
    basins (c:cs) map = size : basins cs newMap
        where
            (size,newMap) = walk c w map

validPos :: Coord -> Map -> World -> Bool
validPos c map w = canWalkOn c map && isHole c w

canWalkOn :: Coord -> Map -> Bool
canWalkOn c m = get m c False

isHole :: Coord -> World -> Bool
isHole c w = get w c 9 /= 9

get :: [[a]] -> (Int,Int) -> a -> a
get []     _     b             = b
get (r:rs) (x,y) b | y /= 0    = get rs (x,y-1) b
                   | otherwise = get' r x
              where
                get' []     _ = b
                get' (i:is) 0 = i
                get' (i:is) x = get' is (x-1)

walk :: Coord -> World -> Map -> (Int,Map)
walk c w m | not $ validPos c m w = (0,m)
           | otherwise            = (rW+dW+lW+uW+1,uM)
    where
        (rW,rM) = walk (rightOf c) w (crossOut c m)
        (dW,dM) = walk (under c)   w rM
        (lW,lM) = walk (leftOf c)  w dM
        (uW,uM) = walk (over c)    w lM

leftOf :: Coord -> Coord
leftOf (x,y) = (x-1,y)
rightOf :: Coord -> Coord
rightOf (x,y) = (x+1,y)
over :: Coord -> Coord
over (x,y) = (x,y-1)
under :: Coord -> Coord
under (x,y) = (x,y+1)

crossOut :: Coord -> Map -> Map
crossOut (x,y) m | (uR, r : rs) <- splitAt y m
                 , (lE, e : es) <- splitAt x r
                 = uR ++ (lE ++ False : es) : rs
                 | otherwise = error "Tried to index matrix outside range"

qs :: Ord a => [a] -> [a]
qs []     = []
qs (x:xs) = qs [e | e <- xs, e < x] ++ x : qs [e | e <- xs, e >=x]

{-
evalPos :: Coord -> [Int]
evalPos cs = eval' cs trueMap where
    eval' (c:cs) map | map `get` c && world `get` c /=9 = bs : eval' cs newMap where
        (bs,newMap) = walkOnGraph c map
-}