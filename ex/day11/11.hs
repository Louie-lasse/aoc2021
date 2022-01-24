import Data.Char (digitToInt)
import Data.Functor
import Data.Maybe
main = do
    mtrx <- readFile "input.txt" <&> createGrid . map (map digitToInt) . lines
    print $ indexOfAllFlash mtrx

both :: (a->b) -> (a,a) -> (b,b)
both f (a,b) = (f a,f b)

type OctoGrid = [[Maybe Int]]

type Coord = (Int,Int)

createGrid :: [[Int]] -> OctoGrid
createGrid = map (map Just)

runUpdates :: Int -> OctoGrid -> (Int,OctoGrid)
runUpdates 0 g = (0,g)
runUpdates n g = (otherLight+newLight, newGrid) where
    (newLight,newGrid)     = updateGrid otherGrid
    (otherLight,otherGrid) = runUpdates (n-1) g

updateGrid :: OctoGrid -> (Int, OctoGrid)
updateGrid g = (l,replaceNothings newGrid) where
    (l,newGrid) = update $ incAll g

update :: OctoGrid -> (Int,OctoGrid)
update g = update' g positions where
    positions = [(x,y) | y <- [0..length g -1], x <- [0..length g -1]]

    update' :: OctoGrid -> [Coord] -> (Int, OctoGrid)
    update' g []                            = (0,g)
    update' g (c:cs) | (Just n) <- get g c Nothing
                     , n >= 10 = (light+1,newGrid)
                     | otherwise = update' g cs
        where
            squid = get g c Nothing
            (light,newGrid) = update $ g `lightSquid` c

replaceNothings :: OctoGrid -> OctoGrid
replaceNothings = map (map reset) where
    reset e = case e of
        Nothing -> Just 0
        _       -> e

incAll :: OctoGrid -> OctoGrid
incAll = map incRow where
    incRow = map ( <&> (1+))

get :: [[a]] -> Coord -> a -> a
get []     _     b             = b
get (r:rs) (x,y) b | y /= 0    = get rs (x,y-1) b
                   | otherwise = get' r x
              where
                get' []     _ = b
                get' (i:is) 0 = i
                get' (i:is) x = get' is (x-1)

lightSquid :: OctoGrid -> Coord -> OctoGrid
lightSquid g c = foldr flash (setNothing c g) surrounding where
    surrounding = zipWith addTogether [c | n <- [0..8]] [(x,y) | y <- [-1..1], x <- [-1..1]]

addTogether :: Coord -> Coord -> Coord
addTogether (x1,y1) (x2,y2) = (x1+x2,y1+y2)

flash :: Coord -> OctoGrid -> OctoGrid
flash (x,y) g | x < 0 || y < 0 = g
              | (uRows,tRow:mRows)  <- splitAt y g
              , (lEs, (Just n):mEs) <- splitAt x tRow
              = uRows ++ (lEs ++ Just (n+1) : mEs) : mRows
              | otherwise = g

setNothing :: Coord -> OctoGrid -> OctoGrid
setNothing (x,y) g | (uR,tR:rs) <- splitAt y g
                   , (lE,tE:es) <- splitAt x tR
                   = uR ++ (lE ++ Nothing : es) : rs
                   | otherwise = error $ "setNothing is out of range for " ++show(x,y) ++ ", with the grid " ++ show g

indexOfAllFlash :: OctoGrid -> Int
indexOfAllFlash g | l == 100  = 1
                  | otherwise = 1 + indexOfAllFlash newGrid
                  where
                      (l,newGrid) = updateGrid g