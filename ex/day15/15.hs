import Data.Functor ( (<&>) )
import Data.Char (digitToInt)
main = do
    wrld <- readFile "tst.txt" <&> map (map digitToInt) . lines
    print $ walkToEnd wrld

type World = [[Int]]
type Coord = (Int,Int)
type Map   = [[Bool]]
type Pair  = (Int,Coord)


walkToEnd :: World -> Int
walkToEnd w = walk [(0,(0,0))] (x,y) w (createMap w) where
    x = length (head w) - 1
    y = length w        - 1


createMap :: World -> Map
createMap w = [createRow r | r <- w] where
    createRow = flip replicate True . length

crossOut :: Map -> Coord -> Map
m `crossOut` (x,y) | (uR,tR:lR) <- splitAt y m
                   , (lE,tE:rE) <- splitAt x tR
                   = uR ++ (lE ++ False : rE) : lR
                   | otherwise = error $ "can't cross out at "++show (x,y)

get :: [[a]] -> Coord -> a -> a
get w (x,y) b | x < 0 || y < 0 = b
              | (uR,tR:lR) <- splitAt y w
              , (lE,tE:rE) <- splitAt x tR
              = tE
              | otherwise = b

val :: World -> Coord -> Int
val w c = get w c (error $ "can't get at "++show c)

canWalkOn :: Map -> Coord -> Bool
canWalkOn m c = get m c False

neighbors :: Pair -> World -> Map -> [Pair]
neighbors (v,c) w m = [(v + val w n, n) | n <- others, canWalkOn m n] where
    others = map (combine c) [(1,0),(0,1)]

combine :: Coord -> Coord -> Coord
combine (a,b) (x,y) = (a+x,b+y)

walk :: [Pair] -> Coord -> World -> Map -> Int
walk ((v,c):ns) t w m | c == t    = v
                      | otherwise = walk newLst t w newMap where
                          newMap = crossOut m c
                          newLst = foldr (+:) ns $ neighbors (v,c) w newMap
walk []         _ _ _             = error "failed to walk"

(+:) :: Ord a => a -> [a] -> [a]
e +: [] = [e]
e +: (o:os) | e <= o    = e:o:os
            | otherwise = o : (e+:os)