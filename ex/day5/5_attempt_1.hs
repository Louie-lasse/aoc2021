{-import Parsing ( digit, oneOrMore, parse )
import CostCentre (CostCentre(cc_loc))
main :: IO ()
main = do
    ns <- map getNums . lines <$> readFile "input.txt"
    let maxN = maximum $ concat ns
    let matrix = genMatrix (maxN+1)
    let poss = concatMap (crossings . posPair) ns
    let cross = concat $ foldr inc matrix poss
    print $ length $ filter (>1) cross

type Pos = (Int,Int)
type Matrix = [[Int]]

(!!=) :: [a] -> (Int,a) -> [a]
[]     !!= _     = error "index of !!= out of bounds"
(x:xs) !!= (0,v) = v:xs
(x:xs) !!= (n,v) = x : xs !!= (n-1,v)

inc :: Pos -> Matrix -> Matrix
inc (r,c) m = m !!= (r, m !! r !!= (c,m !! r !! c + 1))

genMatrix :: Int -> [[Int]]
genMatrix n = replicate n [0 | _ <- [0..n]]

getNums :: String -> [Int]
getNums [] = []
getNums s  = case parse (oneOrMore digit) s of
    Just (n,r) -> read n : getNums r
    Nothing    ->          getNums $ drop 1 s

posPair :: [Int] -> (Pos,Pos)
posPair [x1,y1,x2,y2] = ((x1,y1),(x2,y2))
posPair _             = error "not a valid list of positions"

crossings :: (Pos,Pos) -> [Pos]
crossings (p1,p2) | p1 == p2  = [p1]
                  | otherwise = p1 : crossings (p1 `closer` p2,p2)
    where
        closer (x1,y1) (x2,y2) = (x1 + diff x1 x2, y1 + diff y1 y2)
        diff a b | a < b     =  1
                 | a > b     = -1
                 | otherwise =  0

-}