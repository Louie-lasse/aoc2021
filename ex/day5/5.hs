import Data.Functor ( (<&>) )
import Graphics.UI.Threepenny (lineTo)
main = do
    lines <- readFile "input.txt" <&> map parseLine . lines
    let world = createWorld lines
    return $ countCommon $ foldr place world $ concatMap lineToCoords lines

main' = do
    readFile "tst.txt" >>= mapM_ (print . lineToCoords . parseLine) . lines

type Coord = (Int,Int)

type Line = (Coord,Coord)

type World = [[Int]]

parseLine :: String -> Line
parseLine s | (fst,' ':'-':'>':' ':snd) <- break (==' ') s
            = both parseCoord (fst,snd)
            | otherwise = error $ "failed to parse line "++s

parseCoord :: String -> Coord
parseCoord s | (x,',':y) <- break (==',') s
             = both read (x,y)
             | otherwise = error $ "failed to parse coord "++s

both :: (a->b) -> (a,a) -> (b,b)
both f (a,b) = (f a,f b)

isStraight :: Line -> Bool
isStraight ((x1,y1),(x2,y2)) = x1==x2 || y1 == y2

createWorld :: [Line] -> World
createWorld lns = replicate maxY $ replicate maxX 0 where
    maxX = 1 + maximum (concat [[x1,x2] | ((x1,_),(x2,_)) <- lns])
    maxY = 1 + maximum (concat [[y1,y2] | ((_,y1),(_,y2)) <- lns])

place :: Coord -> World -> World
place (x,y) w | (uR,tR:lR) <- splitAt y w
              , (lE,tE:rE) <- splitAt x tR
              = uR ++ (lE++tE+1:rE) : lR
              | otherwise = error $ "coudn't place at "++show (x,y)

lineToCoords :: Line -> [Coord]
lineToCoords (c1,c2) | c1 == c2 =[c1]
                      | otherwise = c1 : lineToCoords (closer c1 c2,c2) where
                          closer (x1,y1) (x2,y2) = (x1 `closerTo` x2, y1 `closerTo` y2)
                          closerTo a b | a < b     = a + 1
                                       | a > b     = a - 1
                                       | otherwise = a

countCommon :: World -> Int
countCommon w = sum $ map (length . filter (>1)) w