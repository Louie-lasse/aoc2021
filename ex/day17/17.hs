import Prelude hiding (Either,Left,Right)
import Data.Functor

main = do
    range <- readFile "input.txt" <&> getRanges
    print $ length $ aim range

type Range = ((Int,Int),(Int,Int))
type Coord = (Int,Int)
type Velocity = (Int,Int)
data Hit = Left | Right | Hit [Velocity]
    deriving Show

getRanges :: String -> Range
getRanges s | (_,'=':r) <- break (=='=') s
            , (xs,',':' ':'y':'=':ys) <- break (==',') r
            = both parseRange (xs,ys)
            | otherwise = error $ "failed to get range " ++ s

parseRange :: String -> Coord
parseRange s | (f,'.':'.':l) <- break (=='.') s
             = both read (f,l)
             | otherwise = error $ "failed to parse range " ++ s

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a,f b)

aim :: Range -> [Velocity]
aim r = aim' (0,biggestY r) r

aim' :: Velocity -> Range -> [Velocity]
aim' v r = case shoot (0,0) v r of
    Left  -> aim' (add (1,0) v) r
    Right -> lowerShot v r
    Hit vs -> v : aim' (add (1,0) v) r

biggestY :: Range -> Int
biggestY (_,y) = max (abs $ fst y) (abs $ snd y)

tooStrong :: Velocity -> Range -> Bool
tooStrong (x,_) ((_,h),_) = x > h

lowerShot :: Velocity -> Range -> [Velocity]
lowerShot v r | isTooLow v r = []
              | otherwise    = aim' (0,snd v - 1) r
              where
                  isTooLow (_,y) r = y < negate (biggestY r)

shoot :: Coord -> Velocity -> Range -> Hit
shoot c v g | tooStrong v g  = Right
            | nY `isBelow` g = Left
            | (nX,nY) `isInside` g = Hit [(nX,nY)]
            | otherwise          = (nX,nY) ++: shoot (nX,nY) nV g
                where
                  (nX,nY) = add c v
                  nV = velocityChange v

velocityChange :: Velocity -> Velocity
velocityChange (x,y) = (max 0 $ x - 1, y - 1)

isBelow :: Int -> Range -> Bool
isBelow y (_,(l,_)) = y < l

isInside :: Coord -> Range -> Bool
isInside c r = inside' (fst c) (fst r) && inside' (snd c) (snd r) where
  inside' v (l,h) = v >= l && v <=h

(++:) :: Velocity -> Hit -> Hit
i ++: (Hit is) = Hit $ i:is
_ ++: m        = m

add :: (Int,Int) -> (Int,Int) -> (Int,Int)
add (a,b) (x,y) = (a+x,b+y)