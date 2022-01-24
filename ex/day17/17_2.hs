import Prelude hiding (Either,Left,Right)
import Data.Functor

main = do
    range <- readFile "input.txt" <&> getRanges
    print $ aim range

type Range = ((Int,Int),(Int,Int))
type Coord = (Int,Int)
type Velocity = (Int,Int)
data Hit = Left | Right | Hit [Int]
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

aim :: Range -> Maybe Int
aim r | Just is <- aim' (0,biggestY r) r = Just $ maximum is
      | otherwise = error "could not hit target"

aim' :: Velocity -> Range -> Maybe [Int]
aim' (_,0) _ = Nothing
aim' v r     = case shoot (0,0) v r of
    Left  -> aim' (add (1,0) v) r
    Right -> aim' (0,snd v - 1) r
    Hit is -> Just is

biggestY :: Range -> Int
biggestY (_,y) = max (abs $ fst y) (abs $ snd y)

shoot :: Coord -> Velocity -> Range -> Hit
shoot c v g | nY `isBelow` g = leftOrRight nX g
            | (nX,nY) `isInside` g = Hit [nY]
            | otherwise          = nY ++: shoot (nX,nY) nV g
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

leftOrRight :: Int -> Range -> Hit
leftOrRight x ((l,_),_) | x < l     = Left
                        | otherwise = Right

(++:) :: Int -> Hit -> Hit
i ++: (Hit is) = Hit $ i:is
_ ++: m        = m

add :: (Int,Int) -> (Int,Int) -> (Int,Int)
add (a,b) (x,y) = (a+x,b+y)
