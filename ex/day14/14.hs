import Data.Functor ( (<&>) )
import Data.List (group, groupBy)

main = do
    ([org],_:rules') <- readFile "input.txt" <&> break (==[]) . lines
    let rules = map parseRule rules'
    let pairs = countPairs org
    return $ value (head org) $ nTimes 40 (`applyRules` rules) pairs

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ = id
nTimes n f = f . nTimes (n-1) f

type Count = (Pair,Int)
type Pair  = (Char,Char)
type Rule  = (Pair, (Pair,Pair))

countPairs :: String -> [Count]
countPairs (c1:c2:s) = add ((c1,c2),1) $ countPairs (c2:s)
countPairs _         = []

parseRule :: String -> Rule
parseRule s | (a:b:_,'>':' ':[r]) <- break (=='>') s = ((a,b),((a,r),(r,b)))
            | otherwise = error $ "failed to parse string "++s

applyRules :: [Count] -> [Rule] -> [Count]
applyRules []         rs = []
applyRules ((p,n):cs) rs | Just (p1,p2) <- lookup p rs
                         = add (p1,n) $ add (p2,n) $ applyRules cs rs
                         | otherwise = error $ "Can't find rule for "++show p

add :: Count -> [Count] -> [Count]
add (p,n) l | (lE,(_,a):rE) <- break ((==p) . fst) l = lE ++ (p,n+a) : rE
            | otherwise = (p,n) : l

value :: Char -> [Count] -> Int
value f cs = last vals - head vals where
    vals = qs $ map (sum . map snd) letterCount
    letterCount = groupBy (\a b -> fst a == fst b) $ qs $ (f,1) : zip (map (snd . fst) cs) (map snd cs)

{-
value :: [Count] -> Int
value cs = head sorted - last sorted where
    sorted = reverse $ qs $ map snd cs
-}

qs :: Ord a => [a] -> [a]
qs []     = []
qs (x:xs) = qs [e | e <- xs, e < x] ++ x : qs [e | e <- xs, e >= x]