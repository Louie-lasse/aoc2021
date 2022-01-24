import Prelude hiding (Word)
import Data.Functor ( (<&>) )
main = do
    ([org],_:rules') <- readFile "tst.txt" <&> break (==[]) . lines
    let firstMap = createMap $ map parseRule rules'
    print $ val $ fst $ addItteration org 20 firstMap

type Key      = (Char,Char)
type Rule     = (Key,Char)
type Word     = String
type Depth    = Int
type DepthMap = [(Depth,Word)]
type Map      = [(Key,DepthMap)]

parseRule :: String -> Rule
parseRule s | (a:b:_,'>':' ':[r]) <- break (=='>') s = ((a,b),r)
            | otherwise = error $ "failed to parse string "++s

createMap :: [Rule] -> Map
createMap []         = []
createMap ((k,c):rs) = (k , [(1,[c])]) : createMap rs

val :: Word -> Int
val w = head nums - last nums where
    nums = reverse $ qs $ map snd $ occurances w

qs :: Ord a => [a] -> [a]
qs []     = []
qs (x:xs) = qs [e | e <- xs, e < x] ++ x : qs [e | e <- xs, e >= x]

occurances :: Word -> [(Char,Int)]
occurances w = filter (\(_,i) -> i > 0) [(l,length $ filter (==l) w) | l <- ['A'..'Z']]

addItteration :: Word -> Depth -> Map -> (Word,Map)
addItteration (c1:c2:w) d m = (c1:newWord ++ rest,fullMap) where
    (rest,fullMap)   = addItteration (c2:w) d newMap
    (newWord,newMap) = createWord (c1,c2) d m
addItteration w         _ m = (w,m)

createWord :: Key -> Depth -> Map -> (Word,Map)
createWord _ 0 m = ("",m)
createWord k d m | (Just w) <- getWord k d m = (w,m)
                 | otherwise = (trim word,finalMap) where
                     (lowerWord,m') = createWord k (d-1) m
                     (c1,c2) = k
                     (word,m'') = addItteration (c1 : lowerWord ++ [c2]) 1 m'
                     finalMap = addWord k d (trim word) m''

trim :: Word -> Word
trim (_:w) = removeLast w where
    removeLast [e]   = []
    removeLast (h:w) = h : removeLast w
    removeLast _     = []
trim _               = error "Failed to trim"

singleDepth :: Key -> Map -> Word
singleDepth k m | Just w <- getWord k 1 m = w
                | otherwise               = ""

addWord :: Key -> Depth -> Word -> Map -> Map
addWord k d w m | (oK,(k,dM):mK) <- break ((==k) . fst) m
                = oK ++ (k,(d,w) : dM) : mK
                | otherwise = (k,[(d,w)]) : m

getWord :: Key -> Depth -> Map -> Maybe Word
getWord k d m = lookup k m >>= lookup d