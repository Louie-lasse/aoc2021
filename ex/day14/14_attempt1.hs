import Test.QuickCheck
import Data.Functor
import Prelude hiding (Word)
import Data.List (nub)

main = do
    ([org],_:rules') <- readFile "tst.txt" <&> break (==[]) . lines
    let rules = map parseRule rules'
    let trees10 = allTenTree' rules
    let trees20 = twentyTree trees10
    print $ val $ nTimes 2 (`buildWord` trees20) org

type Key  = (Char,Char)
type Rule = (Key,Char)
type Word = String

parseRule :: String -> Rule
parseRule s | (a:b:_,'>':' ':[r]) <- break (=='>') s = ((a,b),r)
            | otherwise = error $ "failed to parse string "++s

qs :: Ord a => [a] -> [a]
qs []     = []
qs (x:xs) = qs [e | e <- xs, e < x] ++ x : qs [e | e <- xs, e >= x]

prop_qs :: Ord a => [a] -> Bool
prop_qs as = 0 == decs (qs as) where
    decs as = length $ filter id $ zipWith (>) as $ tail as

applyRules :: [Rule] -> Word -> Word
applyRules rs (c1:c2:w) = apply rs c1 c2 ++ applyRules rs (c2:w) where
    apply :: [Rule] -> Char -> Char -> Word
    apply rs c1 c2 | Just a <- lookup (c1,c2) rs = [c1,a]
                    | otherwise                   = [c1]
applyRules rs w       = w

val :: Word -> Int
val w = head nums - last nums where
    nums = reverse $ qs $ map snd $ occurances w

occurances :: Word -> [(Char,Int)]
occurances w = filter (\(_,i) -> i > 0) [(l,length $ filter (==l) w) | l <- ['A'..'Z']]

nTimes :: Int -> (a->a) -> a -> a
nTimes 0 _ = id
nTimes n f = f . nTimes (n-1) f

type Depth = Int

createWord :: String -> [Rule] -> Depth -> Word
createWord (c1:c2:w) rs d = c1 : tree (c1,c2) rs d ++ createWord (c2:w) rs d
createWord w         _  _ = w

tree :: Key -> [Rule] -> Depth -> Word
tree (c1,c2) rs d | d == 0 = ""
                  | (Just m) <- lookup (c1,c2) rs
                  = tree (c1,m) rs (d-1) ++ m : tree (m,c2) rs (d-1)
                  | otherwise = ""

tenTree :: Key -> [Rule] -> Word
tenTree cs rs = tree cs rs 10

twentyTree :: [(Key,Word)] -> [(Key,Word)]
twentyTree []         = []
twentyTree ((k,w):es) = (k,buildWord w ((k,w):es)) : twentyTree es

allTenTree' :: [Rule] -> [(Key,Word)]
allTenTree' rs = [(lc, tenTree lc rs) | lc <- map fst rs]

buildWord :: String -> [(Key,Word)] -> Word
buildWord (c1:c2:w) m | (Just res) <- lookup (c1,c2) m
                      = c1 : res ++ buildWord (c2:w) m
                      | otherwise = c1 : buildWord (c2:w) m
buildWord w         _ = w