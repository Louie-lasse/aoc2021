import Prelude hiding (Left,Right)
import Parsing
import Data.List (intersperse, intercalate)

main = do
    nums <- map parseSnail . lines <$> readFile "tst.txt"
    print $ simplify $ foldr1 add nums

data SnailNum = Num Int | Arr SnailNum SnailNum

magnitude :: SnailNum -> Int
magnitude (Arr s1 s2) = 3 * magnitude s1 + 2 * magnitude s2
magnitude (Num n)     = n

data Instr = Ins Dir Int
    deriving Show
data Dir = Left | Right
    deriving Show
type Depth = Int

isLeft :: Instr -> Bool
isLeft (Ins Left _) = True
isLeft _            = False

isRight :: Instr -> Bool
isRight = not . isLeft

instance Show SnailNum where
    show = showSnail

val :: SnailNum -> Int
val (Num n) = n
val _       = error "called 'val' with an array"

showSnail :: SnailNum -> String
showSnail (Num i) = show i
showSnail (Arr s1 s2) = '[':show s1++',':show s2++"]"

parseSnail :: String -> SnailNum
parseSnail s | Just (n,"") <- parse parseNumber s = n
             | otherwise                          = error $ "Failed to parse "++s

parseDouble :: Parser SnailNum
parseDouble = Arr <$> parseNumber <* char ',' <*> parseNumber

parseNumber :: Parser SnailNum
parseNumber = Num <$> readsP <|> char '[' *> parseDouble <* char ']'

add :: SnailNum -> SnailNum -> SnailNum
add s1 = simplify . Arr s1

simplify :: SnailNum -> SnailNum
simplify s | Just (s',_) <- explode s 1 = simplify s'
           | Just s'     <- split s     = simplify s'
           | otherwise                  = s

e1 = parseSnail "[1,2]"
e2 = parseSnail "[[1,2],3]"
e3 = parseSnail "[[[1,2],3],4]"
e4 = parseSnail "[[[[1,2],3],4],5]"
e5 = parseSnail "[[[[[1,2],3],4],5],6]"
e6 = parseSnail "[[[[[[1,2],3],4],5],6],7]"

explode :: SnailNum -> Int -> Maybe (SnailNum,[Instr])
explode (Arr s1 s2) d | d >= 4 = Just (Num 0, [Ins Left (val s1), Ins Right (val s2)])
                      | Just (s',[l,r]) <- explode s1 (d+1) = Just (Arr s' $  applyIns s2 r,[l])
                      | Just (s',[l,r]) <- explode s2 (d+1) = Just (Arr (applyIns s1 l) s', [r])
                      | Just (s',[l])   <- explode s1 (d+1) = 
explode _ _ = Nothing

ex' (Arr s1 s2) d | d >= 4 = Just d
                  | Just n <- ex' s1 (d+1) = Just n
                  | Just n <- ex' s2 (d+1) = Just n
ex' _ _ = Nothing

applyIns :: SnailNum -> Instr -> SnailNum
applyIns (Num n) (Ins _ v) = Num $ n + v
applyIns (Arr s1 s2) i     | isLeft i  = Arr s1 $ applyIns s2 i
                           | isRight i = Arr (applyIns s1 i) s2
                           | otherwise = error $ "unknown istruction "++show i

split :: SnailNum -> Maybe SnailNum
split (Arr s1 s2) | Just s' <- split s1      = Just $ Arr s' s2
                  | Just s' <- split s2      = Just $ Arr s1 s'
split (Num n)     | Just (l,h) <- splitNum n = Just (Arr (Num l) (Num h))
split _                                      = Nothing

splitNum :: Int -> Maybe (Int,Int)
splitNum n | n < 10    = Nothing
           | even n    = Just (s,s)
           | otherwise = Just (s,s+1)
           where
               s = n `div` 2