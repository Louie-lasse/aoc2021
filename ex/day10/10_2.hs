import Data.Maybe
import Prelude hiding (Either,Right,Left)
import CostCentre (CostCentre(cc_loc))
main = readFile "input.txt" >>= print .  map valueOf . filter (not . isMissmatch) . map missmatch . lines

data Either a b = Left a | Right b
    deriving Show

missmatch :: [Char] -> Either Char [Char]
missmatch cs = missmatch' cs [] where
    missmatch' :: [Char] -> [Char] -> Either Char [Char]
    missmatch' []     a                           = Right a
    missmatch' (c:cs) a | isLeftChar c            = missmatch' cs (c:a)
                        | head a == leftVersion c = missmatch' cs (tail a)
                        | otherwise               = Left c

isLeftChar :: Char -> Bool
isLeftChar '(' = True
isLeftChar '<' = True
isLeftChar '[' = True
isLeftChar '{' = True
isLeftChar _   = False

leftVersion :: Char -> Char
leftVersion c = case c of
    ')' -> '('
    ']' -> '['
    '}' -> '{'
    '>' -> '<'
    c   -> error $ "failed to parse " ++ [c]

missmatchValues :: [Either Char [Char]] -> [Int]
missmatchValues ms = map valueOf $ filter isLeft ms

valueOf :: Either Char [Char] -> Int
valueOf c = case c of
    Left ')' -> 3
    Left ']' -> 57
    Left '}' -> 1197
    Left '>' -> 25137
    Right cs -> completeValueOf cs
    _        -> error "not implemented in 'valueOf'"

completeValueOf :: [Char] -> Int
completeValueOf cs = foldr ((\a b -> b*5+a) . missingVal) 0 $ reverse cs
    where
        missingVal :: Char -> Int
        missingVal c = case c of
            '(' -> 1
            '[' -> 2
            '{' -> 3
            '<' -> 4
            c   -> error $ "no match found in 'missingVal' for " ++ [c]

isMissmatch :: Either Char [Char] -> Bool
isMissmatch = isLeft

isLeft :: Either Char [Char] -> Bool
isLeft (Left _) = True
isLeft _        = False

mid :: Ord p => [p] -> p
mid [] = error "mid of no elems"
mid as = qs as !! (length as `div` 2)

qs :: Ord a => [a] -> [a]
qs [] = []
qs (x:xs) = (qs [e | e <- xs, e < x]) ++ x : qs [e | e <- xs, e >= x]