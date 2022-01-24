import Data.Maybe
main = readFile "input.txt" >>= print . sum . missmatchValues . map missmatch . lines

missmatch :: [Char] -> Maybe Char
missmatch cs = missmatch' cs [] where
    missmatch' :: [Char] -> [Char] -> Maybe Char
    missmatch' []     _            = Nothing
    missmatch' (c:cs) a | isLeft c = missmatch' cs (c:a)
                        | head a == leftVersion c = missmatch' cs (tail a)
                        | otherwise = Just c

isLeft :: Char -> Bool
isLeft '(' = True
isLeft '<' = True
isLeft '[' = True
isLeft '{' = True
isLeft _   = False

leftVersion :: Char -> Char
leftVersion c = case c of
    ')' -> '('
    ']' -> '['
    '}' -> '{'
    '>' -> '<'
    c   -> error $ "failed to parse " ++ [c]

missmatchValues :: [Maybe Char] -> [Int]
missmatchValues ms = map valueOf $ filter isJust ms

valueOf :: Maybe Char -> Int
valueOf c = case c of
    Just ')' -> 3
    Just ']' -> 57
    Just '}' -> 1197
    Just '>' -> 25137
    r        -> error $ "failed to parse " ++ show r