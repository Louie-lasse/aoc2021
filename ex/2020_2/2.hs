{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
main = do
    passes <- lines <$> readFile "input.txt"
    let parsed = map split passes
    -- return $ length [pass | (low, high, char, pass) <- parsed, pass !! (low+1) == char && pass !! (high+1) /= char]
    return [(l,h,c,p) | (l,h,c,p) <- parsed , length p < (h+1)]

split :: String -> (Int, Int, Char, String)
split s = (read l, read h, head c, p) where
    (l, s')   = chop s
    (h, s'')  = chop s'
    (c, s''') = chop s''
    (p, _)    = chop s'''
    chop s = (takeWhile (\x -> x `notElem` [' ', '-']) s, drop 1 $ dropWhile (\x -> x `notElem` [' ', '-']) s)

occ :: Char -> String -> Int 
occ cond []                 = 0
occ cond (c:cs) | c == cond = 1+occ cond cs
                | otherwise = occ cond cs