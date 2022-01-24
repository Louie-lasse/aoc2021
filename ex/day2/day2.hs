main = do
    txt <- lines <$> readFile "input.txt"
    let (h,d,a) = parseDim txt
    return $ h * d

parseDim s = parse' s (0,0,0) where
    parse' []     t      = t
    parse' (s:ss) (h,d,a)   | ins == "forward" = parse' ss (h+val,d+val*a,a)
                            | ins == "down"    = parse' ss (h,d,a+val)
                            | ins == "up"      = parse' ss (h,d,a-val)
                            | otherwise        = parse' ss (h,d,a)
                            where
                                (ins,val) = split s

split :: [Char] -> ([Char], Int)
split s = (takeWhile (/=' ') s, read $ drop 1 $ dropWhile (/=' ') s :: Int)