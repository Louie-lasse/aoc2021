import Data.Char (digitToInt)
main = do
    input <- lines <$> readFile "input.txt"
    let gam = toGamma input
    return $ gam

toGamma ss = [ mostCommon ss n | n <- [0..length (head ss) - 1]]

sToInt = map digitToInt

toOxygen ss = toOx $ transpose $ map sToInt ss where
    toOx :: [[Int]] -> [Int]
    toOx []                     = []
    toOx (l:ls) | length l == 1 = map head ls
                | otherwise     = theMostCommon l : toOx ls
    theMostCommon l = if length [x | x <- l, x == 1] >= length [x | x <- l, x == 0] then 1 else 0

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x : xs) : xss) = combine x hds xs tls
  where
    (hds, tls) = unzip [(hd, tl) | hd : tl <- xss]
    combine y h ys t = (y:h) : transpose (ys:t)

mostCommon ss n =  oneMoreCommon [ s !! n | s <- ss] where
    oneMoreCommon l = if length [x | x <- l, x=='1'] > length [x | x <- l, x=='0'] then 1 else 0

fromBin :: [Int] -> Int
fromBin []     = 0
fromBin (i:is) = i * pow 2 (length is) + fromBin is where
    pow _ 0 = 1
    pow p n = p * pow p (n-1)
