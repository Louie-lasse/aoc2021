import Data.Functor
import Data.Char (isDigit)
main = do
    (vals',ins') <- readFile "input.txt" <&> break (=='f')
    let paper = makePaper $ map parseValue $ filter (not . null) $ lines vals'
    let folds = map readFold $ lines ins'
    return $ length $ foldr foldPaper paper (take 2 folds)

data Fold = Y Int | X Int
    deriving Show

type Coord = (Int,Int)

type Paper = [[Bool]]

readFold :: String -> Fold
readFold s = case var of
    'x' -> X val
    'y' -> Y val
    c   -> error $ "no value derived from "++c:" in readFold"
    where
        (txt,ints) = break isDigit s
        val = read ints
        var = txt !! 11


parseValue :: String -> Coord
parseValue s | (x,',':y) <- break (==',') s = both read (x,y)
             | otherwise                    = error $ "couldn't parse "++s++" in 'parseValue'"

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a,f b)

makePaper :: [Coord] -> Paper
makePaper cs = foldr place blank cs where
    blank = replicate maxY $ replicate maxX False
    maxX = maximum (map fst cs) + 1
    maxY = maximum (map snd cs) + 1

place :: Coord -> Paper -> Paper
place (x,y) p | (uR,tR:mR) <- splitAt y p
              , (lE,_:mE)  <- splitAt x tR
              = uR ++ (lE++True:mE) : mR
              | otherwise = error $ "index "++ show (x,y)++"out of range in 'place'"

count :: Paper -> Int
count = sum . map count' where
    count' = sum . map fromEnum


foldPaper :: Fold -> Paper -> Paper
foldPaper (X i) p = map (\r -> zipWith (||) (take i r) (reverse $ drop i r)) p
foldPaper (Y i) p = zipWith combine (take i p) (reverse $ drop i p) where
    combine = zipWith (||)