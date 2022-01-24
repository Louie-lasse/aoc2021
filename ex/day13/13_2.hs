import Data.Functor
import Data.Char (isDigit)
main = do
    (vals,ins) <- readFile "input.txt" <&> break (=='f')
    let paper = makePaper $ map parseValue $ filter (not . null) $ lines vals
    let folds = map readFold $ lines ins
    mapM_ (print . map makeVisible) $ foldr foldPaper paper (reverse folds)

makeVisible b | b = '#'
              | otherwise = '.'

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
    maxX = oddVersion $ maximum (map fst cs) + 1
    maxY = oddVersion $ maximum (map snd cs) + 1

oddVersion :: Integral p => p -> p
oddVersion n | even n    = n + 1
             | otherwise = n

place :: Coord -> Paper -> Paper
place (x,y) p | (uR,tR:mR) <- splitAt y p
              , (lE,_:mE)  <- splitAt x tR
              = uR ++ (lE++True:mE) : mR
              | otherwise = error $ "index "++ show (x,y)++"out of range in 'place'"

count :: Paper -> Int
count = sum . map count' where
    count' = sum . map fromEnum

foldPaper :: Fold -> Paper -> Paper
foldPaper (Y i) p | (uR,fR:lR) <- splitAt i p = zipWith combine uR $ reverse lR
                  | otherwise = error $ "couldn't fold paper at index "++show i++" with paper length"++show (length p)
                where
                    combine = zipWith (||)
foldPaper (X i) p = map (combineRow i) p
    where
    combineRow i r | (lE,fE:rE) <- splitAt i r = zipWith (||) lE $ reverse rE
                   | otherwise                 = error $ "couldn't split row "++show r++" at index"++show i