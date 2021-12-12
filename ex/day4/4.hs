module Main(main) where
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
main = do
    txt <- lines <$> readFile "input.txt"
    let (draw, bingos) = (parseDraw $ head txt, parseAllBingos $ tail txt)
    let (looser,number) = lastToWin bingos draw
    return $ parseNumber looser number

parseNumber :: Bingo -> String -> Int
parseNumber b n = sumOfUnMarked b * read n

sumOfUnMarked :: Bingo -> Int
sumOfUnMarked (Bingo rs) = sum $ map read $ filter (/="") $ concat rs

tst (Bingo rs) = concat rs

type Row   = [String]
newtype Bingo = Bingo [Row]

instance Show Bingo where
    show = showBingo

showBingo :: Bingo -> String
showBingo (Bingo [])     = ""
showBingo (Bingo (r:rs)) = showRow r ++ "\n" ++ show (Bingo rs) where
    showRow []                 = ""
    showRow (i:is) | length i < 2 = " " ++show i ++ " " ++ showRow is
                   | otherwise    = show i ++ " " ++ showRow is

parseDraw :: String -> [String]
parseDraw = splitWhen (==',')

parseAllBingos :: [String] -> [Bingo]
parseAllBingos ss = map (Bingo . map (filter(/="") . splitWhen (==' '))) $ filter (/=[]) $ splitWhen (=="") ss

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f xs = splitWhen' f xs [] where
    splitWhen' f []     acc             = [reverse acc]
    splitWhen' f (x:xs) acc | f x       = reverse acc : splitWhen' f xs []
                            | otherwise = splitWhen' f xs (x:acc)

blocks :: Bingo -> [Row]
blocks (Bingo rows) = rows ++ [map (!! n) rows | n <- [0..4]]

bingoRemove :: Bingo -> String -> Bingo
bingoRemove (Bingo rs) s = Bingo (map (replaceInRow s) rs) where
    replaceInRow s []     = []
    replaceInRow s (e:es) | s == e    = "":es
                          | otherwise = e:replaceInRow s es

winningRow :: [Bingo] -> Maybe Row
winningRow []     = Nothing
winningRow (b:bs) = winningRow' (blocks b) <|> winningRow bs where
    winningRow' []                    = Nothing
    winningRow' (r:rs) | all (=="") r = Just r
                      | otherwise    = winningRow' rs

isWinner :: Bingo -> Bool
isWinner b = any (all (=="")) $ blocks b

winningBingo :: [Bingo] -> Maybe Bingo
winningBingo []                  = Nothing
winningBingo (b:bs) | isWinner b = Just b
                    | otherwise  = winningBingo bs

getFirstWinner :: [Bingo] -> [String] -> (Bingo, String)
getFirstWinner bs []     = error "no solution"
getFirstWinner bs (s:ss) = case winner of
    Nothing -> getFirstWinner stricken ss
    Just r  -> (r, s)
    where
        stricken = [bingoRemove b s | b <- bs]
        winner = winningBingo stricken

removeWinners :: [Bingo] -> [Bingo]
removeWinners [] = []
removeWinners (b:bs) | isWinner b = removeWinners bs
                     | otherwise  = b : removeWinners bs

lastToWin :: [Bingo] -> [String] -> (Bingo, String)
lastToWin _  []                             = error "no last winner"
lastToWin bs (s:ss) | length remainder == 1 = getFirstWinner remainder ss
                    | otherwise             = lastToWin remainder ss
    where
        remainder = filter (not . isWinner) [bingoRemove b s | b <- bs]

{-
firstSolved bs nums = case first (allRows bs) (reverse nums) of
    Nothing    -> error "No solution found"
    Just (r,i) -> (r,i)
  where
    allRows bs = concatMap blocks bs
    first [] (i:is) = Nothing
    first rs (i:is) | length rs == 1 = first (valid $ map (remove i) rs) is <|> Just (head rs,i)
                    | otherwise      = first (valid $ map (remove i) rs) is
    first _  []                      = Nothing
    valid rs = filter (\x -> length x == 5) rs


bingoContains :: String -> Bingo -> Bool
bingoContains s (Bingo rs) = or [rowContains r s | r <- rs] where
    rowContains :: [String] -> String -> Bool
    rowContains []     _          = False 
    rowContains (e:es) s | e == s = True 
                         | otherwise = rowContains es s

firstSolved :: [Bingo] -> [String] -> (Bingo, Maybe String)
firstSolved bs []                      = error "no solution found"
firstSolved bs (s:ss) | length bs == 1 = (head bs, Nothing)
                      | otherwise      = (bingo, num)
                      where
                          num = case last of
                              Nothing -> Just s
                              Just i  -> last
                          (bingo, last) = firstSolved remain ss
                          remain = filter (bingoContains s) bs
-}