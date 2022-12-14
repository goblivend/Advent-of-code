module Main where

import Data.Char
import Data.List
import Data.Containers.ListUtils
import Debug.Trace

data Land = Void | Rock | Sand deriving (Eq)

instance Show Land where
    show Void = " "
    show Rock = "#"
    show Sand = "o"

showLand :: [Land] -> String
showLand [] = []
showLand (e:l) = (show e) ++ showLand l


data Map = Map { lpad :: Int, slice :: [[Land]] } deriving (Eq)

instance Show Map where
    show (Map lp sl) = "Lpad = " ++ (show lp) ++ "\n" ++ (concat $ map (\l -> "\n" ++ showLand l) sl)

parseLine :: String -> [(Int, Int)]
parseLine [] = []
parseLine (' ':'-':'>':' ':l) = parseLine l
parseLine l = (read $ takeWhile isDigit l, read (takeWhile isDigit . drop 1 $ dropWhile isDigit l) :: Int):parseLine (dropWhile (/= ' ') l)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x l = take n l ++ [x] ++ drop (n+1) l

generateRock :: Map -> (Int, Int) -> Map
generateRock (Map lpad slice) (x, y) = Map lpad $ replaceAt y (replaceAt (x-lpad) Rock (slice !! y)) slice

generateRocks :: Map -> [(Int, Int)] -> Map
generateRocks mp [] = mp
generateRocks mp (_:[]) = mp
generateRocks mp (e1:e2:l) = generateRocks newMp (e2:l)
    where
        newMp = (foldl generateRock mp [(x, y) | x <- xrange, y <- yrange])
        xrange = [minimum [fst e1, fst e2] .. maximum [fst e1, fst e2]]
        yrange = [minimum [snd e1, snd e2] .. maximum [snd e1, snd e2]]

pourSand :: Map -> (Int, Int) -> Map
pourSand (Map lpad slice) (x, y)
    | (y+1) == length slice = Map lpad slice
    | slice !! (y+1) !! (x-lpad)   == Void = pourSand (Map lpad slice) (x, y+1)
    | slice !! (y+1) !! (x-lpad-1) == Void = pourSand (Map lpad slice) (x-1, y+1)
    | slice !! (y+1) !! (x-lpad+1) == Void = pourSand (Map lpad slice) (x+1, y+1)
    | otherwise = Map lpad $ replaceAt y (replaceAt (x-lpad) Sand (slice !! y)) slice
        where newMap = Map lpad $ replaceAt y (replaceAt (x-lpad) Sand (slice !! y)) slice

pour :: Map -> Int
pour mp
    | mp /= newMp = 1+ pour newMp
    | otherwise = 0
        where newMp = pourSand mp (500, 0)

printPour :: Map -> Map
printPour mp
    | mp /= newMp = printPour newMp
    | otherwise = mp
        where newMp = pourSand mp (500, 0)


main :: IO ()
main = do
    content <- readFile "shortinput.txt"
    let rocksPositions = map parseLine $ lines content
    let maxY = (+) 2 $ maximum $ map snd $ concat rocksPositions
    let maxX = (+) 5 $ maximum $ map fst $ concat rocksPositions
    let minX = (-) (minimum $ map fst $ concat rocksPositions) 5
    let lp = minX-maxY
    let mp = Map lp $ replicate (maxY+1) $ replicate (maxX-lp+maxY) Void
    let newMp = foldl generateRocks mp rocksPositions
    print $ pour newMp
    --print maxY
    let filledmp = generateRocks newMp [(lp, maxY), (lp + length (slice newMp !! 0)-1, maxY)]
    --print $ filledmp
    --print $ printPour filledmp
    print $ pour filledmp
    print $ length $ (slice filledmp) !! 0
