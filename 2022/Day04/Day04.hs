module Main where

import Data.Char
import Data.List
import Data.List.Split

pairIncluded :: (Int, Int) -> (Int, Int) -> Int
pairIncluded (a, b) (c, d)
    | a <= c && d <= b = 1
    | c <= a && b <= d = 1
    | otherwise = 0

pairOverlap :: (Int, Int) -> (Int, Int) -> Int
pairOverlap (a, b) (c, d)
    | a <= c && c <= b = 1
    | a <= d && d <= b = 1
    | c <= a && a <= d = 1
    | c <= b && b <= d = 1
    | otherwise = 0

readpairs :: ((Int, Int) -> (Int, Int) -> Int) -> [[String]] -> Int
readpairs check ((a:b:_):(c:d:_):_) = check (read a, read b) (read c, read d)
readpairs _ _ = 0


main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = map (\line -> map (splitOn "-") $ splitOn "," line) $ lines content
    print $ sum $ map (readpairs pairIncluded) input
    print $ sum $ map (readpairs pairOverlap) input
