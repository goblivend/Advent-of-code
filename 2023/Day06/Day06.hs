module Main where

import Data.List (sortOn)
import Data.List.Split ( chunksOf, splitOn )
import Data.Set (Set, fromList, singleton, empty, unions)
import qualified Data.Set as S (map)

type Race = (Double, Double)

parseInput1 :: String -> [Race]
parseInput1 =  (\[x, y] -> zip x y) . map (map read . filter (/= "") . words . drop (length "Distance: ")) . lines

parseInput2 :: String -> Race
parseInput2 =  (\[x, y] -> (x, y)) . map (read . concat . filter (/= "") . words . drop (length "Distance: ")) . lines

solve :: Race -> Int
solve (t, d) = root (+) - root (-)
    where
        root op = floor $ (/2) $ op t $ sqrt (t ^ 2 - 4 * d)

part1 :: String -> Int
part1 = product . map solve . parseInput1

part2 :: String -> Int
part2 = solve . parseInput2

main :: IO ()
main = do
    print "Starting"
    content <- readFile "input.txt"
    print $ part1 content -- 2612736
    print $ part2 content -- 29891250
