module Main where

import Data.List (sortOn, group, sortOn, sort, isSuffixOf)
import Data.List.Split ( splitOn )
import Data.Ord ()
import Data.Map (Map, (!), fromList, keys, member, insert, empty)
import Debug.Trace(trace)

type Input = [[Int]]

parseInput :: String -> Input
parseInput = map (map read . words) . lines

extrapolate :: ([Int] -> Int) -> (Int -> Int) -> [Int] -> Int
extrapolate el op = extrapolate
    where
        extrapolate arr
            | all (== 0) arr = 0
            | otherwise      = el arr + extrapolate (map (op . uncurry (-)) . zip arr $ drop 1 arr)

part1 :: Input -> Int
part1 = sum . map (extrapolate last ((-) 0))

part2 :: Input -> Int
part2 = sum . map (extrapolate head id)


main :: IO ()
main = do
    print "Starting"
    content <- readFile "input.txt"
    let inp = parseInput content
    print $ part1 inp
    print $ part2 inp
