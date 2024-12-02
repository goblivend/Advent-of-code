module Main where

import Data.List
import Data.List.Split
import Debug.Trace

type Input = [[Int]]

parseInput :: String -> Input
parseInput  = map (map read . splitOn " ") . lines

fixReport :: [Int] -> [Int]
fixReport report = (if generallyIncreasing report then id else reverse) $ report
    where
        generallyIncreasing report = (>) (div (length report)  2) . length . filter (> 0) . zipWith (-) report $ tail report


part1 :: Input -> Int
part1 = length . filter isSafe
    where
        signFilter l1 l2 = foldl (\res e -> res && (e < 0)) True $ zipWith (-) l1 l2
        decreasing report = signFilter (drop 1 report) report
        increasing report = signFilter report $ drop 1 report
        diff report =  foldl (\res e -> res && (abs e <= 3) && (abs e >= 1)) True $ zipWith (-) report $ tail report
        isSafe report = (increasing $ fixReport report) && (diff report)

part2:: Input -> Int
part2 = length . filter (checkValidity . fixReport)
    where
        checkValidity report = isValid (head report) (drop 1 report) 0 || isValid (head $ drop 1 report) (drop 2 report) 1

        isValid e [] nbSkip = nbSkip <= 1
        isValid e (h:t) nbSkip
            | nbSkip > 1 = False
            | e < h && abs (e-h) <= 3 = (isValid h t nbSkip) || isValid e t (nbSkip+1)
            | otherwise = isValid e t (nbSkip+1)

main :: IO()
main = do
    content <- readFile "input.txt"
    let input = parseInput  content
    print $ (== 252) $  part1 input
    print $ (== 324)$ part2 input
