module Main where

import Data.List
import Data.List.Split
import Debug.Trace

type Input = [[Int]]

parseInput :: String -> Input
parseInput = map (map read . splitOn " ") . lines

fixReport :: [Int] -> [Int]
fixReport report = (if generallyIncreasing report then id else reverse) $ report
  where
    generallyIncreasing report = (>) (div (length report) 2) . length . filter (> 0) . zipWith (-) report $ tail report

nbValidReports :: Int -> Input -> Int
nbValidReports n = length . filter (checkValidity . fixReport)
  where
    checkValidity report = isValid (head report) (drop 1 report) 0 || isValid (head $ drop 1 report) (drop 2 report) 1

    isValid e [] nbSkip = nbSkip <= n
    isValid e (h : t) nbSkip
      | nbSkip > n = False
      | e < h && abs (e - h) <= 3 = (isValid h t nbSkip) || isValid e t (nbSkip + 1)
      | otherwise = isValid e t (nbSkip + 1)

part1 :: Input -> Int
part1 = nbValidReports 0

part2 :: Input -> Int
part2 = nbValidReports 1

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = parseInput content
  print $ (== 252) $ part1 input
  print $ (== 324) $ part2 input
