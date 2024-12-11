module Main where

import Control.Parallel.Strategies
import Debug.Trace
import System.Environment

type Input = [Int]

type Output = Int

parseInput :: String -> Input
parseInput = map read . words . head . lines

digitCount :: Int -> Int
digitCount i
  | i < 10 = 1
  | otherwise = 1 + digitCount (div i 10)

splitNumber :: Int -> Int -> [Int]
splitNumber n lengthn = [firstHalf, secondHalf]
  where
    powered10 = {-# SCC powered10 #-} (10 ^ (div lengthn 2))
    firstHalf = {-# SCC firstHalf #-} div n powered10
    secondHalf = {-# SCC secondHalf #-} n - (firstHalf * powered10)

iterateStones :: [Int] -> [Int]
iterateStones stones = concat $ map iterateStone stones
  where
    iterateStone 0 = [1]
    iterateStone stone
      | isSplitable = splitNumber stone stoneLength
      | otherwise = [stone * 2024]
      where
        isSplitable = {-# SCC isSplitable #-} (== 0) $ (`mod` 2) stoneLength
        stoneLength = {-# SCC stoneLength #-} digitCount stone

solve :: Int -> Input -> Output
solve n arr = length . fst . head . drop n $ iterate (\(l, i) -> trace (show (i, length l)) (iterateStones l, i + 1)) (arr, 0)

part1 :: Input -> Output
part1 = solve 25

part2 :: Input -> Output
part2 = solve 40

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print input

  print $ part1 input
  print $ (== 11965325) $ part2 input
