module Main where

import Control.Parallel.Strategies
import Data.List.Split
import Debug.Trace
import System.Environment

-- TODO: Cleanup imports after day done

type Input = [Int]

type Output = Int

parseInput :: String -> Input
parseInput = map read . splitOn " " . head . lines

digitCount :: Int -> Int
digitCount 0 = 1
digitCount n = countThem n
  where
    countThem i
      | i < 10 = 1
      | otherwise = 1 + countThem (div i 10)

splitNumber :: Int -> Int -> [Int]
splitNumber n lengthn = [firstHalf, secondHalf]
  where
    -- lengthn = digitCount n
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
part2 = solve 35

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print input

  print $ part1 input
  print $ part2 input
