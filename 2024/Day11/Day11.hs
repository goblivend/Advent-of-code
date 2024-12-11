module Main where

import Control.Parallel.Strategies
import Data.Map (Map)
import Data.Map qualified as M
import Debug.Trace
import System.Environment

type Input = [Int]

type Output = Int

parseInput :: String -> Input
parseInput = map read . words . head . lines

-- Number of digits in a number : digitCount 128 = 3
digitCount :: Int -> Int
digitCount i
  | i < 10 = 1
  | otherwise = 1 + digitCount (div i 10)

-- Splits a number containing an even number of digits in 2 : splitNumber 2048 = [20, 48]
splitNumber :: Int -> [Int]
splitNumber n = [firstHalf, secondHalf]
  where
    lengthn = {-# SCC stoneLength #-} digitCount n
    powered10 = {-# SCC powered10 #-} (10 ^ (div lengthn 2))
    firstHalf = {-# SCC firstHalf #-} div n powered10
    secondHalf = {-# SCC secondHalf #-} n - (firstHalf * powered10)

-- Main Algorithm of the day
iterateStone :: Int -> [Int]
iterateStone 0 = [1]
iterateStone stone
  | (== 0) . (`mod` 2) $ digitCount stone = splitNumber stone
  | otherwise = [stone * 2024]

solveFold :: Map (Int, Int) Int -> (Int, Int) -> Map (Int, Int) Int
solveFold memRes (stone, n)
  | (stone, n) `M.member` memRes = memRes
  | n == 1 = M.insert (stone, n) (length newStones) memRes
  | otherwise = M.insert (stone, n) total memResFold
  where
    newStones = iterateStone stone
    newQuerries = zip newStones $ repeat (n - 1)

    memResFold = foldl solveFold memRes $ newQuerries

    total = sum . map (memResFold M.!) $ newQuerries

solve :: Int -> Input -> Output
solve n arr = sum $ map (solutions M.!) . zip arr $ repeat n
  where
    solutions :: Map (Int, Int) Int
    solutions = foldl solveFold M.empty . zip arr $ repeat n

part1 :: Input -> Output
part1 = solve 25

part2 :: Input -> Output
part2 = solve 75

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print input

  let res1 = part1 input
  -- print res1
  print $ any (== res1) [55312, 183484]
  let res2 = part2 input
  -- print res2
  print $ any (== res2) [65601038650482, 218817038947400]




-- Gets main algorithm using a memory map to avoid calculating the same result twice
-- lookUpIterateStone :: Map Int [Int] -> Int -> (Map Int [Int], [Int])
-- lookUpIterateStone mem n
--   | n `M.member` mem = (mem, mem M.! n)
--   | otherwise = (newMem, newArr)
--   where
--     newArr = iterateStone n
--     newMem = M.insert n newArr mem

-- solveFold :: (Map (Int, Int) Int, Map Int [Int]) -> (Int, Int) -> (Map (Int, Int) Int, Map Int [Int])
-- solveFold (memRes, mem1) (stone, n)
--   | (stone, n) `M.member` memRes = (memRes, mem1)
--   | n == 1 = (M.insert (stone, n) (length resLookUp) memRes, mem1LookUp)
--   | otherwise = (M.insert (stone, n) total memResFold, mem1Fold)
--   where
--     (mem1LookUp, resLookUp) = lookUpIterateStone mem1 stone
--     (memResSub1, mem1Sub1) = solveFold (memRes, mem1) (stone, n - 1)

--     newStones :: [Int]
--     newStones = mem1Sub1 M.! stone

--     newQuerries :: [(Int, Int)]
--     newQuerries = zip newStones $ repeat (n - 1)
--     (memResFold, mem1Fold) = foldl solveFold (memResSub1, mem1Sub1) $ newQuerries
--     total = sum . map (memResFold M.!) $ newQuerries

-- solve :: Int -> Input -> Output
-- solve n arr = sum $ map (solutions M.!) . zip arr $ repeat n
--   where
--     solutions :: Map (Int, Int) Int
--     solutions = fst . foldl solveFold (M.empty, M.empty) . zip arr $ repeat n
