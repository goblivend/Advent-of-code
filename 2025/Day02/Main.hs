module Day02.Main (day02) where

import AOC.Cli (defaultUsage, getDefaultFlags)
import AOC.Runtime (printRuntime)
import Control.Monad (when)
import Data.List.Extra (replace)
import Data.List.Split (splitOn)

type Input = [(Int, Int)]

type Output = Int

parseInput :: String -> Input
parseInput = map (read . ("(" ++) . (++ ")") . replace "-" ",") . splitOn ","

isInvalidForN :: Int -> String -> Bool
isInvalidForN n s
  | l `mod` n /= 0 = False
  | otherwise = all null . splitOn (take delta s) $ drop delta s -- Could have changed to `uncurry splitOn $ splitAt delta s` but it makes the solution 10% slower
  where
    l = length s
    delta = div l n

sumInvalid :: (String -> Bool) -> [(Int, Int)] -> Int
sumInvalid isInvalid = sum . filter (isInvalid . show) . concat . map (uncurry enumFromTo)

part1 :: Input -> Output
part1 = sumInvalid isDouble
  where
    -- Sligthly faster
    -- isDouble s = uncurry (==) $ splitAt (length s `div` 2) s
    -- Using existing code
    isDouble = isInvalidForN 2

part2 :: Input -> Output
part2 = sumInvalid isInvalidForAll
  where
    isInvalidForAll s = not . null . filter (`isInvalidForN` s) $ [2 .. length s]

day02 :: [String] -> IO ()
day02 args = do
  let (help, input, p1, p2) = getDefaultFlags 2025 02 args

  if help
    then defaultUsage 2025 02
    else do
      content <- readFile input
      let inp = parseInput content
      when p1 $ printRuntime ((++) "2025/Day02 Part1: " . show) (return (part1 inp))
      when p2 $ printRuntime ((++) "2025/Day02 Part2: " . show) (return (part2 inp))
