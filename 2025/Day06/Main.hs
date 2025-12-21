module Day06.Main (day06) where

import AOC.Cli (defaultUsage, getDefaultFlags)
import AOC.Runtime (printRuntime)
import Control.Monad (when)
import Data.List (transpose)
import Data.List.Extra (split)
import Data.Tuple.Extra (dupe, second, (***))

type Input = [((Int -> Int -> Int), [String])]

type Output = Int

parseInput :: String -> Input
parseInput = map getOp . map transpose . split (all (== ' ')) . transpose . lines
  where
    getOp = (***) (readOp . last) init . dupe
    readOp s
      | elem '*' s = (*)
      | elem '+' s = (+)
      | otherwise = error ("invalid input given, should have either '+' or '*' but has none : " ++ s)

part1 :: Input -> Output
part1 = sum . map (uncurry foldl1 . second (map read))

part2 :: Input -> Output
part2 = part1 . map (second transpose)

day06 :: [String] -> IO ()
day06 args = do
  let (help, input, p1, p2) = getDefaultFlags 2025 06 args

  if help
    then defaultUsage 2025 06
    else do
      content <- readFile input
      let inp = parseInput content
      when p1 $ printRuntime ((++) "2025/Day06 Part1: " . show) (return (part1 inp))
      when p2 $ printRuntime ((++) "2025/Day06 Part2: " . show) (return (part2 inp))
