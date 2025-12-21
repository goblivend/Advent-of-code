module Day03.Main (day03) where

import AOC.Cli (defaultUsage, getDefaultFlags)
import AOC.Runtime (printRuntime)
import Control.Monad (when)

type Input = [[Int]]

type Output = Int

infixr 8 .:

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g = \x y -> f (g x y)

parseInput :: String -> Input
parseInput = map (map (read . pure)) . lines

bests :: Int -> [Int] -> [Int]
bests 0 _ = []
bests i l
  | nbl <= i = l
  | otherwise = best : bests (i - 1) (tail $ dropWhile (/= best) l)
  where
    nbl = length l
    best = maximum $ take (nbl - i + 1) l

findBestsOf :: Int -> [Int] -> Int
findBestsOf = read . concatMap show .: bests

part1 :: Input -> Output
part1 = sum . map (findBestsOf 2)

part2 :: Input -> Output
part2 = sum . map (findBestsOf 12)

day03 :: [String] -> IO ()
day03 args = do
  let (help, input, p1, p2) = getDefaultFlags 2025 03 args

  if help
    then defaultUsage 2025 03
    else do
      content <- readFile input
      let inp = parseInput content
      when p1 $ printRuntime ((++) "2025/Day03 Part1: " . show) (return (part1 inp))
      when p2 $ printRuntime ((++) "2025/Day03 Part2: " . show) (return (part2 inp))
