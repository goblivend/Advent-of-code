module Day05.Main (day05) where

import AOC.Cli (defaultUsage, getDefaultFlags)
import AOC.Runtime (printRuntime)
import AOC.Utils (toTuple)
import Control.Monad (when)
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Tuple.Extra (both, (***))

type Input = ([(Int, Int)], [Int])

type Output = Int

parseInput :: String -> Input
parseInput = (***) firstParse (map read) . both lines . toTuple . splitOn "\n\n"
  where
    firstParse = map (toTuple . map read . splitOn "-")

part1 :: Input -> Output
part1 (fresh, ingredients) = length . filter (\i -> any (\(mi, ma) -> mi <= i && i <= ma) fresh) $ ingredients

part2 :: Input -> Output
part2 = sum . map ((+) 1 . uncurry (flip (-))) . mergeRanges . sortOn fst . fst
  where
    mergeRanges [] = []
    mergeRanges [lh] = [lh]
    mergeRanges ((l1, h1) : (l2, h2) : l)
      | h1 < l2 = (l1, h1) : mergeRanges ((l2, h2) : l)
      | otherwise = mergeRanges ((l1, max h1 h2) : l)

day05 :: [String] -> IO ()
day05 args = do
  let (help, input, p1, p2) = getDefaultFlags 2025 05 args

  if help
    then defaultUsage 2025 05
    else do
      content <- readFile input
      let inp = parseInput content
      when p1 $ printRuntime ((++) "2025/Day05 Part1: " . show) (return (part1 inp))
      when p2 $ printRuntime ((++) "2025/Day05 Part2: " . show) (return (part2 inp))
