module Day12.Main (day12) where

import AOC.Cli (defaultUsageFinalDay, getDefaultFlagsFinalDay)
import AOC.Runtime (printRuntime)
import Control.Monad (when)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as M (fromAscList, elems)
import Data.Tuple.Extra (second, both, dupe, first)

type Input = (Map Int [[Bool]], [((Int, Int), [Int])])

type Output = Int

parseInput :: String -> Input
parseInput = second (map (parseSpot . words) . last) . first (M.fromAscList . map parsePresent . init) . dupe . splitOn [[]] . lines
  where
    parsePresent (id : shape) = (read $ init id, map (map (== '#')) shape)
    parseSpot (dims : l) = (both read . second (tail . init) . break (== 'x') $ dims, map read l)

sizePresent :: [[Bool]] -> Int
sizePresent = length . filter id . concat

part1 :: Input -> Output
part1 (presents, spots) = length . filter canFit $ spots
  where
    canFit ((w, h), press) = (<= w * h) . sum . map (uncurry (*)) . zip press . map sizePresent $ M.elems presents

day12 :: [String] -> IO ()
day12 args = do
  let (help, input, p1) = getDefaultFlagsFinalDay 2025 12 args

  if help
    then defaultUsageFinalDay 2025 12
    else do
      content <- readFile input
      let inp = parseInput content
      when p1 $ printRuntime ((++) "2025/Day12 Part1: " . show) (return (part1 inp))
