module Day07.Main (day07) where

import AOC.Cli (defaultUsage, getDefaultFlags)
import AOC.Runtime (printRuntime)
import Control.Monad (when)
import Data.List (elemIndex, singleton)
import Data.Map qualified as M (fromAscList, elems, (!))
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S (size, unions, fromAscList, filter, map, singleton)
import Data.Tuple.Extra ((&&&), second, )

type Input = ([[Bool]], Int)

type Output = Int

parseInput :: String -> Input
parseInput = (&&&) getMat getS . lines
  where
    getS = fromJust . elemIndex 'S' . head
    getMat = map (map (== '^')) . filterLines
    -- filterLines = filter (any ((/=) '.')) . tail
    filterLines = filter (elem '^') . tail

getNextCols :: [Bool] -> Int -> [Int]
getNextCols line x
  | line !! x = [x - 1, x + 1]
  | otherwise = [x]

part1 :: Input -> Output
part1 = uncurry help . second S.singleton
  where
    help :: [[Bool]] -> Set Int -> Int
    help [] _ = 0
    help (line : m) xs = countHits + (help m newXs)
      where
        countHits = S.size $ S.filter (line !!) xs
        newXs = S.unions $ S.map (S.fromAscList . getNextCols line) xs

part2 :: Input -> Output
part2 = head . uncurry help . second (singleton)
  where
    help :: [[Bool]] -> [Int] -> [Int]
    help [] xs = 1 <$ xs
    help (line : m) xs = map (sum . map (nextRes M.!) . (M.!) newXsmp) xs
      where
        newXsmp = M.fromAscList . zip xs . map (getNextCols line) $ xs
        newXs = concat $ M.elems newXsmp
        nextRes = M.fromAscList $ zip newXs (help m newXs)

day07 :: [String] -> IO ()
day07 args = do
  let (help, input, p1, p2) = getDefaultFlags 2025 07 args

  if help
    then defaultUsage 2025 07
    else do
      content <- readFile input
      let inp = parseInput content
      when p1 $ printRuntime ((++) "2025/Day07 Part1: " . show) (return (part1 inp))
      when p2 $ printRuntime ((++) "2025/Day07 Part2: " . show) (return (part2 inp))
