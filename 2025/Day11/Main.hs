module Day11.Main (day11) where

import AOC.Cli (defaultUsage, getDefaultFlags)
import AOC.Runtime (printRuntime)
import Control.Monad (when)
import Data.List (permutations)
import Data.Map (Map)
import Data.Map qualified as M (empty, fromList, insert, member, notMember, singleton, union, (!))
import Data.Tuple.Extra (dupe, second)

type Input = Map String [String]

type Output = Int

parseInput :: String -> Input
parseInput = M.fromList . map (second (words . drop 1) . splitAt 3) . lines

findAllPaths :: Map String [String] -> Map String Int -> String -> String -> Map String Int
findAllPaths devs calculated dest curr
  | M.member curr calculated = calculated
  | dest == curr = M.singleton dest 1
  | M.notMember curr devs = M.singleton curr 0
  | otherwise = M.insert curr (sum $ map ((M.!) uniontMp) nextKeys) uniontMp
  where
    nextKeys = devs M.! curr
    uniontMp :: Map String Int
    uniontMp = foldl (\m k -> M.union m $ findAllPaths devs m dest k) calculated nextKeys

nbPaths :: Map String [String] -> String -> String -> Int
nbPaths input from to = (M.! from) $ findAllPaths input M.empty to from

part1 :: Input -> Output
part1 input = nbPaths input "you" "out"

passingThrough :: [String] -> Input -> String -> String -> Int
passingThrough mustSee input from to = sum . map (product . map (uncurry (nbPaths input)) . (uncurry zip) . second (drop 1) . dupe) $ ways
  where
    ways = map ((++ [to]) . (from :)) $ permutations mustSee

part2 :: Input -> Output
part2 input = passingThrough ["fft", "dac"] input "svr" "out"

day11 :: [String] -> IO ()
day11 args = do
  let (help, input, p1, p2) = getDefaultFlags 2025 11 args

  if help
    then defaultUsage 2025 11
    else do
      content <- readFile input
      let inp = parseInput content
      when p1 $ printRuntime ((++) "2025/Day11 Part1: " . show) (return (part1 inp))
      when p2 $ printRuntime ((++) "2025/Day11 Part2: " . show) (return (part2 inp))
