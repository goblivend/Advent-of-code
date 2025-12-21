module Day08.Main (day08) where

import AOC.Cli (defaultUsage, getDefaultFlags)
import AOC.Runtime (printRuntime)
import AOC.Utils (pairs)
import Control.Monad (when)
import Data.List (sort, sortOn)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra (both, dupe, fst3, second, (&&&))

type Box = (Int, Int, Int)

type Input = (Int, [((Box, Box), [Set Box])])

type Output = Int

parseInput :: String -> Input
parseInput = (&&&) length circuits . parse
  where
    parse = map (read . (++ ")") . ("(" ++)) . lines
    links = sortOn (uncurry distance) . pairs
    circuits = uncurry zip . second (drop 1 . scanl addLink []) . dupe . links

distance :: Box -> Box -> Int
distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int) + (z1 - z2) ^ (2 :: Int)

merge :: (Ord a) => [Set a] -> Set a -> [Set a]
merge [] s = [s]
merge (s' : l) s
  | not $ S.disjoint s' s = S.union s s' : l
  | otherwise = s' : merge l s

addLink :: [Set Box] -> (Box, Box) -> [Set Box]
addLink [] (xyz1, xyz2) = [S.fromList [xyz1, xyz2]]
addLink (s : l) (xyz1, xyz2)
  | S.member xyz1 s = merge l (S.insert xyz2 s)
  | S.member xyz2 s = merge l (S.insert xyz1 s)
  | otherwise = s : addLink l (xyz1, xyz2)

part1 :: Input -> Output
part1 = product . take 3 . reverse . sort . map S.size . snd . head . drop 1000 . snd

part2 :: Input -> Output
part2 (n, l) = uncurry (*) . both fst3 . fst . head . dropWhile (\(_, s) -> (null s) || S.size (head s) /= n) $ l

day08 :: [String] -> IO ()
day08 args = do
  let (help, input, p1, p2) = getDefaultFlags 2025 08 args

  if help
    then defaultUsage 2025 08
    else do
      content <- readFile input
      let inp = parseInput content
      when p1 $ printRuntime ((++) "2025/Day08 Part1: " . show) (return (part1 inp))
      when p2 $ printRuntime ((++) "2025/Day08 Part2: " . show) (return (part2 inp))
