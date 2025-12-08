module Main where

import Data.List ( sort, sortOn, tails )
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra ( (&&&), both, dupe, fst3, second )
import System.Environment

type Box = (Int, Int, Int)

type Input = (Int, [((Box, Box), [Set Box])])

type Output = Int

parseInput :: String -> Input
parseInput = (&&&) length circuits . parse
  where
    parse = map (read . (++ ")") . ("(" ++)) . lines
    links =  sortOn (uncurry distance) . pairs
    circuits = uncurry zip . second (drop 1 . scanl addLink []) . dupe . links

distance :: Box -> Box -> Int
distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : l) <- tails l, y <- l]

merge :: (Ord a) => [Set a] -> Set a -> [Set a]
merge [] s = [s]
merge (s' : l) s
  | not $ S.disjoint s' s = S.union s s': l
  | otherwise = s' : merge l s

addLink :: [Set Box] -> (Box, Box) ->  [Set Box]
addLink [] (xyz1, xyz2) = [S.fromList [xyz1, xyz2]]
addLink (s:l) (xyz1, xyz2)
  | S.member xyz1 s = merge l (S.insert xyz2 s)
  | S.member xyz2 s = merge l (S.insert xyz1 s)
  | otherwise = s : addLink l (xyz1, xyz2)

part1 :: Input -> Output
part1 = product . take 3 . reverse . sort . map S.size . snd . head . drop 1000 . snd

part2 :: Input -> Output
part2 (n, l) = uncurry (*) . both fst3 . fst . head . dropWhile (\(_, s) -> ( null  s) || S.size (head s) /= n) $ l

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print $ part1 input
  print $ 90036 == part1 input
  -- print $ part2 input
  print $ 6083499488 == part2 input
