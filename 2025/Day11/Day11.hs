module Main where

import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Tuple.Extra
import System.Environment

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

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ part1 input
  print $ part2 input
