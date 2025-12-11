module Main where

import Data.Bits
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Map (Map)
import Data.Map qualified as M
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))

-- TODO: Cleanup imports after day done

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
part2 input = passingThrough ["fft", "dac"] input "svr" "out" -- (svrDac * dacFft * fftOut) + (svrFft * fftDac * dacOut)
  where
    svrFft = nbPaths input "svr" "fft"
    svrDac = nbPaths input "svr" "dac"
    dacFft = nbPaths input "dac" "fft"
    fftDac = nbPaths input "fft" "dac"
    fftOut = nbPaths input "fft" "out"
    dacOut = nbPaths input "dac" "out"

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ 701 == part1 input
  print $ 390108778818526 == part2 input
