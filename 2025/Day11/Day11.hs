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

findAllPaths :: Map String [String] -> Set String -> Map String Int -> String -> String -> Map String Int
findAllPaths devs seen calculated dest curr
  | M.member curr calculated = calculated
  | S.member curr seen = M.singleton curr 0
  | dest == curr = M.singleton dest 1
  | M.notMember curr devs = M.singleton curr 0
  | otherwise = M.insert curr (sum $ map ((M.!) uniontMp) nextKeys) uniontMp -- trace (show ("seen", S.size seen, curr, seen))
  where
    nextKeys = devs M.! curr
    newSeen = S.insert curr seen
    uniontMp :: Map String Int
    uniontMp = foldl (\m k -> M.union m $ findAllPaths devs (S.union newSeen (M.keysSet m)) m dest k) calculated nextKeys

nbPaths :: Map String [String] -> String -> String -> Int
nbPaths input from to = (\e -> trace (show ("from", from, "to", to, "got", e)) e) $ (M.! from) $ findAllPaths input S.empty M.empty to from

part1 :: Input -> Output
part1 input = nbPaths input "you" "out"

part2 :: Input -> Output
part2 input = nbPaths input "svr" "out" -- (svrDac * dacFft * fftOut) + (svrFft * fftDac * dacOut)
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

  print $ part1 input
  print $ part2 input
