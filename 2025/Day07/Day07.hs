module Main where

import Data.Bits
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Map (Map)
import Data.Map qualified as M
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))

-- TODO: Cleanup imports after day done

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
    help [] xs = 0
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

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ part1 input
  print $ part2 input
