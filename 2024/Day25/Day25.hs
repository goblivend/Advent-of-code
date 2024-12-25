module Main where

import Data.Bits
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))
-- TODO: Cleanup imports after day done

type Input = ([[Int]], [[Int]])
type Output = Int

parseInput :: String -> Input
parseInput input = (locks, keys)
  where
    inputs = splitOn "\n\n" input
    filterOut c = map transpose $ filter (\e -> all (== c) $ head e) $ map lines inputs
    findHeights c = map (map (((+) (-1)) . length . c))
    locks = findHeights (takeWhile (== '#')) $ filterOut '#'
    keys = findHeights (dropWhile (== '.')) $ filterOut '.'

fit :: [Int] -> [Int] -> Bool
fit k l = all (<= 5) . map (\(a, b) -> a+b) $ zip k l

part1 :: Input -> Output
part1 (locks, keys) = length [(key, lock) | key<-keys, lock<-locks, fit key lock]

main :: IO ()
main = do
  args  <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ part1 input
