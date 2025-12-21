module Day04.Main (day04) where

import AOC.Cli (defaultUsage, getDefaultFlags)
import AOC.Runtime (printRuntime)
import Control.Monad (when)
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat

type Input = Matrix Bool

type Output = Int

parseInput :: String -> Input
parseInput = Mat.fromLists . map (map (== '@')) . lines

removeRols :: Matrix Bool -> Matrix Bool
removeRols input = foldl removeRol input . filter ((<= 4) . nbNeighbors) $ filter (input !) [(x, y) | x <- [1 .. w], y <- [1 .. h]]
  where
    (w, h) = (Mat.ncols input, Mat.nrows input)
    removeRol = flip (Mat.setElem False)
    nbNeighbors = length . filter (input !) . listNeighbors
    listNeighbors (x, y) = filter checkBound [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]
    checkBound (x, y) = x >= 1 && y >= 1 && x <= w && y <= h

nbRolls :: Matrix Bool -> Int
nbRolls = length . filter id . Mat.toList

part1 :: Input -> Output
part1 input = nbStart - nbEnd
  where
    nbStart = nbRolls input
    nbEnd = nbRolls $ removeRols input

part2 :: Input -> Output
part2 input
  | nbEnd == nbStart = 0
  | otherwise = nbStart - nbEnd + part2 matEnd
  where
    nbStart = nbRolls input
    nbEnd = nbRolls matEnd
    matEnd = removeRols input

day04 :: [String] -> IO ()
day04 args = do
  let (help, input, p1, p2) = getDefaultFlags 2025 04 args

  if help
    then defaultUsage 2025 04
    else do
      content <- readFile input
      let inp = parseInput content
      when p1 $ printRuntime ((++) "2025/Day04 Part1: " . show) (return (part1 inp))
      when p2 $ printRuntime ((++) "2025/Day04 Part2: " . show) (return (part2 inp))
