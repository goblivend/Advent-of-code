module Main where

import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import System.Environment
import Text.Regex.TDFA ((=~))

-- TODO: Cleanup imports after day done

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

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ 1587 == part1 input
  print $ 8946 == part2 input
