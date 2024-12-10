module Main where

import Data.Bits
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Set (Set)
import Data.Set qualified as S
import System.Environment
-- TODO: Cleanup imports after day done

type Input = (Matrix Int, [(Int, Int)])
type Output = Int

data Dir = UP | LEFT | RIGHT | DOWN deriving (Show, Eq)

parseInput :: String -> Input
parseInput s = (mat, starts)
  where
    mat = Mat.fromLists . map (map (read . (:[]))) $ lines s
    starts = [(y, x) | y<-[1..Mat.nrows mat], x<-[1..Mat.ncols mat], mat ! (y,x) == 0]

move :: (Int, Int) -> Dir -> (Int, Int)
move (y, x) UP = (y-1, x)
move (y, x) DOWN = (y+1, x)
move (y, x) LEFT = (y, x-1)
move (y, x) RIGHT = (y, x+1)

isOut :: (Int, Int) -> (Int, Int) -> Bool
isOut (width, height) (y, x) = x <= 0 || y <= 0 || x > width || y > height

validEndTrails :: Matrix Int -> Int -> (Int, Int) -> [(Int, Int)]
validEndTrails grid curr pos
      | isOut (Mat.ncols grid, Mat.nrows grid) pos = []
      | grid ! pos /= curr + 1 = []
      | grid ! pos == 9 = [pos]
      | otherwise = concat $ map (validEndTrails grid (grid ! pos) . move pos) [UP, DOWN, LEFT, RIGHT]

part1 :: Input -> Output
part1 (grid, pos) = sum $ map (length . sortUniq . validEndTrails grid (-1)) pos

part2 :: Input -> Output
part2 (grid, pos) = sum $ map (length . validEndTrails grid (-1)) pos

main :: IO ()
main = do
  args  <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print $ part1 input
  print $ part2 input
