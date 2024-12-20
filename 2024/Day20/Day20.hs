module Main where

import Control.Parallel.Strategies
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

type Input = (Matrix Char, ((Int, Int), (Int, Int)))

type Output = Int

data Dir = UP | LEFT | RIGHT | DOWN deriving (Show, Eq, Ord)

parseInput :: String -> Input
parseInput input = (grid, (start, finish))
  where
    grid = Mat.fromLists . lines $ input
    start = head [(y, x) | y <- [1 .. Mat.nrows grid], x <- [1 .. Mat.ncols grid], grid ! (y, x) == 'S']
    finish = head [(y, x) | y <- [1 .. Mat.nrows grid], x <- [1 .. Mat.ncols grid], grid ! (y, x) == 'E']

move :: (Int, Int) -> Dir -> (Int, Int)
move (y, x) UP = (y - 1, x)
move (y, x) DOWN = (y + 1, x)
move (y, x) LEFT = (y, x - 1)
move (y, x) RIGHT = (y, x + 1)

isOut :: (Int, Int) -> (Int, Int) -> Bool
isOut (width, height) (y, x) = x <= 0 || y <= 0 || x > width || y > height

dfs :: Matrix Char -> (Int, Int) -> Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
dfs grid target seen yx
  | yx `S.member` seen = []
  | yx == target = [yx]
  | isOut (Mat.ncols grid, Mat.nrows grid) yx = []
  | grid ! yx == '#' = []
  | res == [] = []
  | otherwise = yx : (head res)
  where
    seen' = S.insert yx seen
    explore = map (\yx' -> dfs grid target seen' yx') $ map (move yx) $ [LEFT, DOWN, RIGHT, UP]
    res = filter (/= []) explore

dist :: (Int, Int) -> (Int, Int) -> Int
dist (y, x) (y', x') = abs (y - y') + abs (x - x')

findShortCuts :: Int -> Int -> [(Int, Int)] -> [Int]
findShortCuts minCheat cheatTime path = ({-# SCC filtering #-} withStrategy (parList rseq) . filter (>= minCheat)) $ ({-# SCC mapTimeDiff #-} parMap rseq ((\(i, j) -> j - i - dist (path !! i) (path !! j)))) pairIndices
  where
    pathLen = length path
    pairIndices = {-# SCC generateIndices #-} [(i, j) | i <- [0 .. pathLen - 1], j <- [i + minCheat + 1 .. pathLen - 1], dist (path !! i) (path !! j) <= cheatTime]

part1 :: Input -> Output
part1 (grid, (start, finish)) = length $ findShortCuts 0 2 $ dfs grid finish S.empty start

part2 :: Input -> Output
part2 (grid, (start, finish)) = length $ findShortCuts 100 20 $ dfs grid finish S.empty start

displayPath :: Matrix Char -> Set (Int, Int) -> [String]
displayPath grid path = map (map (\yx -> if yx `S.member` path then 'O' else grid ! yx)) $ [[(y, x) | x <- [1 .. Mat.ncols grid]] | y <- [1 .. Mat.nrows grid]]

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print $ first Mat.toLists $ input

  print $ part1 input
  print $ part2 input
