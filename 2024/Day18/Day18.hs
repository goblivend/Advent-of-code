module Main where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment

type Input = [(Int, Int)]

type Output = Int

parseInput :: String -> Input
parseInput = map (swap . read) . map (\l -> "(" ++ l ++ ")") . lines

data Dir = UP | LEFT | RIGHT | DOWN deriving (Show, Eq, Ord)

move :: (Int, Int) -> Dir -> (Int, Int)
move (y, x) UP = (y - 1, x)
move (y, x) DOWN = (y + 1, x)
move (y, x) LEFT = (y, x - 1)
move (y, x) RIGHT = (y, x + 1)

isOut :: (Int, Int) -> (Int, Int) -> Bool
isOut (width, height) (y, x) = x < 0 || y < 0 || x > width || y > height

bfs :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> Map (Int, Int) (Set (Int, Int)) -> Set (Int, Int)
bfs memory target seen curr paths
  | S.null curr = S.empty
  | target `S.member` curr = paths M.! target
  | otherwise = bfs memory target (S.union seen curr) newCurr newPaths
  where
    notSeen = curr S.\\ seen
    notInWall = notSeen S.\\ memory
    notOut = S.filter (not . isOut target) notInWall

    f2 p (curr', paths') p' = (S.insert p' curr', M.insert p' (S.insert p' (paths M.! p)) paths')
    f1 cp p = foldl (f2 p) cp $ map (move p) [UP, LEFT, RIGHT, DOWN]
    (newCurr, newPaths) = S.foldl f1 (S.empty, M.empty) notOut

part1 :: Int -> Int -> Input -> Output
part1 range firstBatch input = S.size $ bfs memory (range, range) S.empty (S.singleton (0, 0)) (M.singleton (0, 0) S.empty)
  where
    memory = S.fromList $ take firstBatch input

part2 :: Int -> Int -> Input -> (Int, Int)
part2 range firstBatch input = sub (firstBatch, length input + 1)
  where
    findPath m = bfs m (range, range) S.empty (S.singleton (0, 0)) (M.singleton (0, 0) S.empty)
    sub :: (Int, Int) -> (Int, Int)
    sub (imin, imax)
      | imin >= imax = swap $ input !! (imin - 1)
      | S.null currPath = sub (imin, midi)
      | otherwise = sub (midi + 1, imax)
      where
        midi = imin + (imax - imin) `div` 2
        mem = S.fromList $ take midi input
        currPath = findPath mem

showMemory :: Int -> Set (Int, Int) -> [String]
showMemory range memory = [[if S.member (y, x) memory then '#' else '.' | x <- [0 .. range]] | y <- [0 .. range]]

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content
  let range = 70
  let firstStep = 1024
  -- print input

  print $ part1 range firstStep input
  print $ part2 range firstStep input

-- print $ part2dicho range firstStep input

-- let memory = foldl (\r e -> S.insert e r) S.empty $ take 1024 input
-- putStrLn . unlines $ showMemory range memory
