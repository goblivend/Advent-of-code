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

smallestPathFind :: Set (Int, Int) -> Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int -> (Int, Set (Int, Int), Map (Int, Int) Int)
smallestPathFind memory seen pos target nbMoves
  -- \| trace ("Pos: " ++ show pos) False = (-1, S.empty, seen)
  | isOut target pos = (-1, S.empty, seen)
  | pos `M.member` seen && seen M.! pos <= nbMoves = (-1, S.empty, seen) -- trace ("At " ++ show nbMoves ++ " but already seen at " ++ show (seen M.! pos))
  | pos `S.member` memory = (-1, S.empty, currSeen)
  | pos == target = (nbMoves, S.singleton target, currSeen)
  | length bestSol /= 0 = (bestMoves, S.insert pos bestPath, seenRight)
  | otherwise = (-1, S.empty, seenRight)
  where
    currSeen = M.insert pos nbMoves seen
    (movesUp, pathUp, seenUp) = smallestPathFind memory currSeen (move pos UP) target (nbMoves + 1)
    (movesLeft, pathLeft, seenLeft) = smallestPathFind memory seenUp (move pos LEFT) target (nbMoves + 1)
    (movesDown, pathDown, seenDown) = smallestPathFind memory seenLeft (move pos DOWN) target (nbMoves + 1)
    (movesRight, pathRight, seenRight) = smallestPathFind memory seenDown (move pos RIGHT) target (nbMoves + 1)

    bestSol = sortOn fst . filter ((/= -1) . fst) $ [(movesUp, pathUp), (movesLeft, pathLeft), (movesDown, pathDown), (movesRight, pathRight)]
    (bestMoves, bestPath) = head bestSol

part1 :: Int -> Input -> Output
part1 range input = fst3 $ smallestPathFind memory M.empty (0, 0) (range, range) 0
  where
    memory = foldl (\r e -> S.insert e r) S.empty $ take 1024 input

anyPathFind :: Set (Int, Int) -> Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int -> (Int, Set (Int, Int), Map (Int, Int) Int)
anyPathFind memory seen pos target nbMoves
  -- \| trace ("Pos: " ++ show pos) False = (-1, S.empty, seen)
  | isOut target pos = (-1, S.empty, seen)
  | pos `M.member` seen && seen M.! pos <= nbMoves = (-1, S.empty, seen) -- trace ("At " ++ show nbMoves ++ " but already seen at " ++ show (seen M.! pos))
  | pos `S.member` memory = (-1, S.empty, currSeen)
  | pos == target = (nbMoves, S.singleton target, currSeen)
  | movesUp /= -1 = (movesUp, S.insert pos pathUp, seenUp)
  | movesLeft /= -1 = (movesLeft, S.insert pos pathLeft, seenLeft)
  | movesDown /= -1 = (movesDown, S.insert pos pathDown, seenDown)
  | movesRight /= -1 = (movesRight, S.insert pos pathRight, seenRight)
  | otherwise = (-1, S.empty, seenRight)
  where
    currSeen = M.insert pos nbMoves seen
    (movesUp, pathUp, seenUp) = anyPathFind memory currSeen (move pos UP) target (nbMoves + 1)
    (movesLeft, pathLeft, seenLeft) = anyPathFind memory seenUp (move pos LEFT) target (nbMoves + 1)
    (movesDown, pathDown, seenDown) = anyPathFind memory seenLeft (move pos DOWN) target (nbMoves + 1)
    (movesRight, pathRight, seenRight) = anyPathFind memory seenDown (move pos RIGHT) target (nbMoves + 1)

part2 :: Int -> Input -> (Int, Int)
part2 range input = sub 1024 memory (drop 1024 input) (snd3 $ smallestPathFind memory M.empty (0, 0) (range, range) 0)
  where
    memory = foldl (\r e -> S.insert e r) S.empty $ take 1024 input
    sub n mem [] lastPath = (-1, -1)
    sub n mem (e : l) lastPath
      -- \| trace (show lastPath) False = (0, 0)
      | e `S.notMember` lastPath = sub (n + 1) newMem l lastPath
      | currRes == -1 = swap e
      | otherwise = sub (n + 1) newMem l currPath
      where
        newMem = S.insert e mem
        (currRes, currPath, _) = smallestPathFind newMem M.empty (0, 0) (range, range) 0

showMemory :: Int -> Set (Int, Int) -> [String]
showMemory range memory = [[if S.member (y, x) memory then '#' else '.' | x <- [0 .. range]] | y <- [0 .. range]]

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content
  let range = 70

  -- print input

  -- let memory = foldl (\r e -> S.insert e r) S.empty $ take 1024 input

  -- putStrLn . unlines $ showMemory range memory

  print $ part1 range input
  print $ part2 range input
