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

type Input = (Matrix Char, (Int, Int))
type Output = Int

parseInput :: String -> Input
parseInput input = (maze, start)
  where
    maze = Mat.fromLists $ lines input
    start = head [(y, x) | y <-[1..Mat.nrows maze], x <-[1..Mat.ncols maze], maze ! (y, x) == 'S']


data Dir = UP | LEFT | RIGHT | DOWN deriving (Show, Eq, Ord)

move :: (Int, Int) -> Dir -> (Int, Int)
move (y, x) UP = (y - 1, x)
move (y, x) DOWN = (y + 1, x)
move (y, x) LEFT = (y, x - 1)
move (y, x) RIGHT = (y, x + 1)

turn90 :: Dir -> [Dir]
turn90 UP = [LEFT, RIGHT]
turn90 DOWN = [LEFT, RIGHT]
turn90 LEFT = [UP, DOWN]
turn90 RIGHT = [UP, DOWN]

pathFind ::Matrix Char -> Map ((Int, Int), Dir) Int -> Int -> Dir -> (Int, Int) -> (Bool, Map ((Int, Int), Dir) Int, Int, Set (Int, Int))
pathFind maze seen score dir pos
  -- | rightPos && trace ("At " ++ show pos) (False ) = (False, seen, -1, S.empty)
  | (pos, dir) `M.member` seen && seen M.! (pos, dir) < score = (False, seen, -1, S.empty) -- Lower score ok
  -- | rightPos && trace ("At " ++ show pos ++ " has better score") (False ) = (False, seen, -1, S.empty)
  | maze ! pos == 'E' = (True, seen, score, S.singleton pos)
  -- | rightPos && trace ("At " ++ show pos ++ " not at end") (False ) = (False, seen, -1, S.empty)
  | maze ! pos == '#' = (False, seen, -1, S.empty)
  -- | rightPos && trace ("At " ++ show pos ++ " not at wall" ++ show bestSols) (False ) = (False, seen, -1, S.empty)
  | length bestSols /= 0 = bestSol
  | otherwise = (False, s2seen, -1, S.empty)
    where
      rightPos = (pos == (5, 4) || (pos == (4, 4) && dir == UP))



      currSeen = (M.insert (pos, dir) score seen)
      (fbool, fseen, fscore, fseats) = pathFind maze currSeen (score + 1) dir (move pos dir)
      [d1, d2] = turn90 dir
      (s1bool, s1seen, s1score, s1seats) = pathFind maze fseen (score + 1001) d1 (move pos d1)
      (s2bool, s2seen, s2score, s2seats) = pathFind maze s1seen (score + 1001) d2 (move pos d2)

      bestSols :: [[(Bool, Set (Int, Int), Int)]]
      bestSols = groupBy (\a b -> thd3 a == thd3 b) . sortOn thd3 $ filter (fst3) [(fbool, fseats, fscore), (s1bool, s1seats, s1score), (s2bool, s2seats, s2score)]

      seats = S.unions $ map snd3 $ head bestSols

      bestSol = (True, s2seen, thd3 . head $ head bestSols, S.insert pos seats)

part :: Input -> (Output, Output)
part (maze, pos) = (\(_,_,p1,p2) -> (p1, S.size p2)) $ pathFind maze M.empty 0 RIGHT pos

showMaze :: Matrix Char -> Set (Int, Int) -> [String]
showMaze maze path = map (map (\yx -> if yx `S.member` path then 'O' else maze ! yx)) [[(y, x) | x<-[1..Mat.ncols maze]] | y<-[1..Mat.nrows maze]]



main :: IO ()
main = do
  args  <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print $ first Mat.toLists input

  let (part1, part2) = part input

  print $ (== 85480) $ part1
  print $ (== 518) $ part2

  -- putStrLn $ unlines . showMaze (fst input) $ (\(_,_,_,e) -> e) $ pathFind (fst input) M.empty 0 RIGHT (snd input)
