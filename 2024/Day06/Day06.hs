module Main where

import Control.Parallel.Strategies
import Data.List
import System.Environment
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Set (Set)
import Data.Set qualified as S
-- TODO: Cleanup imports after day done


data Heading = UP | RIGHT | DOWN | LEFT deriving(Show, Eq, Ord)
type Input = (Matrix Char, (Int, Int), (Int, Int), Heading)
type Output = Int

parseInput :: String -> Input
parseInput s = (mat, (width, height), pos, UP)
    where
        mat = Mat.fromLists $ lines s
        height = Mat.nrows mat
        width = Mat.ncols mat

        pos = head . filter ((== '^') . (mat !)) $ [(y, x) | x<-[1..width], y<-[1..height]]


turn :: Heading -> Heading
turn UP = RIGHT
turn RIGHT = DOWN
turn DOWN = LEFT
turn LEFT = UP

move :: Heading -> (Int, Int) -> (Int, Int)
move UP (y, x) = (y-1, x)
move RIGHT (y, x) = (y, x+1)
move DOWN (y, x) = (y+1, x)
move LEFT (y, x) = (y, x-1)

isOut :: (Int, Int) -> (Int, Int) -> Bool
isOut (width, height) (y, x) = x <= 0 || y <= 0 || x > width || y > height

visitedCells :: Input -> Set (Int, Int)
visitedCells (grid, dim, pos, dir) = moveOut pos dir S.empty
    where
        moveOut yx dir positions
            | isOut dim (move dir yx) = newPoss
            | grid ! (move dir yx) == '#' = moveOut yx newDir newPoss
            | otherwise = moveOut (move dir yx) dir newPoss
                where
                    newPoss = (S.insert yx positions)
                    newDir = turn dir


part1 :: Input -> Output
part1 = S.size . visitedCells

part2 :: Input -> Output
part2 (grid, (dim@(width, height)), pos, dir) = length . filter id . parMap rseq (isLoop pos dir S.empty) . map insertObstacle $ wallsPos
    where
        visited = visitedCells (grid, dim, pos, dir)
        wallsPos =  [(y, x) | y<-[1..height], x<-[1..width], ((y, x) `S.member` visited) && grid ! (y, x) == '.']

        insertObstacle :: (Int, Int) -> Matrix Char
        insertObstacle yx = Mat.setElem '#' yx grid

        isLoop :: (Int, Int) -> Heading -> Set ((Int, Int), Heading) -> Matrix Char -> Bool
        isLoop yx dir positions grid
            | (yx, dir) `S.member` positions = True
            | isOut dim yx = False
            | isOut dim (move dir yx) = False
            | grid ! (move dir yx) == '#' = isLoop yx newDir newPos grid
            | otherwise = isLoop (move dir yx) dir newPos grid
                where
                    newPos = (S.insert (yx, dir) positions)
                    newDir = turn dir


main :: IO ()
main = do
    args  <- getArgs
    content <- readFile (last args)
    let input = parseInput content

    print $ (== 5067) $ part1 input
    print $ (== 1793) $ part2 input
