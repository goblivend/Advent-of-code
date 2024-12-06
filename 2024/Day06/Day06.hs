module Main where

import Data.List
import System.Environment
import Data.Set (Set)
import Data.Set qualified as S
-- TODO: Cleanup imports after day done


data Heading = UP | RIGHT | DOWN | LEFT deriving(Show, Eq, Ord)
type Input = ([String], (Int, Int), (Int, Int), Heading)
type Output = Int

parseInput :: String -> Input
parseInput s = (lines s, (width, height), pos, UP)
    where
        height = length $ lines s
        width =  length . head $ lines s
        pos = head . filter (\(x, y) -> (== '^') . (!! x) . (!! y) $ lines s) $ [(x, y) | x<-[0..width-1], y<-[0..height-1]]


turn :: Heading -> Heading
turn UP = RIGHT
turn RIGHT = DOWN
turn DOWN = LEFT
turn LEFT = UP

move :: Heading -> (Int, Int) -> (Int, Int)
move UP (x, y) = (x, y-1)
move RIGHT (x, y) = (x+1, y)
move DOWN (x, y) = (x, y+1)
move LEFT (x, y) = (x-1, y)

isOut :: (Int, Int) -> (Int, Int) -> Bool
isOut (width, height) (x, y) = x < 0 || y < 0 || x >= width || y >= height

cellAt :: [String] -> (Int, Int) -> Char
cellAt grid (x, y) = (grid !! y) !! x

visitedCells :: Input -> Set (Int, Int)
visitedCells (grid, dim, pos, dir) = moveOut pos dir S.empty
    where
        moveOut xy dir positions
            | isOut dim (move dir xy) = newPoss
            | cellAt grid (move dir xy) == '#' = moveOut (move newDir xy) newDir newPoss
            | otherwise = moveOut (move dir xy) dir newPoss
                where
                    newPoss = (S.insert xy positions)
                    newDir = turn dir


part1 :: Input -> Output
part1 = S.size . visitedCells

part2 :: Input -> Output
part2 (grid, (dim@(width, height)), pos, dir) = length . filter (isLoop pos dir S.empty) . map insertObstacle $ wallsPos
    where
        visited = visitedCells (grid, dim, pos, dir)
        wallsPos = [(x, y) | x<-[0..width-1], y<-[0..height-1], ((x, y) `S.member` visited) && cellAt grid (x, y) /= '#']

        insertObstacle :: (Int, Int) -> [String]
        insertObstacle (x, y) = linesBefore ++ [elemsBefore ++ "#" ++ elemsAfter] ++ linesAfter
            where
                (linesBefore, lineAt:linesAfter) = splitAt y grid
                (elemsBefore, elemAt:elemsAfter) = splitAt x lineAt

        isLoop :: (Int, Int) -> Heading -> Set ((Int, Int), Heading) -> [String] -> Bool
        isLoop xy dir positions grid
            | (xy, dir) `S.member` positions = True
            | isOut dim xy = False
            | isOut dim (move dir xy) = False
            | cellAt grid (move dir xy) == '#' = isLoop xy newDir newPos grid
            | otherwise = isLoop (move dir xy) dir newPos grid
                where
                    newPos = (S.insert (xy, dir) positions)
                    newDir = turn dir


main :: IO ()
main = do
    args  <- getArgs
    content <- readFile (last args)
    let input = parseInput content

    print $ (== 5067) $ part1 input
    print $ (== 1793) $ part2 input
