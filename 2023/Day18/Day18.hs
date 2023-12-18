module Main where

import Data.Maybe ( fromJust, isJust )
import Debug.Trace(trace)
import Data.List.Split
import Data.Set (Set, member, fromList, toList)
import Data.Map (Map, (!))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Matrix as Mat

data Direction = North | South | East | West deriving (Eq, Ord, Show)
data DigPlan = DigPlan {dir :: Direction, nb:: Int, color::String} deriving (Eq, Ord, Show)
type Input = [DigPlan]

parsePlan :: String -> DigPlan
parsePlan l = DigPlan (createDir dir) (read nb) color
    where
        createDir "U" = North
        createDir "D" = South
        createDir "L" = West
        createDir "R" = East
        [dir, nb, color] = splitOn " " l

parseInput :: String -> Input
parseInput = map parsePlan . lines

digOut :: [DigPlan] -> (Int, Int) -> [(Int, Int)]
digOut [] xy = []
digOut ((DigPlan d 0 _):l) xy = digOut l xy
digOut ((DigPlan North n c):l) (x, y) = (x, y) : digOut l (x, y-n)
digOut ((DigPlan South n c):l) (x, y) = (x, y) : digOut l (x, y+n)
digOut ((DigPlan East  n c):l) (x, y) = (x, y) : digOut l (x+n, y)
digOut ((DigPlan West  n c):l) (x, y) = (x, y) : digOut l (x-n, y)


solve :: [(Int, Int)] -> Int
solve grid = abs . (`div` 2) . sum . map (uncurry solve') $ zip grid (drop 1 $ cycle grid)
    where
        solve' (x1, y1) (x2, y2) = x1*y2 - x2*y1

part1 :: Input -> Int
part1 inp = area + (sides `div` 2) + 1
    where
        sides = sum $ map nb inp
        area = solve points
        points = digOut inp (0, 0)

part2 :: Input -> Int
part2 = part1 . map cleanPlan
    where
        cleanPlan (DigPlan _ _ c) = (DigPlan d (read n) "")
            where
                n = "0x" ++ (take 5 $ drop 2 c)
                d = createDir . take 1 $ drop 7 c
                createDir "3" = North
                createDir "1" = South
                createDir "2" = West
                createDir "0" = East


main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    print $ part1 inp
    print $ part2 inp
