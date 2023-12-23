module Main where

import Data.Char
import Data.Maybe ( fromJust, isJust )
import Debug.Trace(trace)
import Data.Tuple
import Data.Tuple.Extra;
import Data.List.Split
import Data.List.Unique
import Data.Set (Set, member, fromList, toList)
-- import Data.Map (Map, (!))
import Data.NumInstances.Tuple
import Data.Matrix (Matrix, (!))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Matrix as Mat
import qualified Data.List as L

data Block = Wall | Path | Slope {dir::(Int, Int)} deriving (Eq)
data Input = Input {start ::(Int, Int), end::(Int, Int), grid:: [[Block]]}

instance Show Block where
    show Wall = "#"
    show Path = "."
    show (Slope (-1,  0)) = "<"
    show (Slope ( 1,  0)) = ">"
    show (Slope ( 0, -1)) = "^"
    show (Slope ( 0,  1)) = "v"

toStr mat = unlines . map (concat . map show) $  mat

parseInput :: String -> Input
parseInput inp = Input (xStart, 0) (yStart, length lined-1) grid
    where
        readBlock '.' = Path
        readBlock '#' = Wall
        readBlock '<' = Slope (-1,  0)
        readBlock '>' = Slope ( 1,  0)
        readBlock '^' = Slope ( 0, -1)
        readBlock 'v' = Slope ( 0,  1)
        lined = lines inp
        grid = map (map readBlock) $ lined
        xStart = length . takeWhile (/= '.') . (!! 0) $ lined
        yStart = length . takeWhile (/= '.') . (!! (length lined -1)) $ lined

at :: Input -> (Int, Int) -> Block
at (Input _ _ grid) (x, y) = grid !! y !! x

pathFind :: Input -> Set (Int, Int) -> (Int, Int) -> Block -> Set (Int, Int)
pathFind inp seen curr Wall = S.empty
pathFind inp seen curr block
    | curr == start inp = pathFind inp (S.insert curr seen) (curr + (0, 1)) (at inp (curr + (0, 1)))
    | curr == end inp = seen
    | curr `S.member` seen = S.empty
pathFind inp seen curr (Slope dir) = pathFind inp (S.insert curr seen) (curr + dir) (at inp (curr + dir))
pathFind inp seen curr Path
    | otherwise         =  (last . L.sortBy (\s1 s2 -> compare (S.size s1) (S.size s2)) . map ((\next -> pathFind inp (S.insert curr seen) next (at inp next)) . (+curr)) $ [(1, 0), (-1, 0), (0, -1), (0, 1)])

part1 :: Input -> Int
part1 inp = S.size $ pathFind inp S.empty (start inp) (at inp (start inp))


pathFind2 :: Input -> Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
pathFind2 inp seen curr
    | at inp curr == Wall = S.empty
    | curr == start inp = pathFind2 inp (S.insert curr seen) (curr + (0, 1))
    | curr == end inp = seen
    | curr `S.member` seen = S.empty
    | otherwise =  (last . L.sortBy (\s1 s2 -> compare (S.size s1) (S.size s2)) . map ((pathFind2 inp (S.insert curr seen)) . (+curr)) $ [(1, 0), (-1, 0), (0, -1), (0, 1)])

part2 :: Input -> Int
part2 inp =S.size $ pathFind2 inp S.empty (start inp)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    -- print inp
    -- putStr $ toStr $ grid inp
    -- print $ (grid inp) !! 3 !! 1
    print $ part1 inp
    print $ part2 inp
