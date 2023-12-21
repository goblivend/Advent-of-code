module Main where

import Data.Char
import Data.Maybe ( fromJust, isJust )
import Debug.Trace(trace)
import Data.Tuple
import Data.Tuple.Extra
import Data.List.Split
import Data.Set (Set, member, fromList, toList)
import Data.Map (Map)
import Data.Matrix (Matrix, (!))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Matrix as Mat
import qualified Data.List as L

data Square = Garden | Rock deriving ( Eq)

instance Show Square where
    show Garden = "."
    show Rock   = "#"


type Grid = Matrix Square
data Input = Input { area ::(Int, Int), grid:: Grid, start::(Int, Int)} deriving (Show, Eq)

toStr :: Grid -> Set (Int, Int) -> String
toStr grid p = unlines $ [concat [if (x, y) `S.member` p then "O" else show (grid ! (x, y)) | x <-[1..Mat.ncols grid]]| y <-[1..Mat.nrows grid]]


parseInput :: String -> Input
parseInput str = Input (w, h) grid (x+1, y+1)
    where
        readSquare '#' = Rock
        readSquare _   = Garden
        grid = Mat.fromLists . L.transpose . L.transpose . L.transpose $ map (map readSquare) $ lines str
        y = length $ takeWhile ('S' `notElem`)   $ lines str
        x = length $ takeWhile (/= 'S') . (!! y) $ lines str
        h = Mat.nrows grid
        w = Mat.ncols grid

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (x' ,y') = (x+ x', y+y')

goNorth :: (Int, Int) -> (Int, Int)
goNorth = add ( 0, -1)
goSouth = add ( 0,  1)
goEast  = add ( 1,  0)
goWest  = add (-1,  0)

inGrid :: (Int, Int) -> (Int, Int) -> (Int, Int)
inGrid (w, h) (x, y) = (((x-1) `mod` w) +1, ((y-1) `mod` h)  +1)

walk1step :: Input -> [(Int, Int)] -> Set (Int, Int)
walk1step _ [] = S.empty
walk1step (inp@(Input wh grid _)) (xy:l) = availableHere `S.union` (walk1step inp l)
    where
        availableHere = S.fromList . filter ((==Garden) . (grid !) . inGrid wh) $ map ($ xy) [goWest, goNorth, goSouth, goEast]

walk :: Input -> [Int]
walk inp = map S.size $ iterate (walk1step inp . S.toList) (S.singleton $ start inp)

part1 :: Input -> Int
part1 =  (!! 64) . walk


extrapolate :: Input -> Int -> Int
extrapolate inp n = a*(x-1)*x+p1*x+a0
    where
        allRes = walk inp
        w = Mat.ncols . grid $ inp
        a0 = allRes !! (div w 2)
        a1 = allRes !! (w + div w 2)
        a2 = allRes !! (2*w + div w 2)
        p1 = (a1-a0)
        p2 = (a2-a1)
        a = (p2-p1) `div` 2
        x = n `div` w

part2 :: Input -> Int
part2 = (`extrapolate` 26501365)

main :: IO ()
main = do

    content <- readFile "input.txt"
    let inp = parseInput content
    -- print inp
    print $ part1 inp
    print $ part2 inp
