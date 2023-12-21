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
import Data.NumInstances.Tuple
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

inGrid :: (Int, Int) -> (Int, Int) -> (Int, Int)
inGrid (w, h) (x, y) = (((x-1) `mod` w) +1, ((y-1) `mod` h)  +1)

walk1step :: Input -> (Set (Int, Int), Set (Int, Int), [Int]) -> (Set (Int, Int), Set (Int, Int), [Int])
walk1step (inp@(Input wh grid _)) (curr, old, sizes) = (new, curr, (sizes !! 1 + S.size curr):sizes)
    where
        new = S.unions $ S.map availableHere curr
        availableHere xy = S.fromList
                . filter ((==Garden) . (grid !) . inGrid wh)
                . filter (`S.notMember` old)
                . map (+xy)
                $ [(-1, 0), (0, -1), (0, 1), (1, 0)]

walk :: Input -> [Int]
walk inp = drop 1 . map (head . thd3) $ iterate (walk1step inp) (S.singleton $ start inp, S.empty, [0, 0])

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


-- Invalid Estimate version : works to get roughly the results quite fast but not the exact ones
flood :: Input -> Int -> Int
flood inp n = S.size                                              .
        S.filter ((== Garden) . (grid inp !) . inGrid (area inp)) .
        S.map (+ (start inp))                                     .
        S.fromList                                                $
        concat [[(x, y), (x, -y), (-x, y), (-x, -y)] | y<-[0..n], x<-[0..n-y], (x+y) `mod` 2 == n `mod` 2]


extrapolate2 :: Input -> Int -> Int
extrapolate2 inp n = a*(x-1)*x+p1*x+a0
    where
        w = Mat.ncols . grid $ inp
        a0 = flood inp (div w 2)
        a1 = flood inp (w + div w 2)
        a2 = flood inp (2*w + div w 2)
        p1 = (a1-a0)
        p2 = (a2-a1)
        a = (p2-p1) `div` 2
        x = n `div` w


main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    -- print inp
    print $ part1 inp
    print $ part2 inp
    print $ flood inp 64
