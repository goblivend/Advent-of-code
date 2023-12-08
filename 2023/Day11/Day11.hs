module Main where

import Data.List (sortOn, group, sortOn, sort, isSuffixOf)
import Data.List.Split ( splitOn )
import Data.Ord ()
import Debug.Trace(trace)

data Input = Space {dims :: (Int, Int), galaxies :: [(Int, Int)]} deriving (Show)

parseInput :: String -> Input
parseInput s = Space (w, h) galaxies
    where
        ls = lines s
        (w, h) = (length $ ls !! 0, length ls)
        galaxies = concat . map (map snd . filter ((== '#') . fst)) . map (\(y, l) -> zip l . zip [0..] $ cycle [y])$ zip [0..] ls

expandUniverse :: Int -> Input -> Input
expandUniverse n (Space (w, h) gals) = Space (newW, newH) (expand (colExpander) (expCols) $ expand (rowExpander) (expRows) gals)
    where
        exp f = reverse . filter (\x -> null $ filter (\xy' -> f xy' == x) gals)
        expCols = exp fst [0..w-1]
        expRows = exp snd [0..h-1]
        (newW, newH) = (w + (n-1) * length expCols, h + (n-1) * length expRows)
        expand _        []    gal = gal
        expand expander (e:l) gal = expand expander l (map (expander e) gal)

        rowExpander y' xy = (fst xy, if snd xy < y' then snd xy else snd xy+n-1)
        colExpander x' xy = (if fst xy < x' then fst xy else fst xy+n-1, snd xy)

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x2-x1) + abs (y2-y1)

distances :: Input -> Int
distances (Space _ galaxies) = (`div` 2) . sum $ map (uncurry dist) $ [(xy, xy') | xy <-galaxies, xy'<-galaxies]

part1 :: Input -> Int
part1 = distances . expandUniverse 2

part2 :: Input -> Int
part2 = distances . expandUniverse 1000000

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    print $ 9639160 == part1 inp
    print $ 752936133304 == part2 inp
