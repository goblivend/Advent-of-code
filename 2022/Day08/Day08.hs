module Main where

import Data.List.Split

-- Check if a pair of coordinates belongs in a field
inRange :: [[Int]] -> Int -> Int -> Bool
inRange input x y = x >= 0 && y >= 0 && y < length input && x < length (input !! 0)

-- Check if from a point we can see in any direction
checkVisibility :: [[Int]] -> Int -> Int -> Bool
checkVisibility input x y = left || right || top || bottom
            where
                right  = all id (map (\x2 -> input !! y !! x > input !! y !! x2) [x+1..(length $ input !! 0) - 1])
                left   = all id (map (\x2 -> input !! y !! x > input !! y !! x2) [0..x-1])
                top    = all id (map (\y2 -> input !! y !! x > input !! y2 !! x) [0..y-1])
                bottom =  all id (map (\y2 -> input !! y !! x > input !! y2 !! x) [y+1..(length input) - 1])

-- Calculates the number of tree we can look above until one is blocking the view
calculatePart :: Int -> [[Int]] -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Int
calculatePart max input x y tox2 toy2
        | not (inRange input x y) = 0
        | max <= input !! y !! x = 1
        | otherwise = 1 + calculatePart max input (tox2 x) (toy2 y) tox2 toy2

-- Calculates the value of Scenic as : left*right*top*bottom
getScenic :: [[Int]] -> Int -> Int -> Int
getScenic input x y = left * right * top * bottom
            where 
                right  = calculatePart (input !! y !! x) input (x+1) y (\x -> x+1) (\y -> y)
                left   = calculatePart (input !! y !! x) input (x-1) y (\x -> x-1) (\y -> y)
                bottom = calculatePart (input !! y !! x) input x (y+1) (\x -> x) (\y -> y+1)
                top    = calculatePart (input !! y !! x) input x (y-1) (\x -> x) (\y -> y-1)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = map (\line ->  map (\x -> read x :: Int) $ chunksOf 1 line) $ lines content

    let visibility = map (\y -> map (\x -> checkVisibility input x y) [0..(length (input !! y)) - 1]) [0..(length input) - 1]
    print $ sum $ map (\e -> if e then 1 else 0) $ concat visibility

    print $ maximum $ concat $ map (\y -> map (\x -> getScenic input x y) [0..(length (input !! 0)) - 1]) [0..(length input) - 2]
