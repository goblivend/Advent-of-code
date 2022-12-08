module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

checkVisibility :: [[Int]] -> Int -> Int -> Bool
checkVisibility input x y = 
                            all (\e -> e) (map (\x2 -> input !! y !! x > input !! y !! x2) [x+1..(length $ input !! 0) - 1])
                         || all (\e -> e) (map (\x2 -> input !! y !! x > input !! y !! x2) [0..x-1])
                         || all (\e -> e) (map (\y2 -> input !! y !! x > input !! y2 !! x) [0..y-1])
                         || all (\e -> e) (map (\y2 -> input !! y !! x > input !! y2 !! x) [y+1..(length input) - 1])

--changeVisibility :: [[(Int, Bool)]] -> Int -> Int -> Bool -> [[(Int, Bool)]]
--changeVisibility input x y v = take x input ++ [take y (input !! x) ++ [(fst (input !! x !! y), v)] ++ drop (y+1) (input !! x)] ++ drop (x+1) input




main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = map (\line ->  map (\x -> read x :: Int) $ chunksOf 1 line) $ lines content
    let visibility = map (\y -> map (\x -> checkVisibility input x y) [0..(length (input !! y)) - 1]) [0..(length input) - 1]
    --print visibility
    --let range = [2+1..(length (input !! 1)) - 1]
    --print $ all (\e -> e) $ map (\x -> (x > input !! 1 !! x)) range --  checkBigger 5 input 2 1 (\x -> x+1) (\y -> y) 

    print $ sum $ map (\e -> if e then 1 else 0) $ concat visibility
