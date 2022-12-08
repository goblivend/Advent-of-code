module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

getVisible :: [[(Int, Bool)]] -> Int -> Int -> Bool
getVisible input x y
    | x == 0 || y == 0
             || x == (length input)-1
             || y == (length $ input !! 0)-1 = True
    | snd (input !! x !! y) = True
    | otherwise = x > 0                      && snd (input !! (x-1) !! y) && (input !! (x-1) !! y) < (input !! x !! y)
            || x < (length input)            && snd (input !! (x+1) !! y) && (input !! (x+1) !! y) < (input !! x !! y)
            || y > 0                         && snd (input !! x !! (y-1)) && (input !! x !! (y-1)) < (input !! x !! y)
            || (y+1) < (length $ input !! 0) && snd (input !! x !! (y+1)) && (input !! x !! (y+1)) < (input !! x !! y)

changeVisibility :: [[(Int, Bool)]] -> Int -> Int -> Bool -> [[(Int, Bool)]]
changeVisibility input x y v = take x input ++ [take y (input !! x) ++ [(fst (input !! x !! y), v)] ++ drop (y+1) (input !! x)] ++ drop (x+1) input

checkVisibility :: [[(Int, Bool)]] -> Int -> Int -> [[(Int, Bool)]]
checkVisibility input x y
    | y >= (length $ input !! 0) = input
    | otherwise = checkVisibility (changeVisibility input x y (getVisible input x y)) (mod (x+1) $ length input) (y + floor ((fromIntegral $ x+1) / (fromIntegral $ length input)))

doNChecks :: [[(Int, Bool)]] -> Int -> [[(Int, Bool)]]
doNChecks input 0 = input
doNChecks input n = doNChecks (checkVisibility input 0 0) (n-1)


main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = map (\line ->  map (\x -> (read x :: Int, False)) $ chunksOf 1 line) $ lines content
    --print $ checkVisibility input 1 0
    let visibility = doNChecks input 100
    print $ sum $ map (\x -> if snd x then 1 else 0) $ concat visibility
