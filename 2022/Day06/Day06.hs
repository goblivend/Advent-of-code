module Main where

import Data.Containers.ListUtils

findMarker :: Int -> [Char] -> [Char] -> Int
findMarker nbchar marker (el:input)
    | (length $ nubOrd marker) == nbchar = 0
    | otherwise = 1 + findMarker nbchar (take nbchar (el:marker)) input

main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ map (findMarker 4 []) $ lines content
    print $ map (findMarker 14 []) $ lines content
