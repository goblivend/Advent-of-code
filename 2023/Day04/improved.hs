module Main where

import Data.Char
import Data.List
import Data.List.Split
import Text.Regex.TDFA ( (=~) )

parseInput :: String -> [Int]
parseInput = map (getNumbers . break (== '|') . drop (length "Card   1:")) . lines
    where
        winnings w d = length $ filter (`elem` w) d
        getNumbers (w, _ : n) = winnings (words w) (words n)

part1 :: [Int] -> Int
part1 = sum . map ((2 ^) . (+ (-1))) . filter (/= 0)

part2 :: [Int] -> Int
part2 = sum . foldr (\w acc -> 1 + sum (take w acc) : acc) []

main :: IO ()
main = do
    content <- readFile "input.txt"
    let cards = parseInput content
    print $ part1 cards
    print $ part2 cards
