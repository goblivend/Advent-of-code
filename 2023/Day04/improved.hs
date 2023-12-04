module Main where

import Data.Char
import Data.List
import Data.List.Split
import Text.Regex.TDFA ( (=~) )


-- parseInput :: String -> Int
-- parseInput = head . map (makeCard) . matchCard
--     where
--         matchCard s = s =~ "(.*)[|](.*)" :: [[String]]
--         matchNumber s = s =~ "( *([0-9]+))" :: [[String]]
--         makeNumber [_, _, n] = read n :: Int
--         getNumbers = map (makeNumber) . matchNumber
--         winnings w d = length $ filter (`elem` w) d
--         makeCard [_, w, d] = winnings (drop 1 $ getNumbers w) (getNumbers d)

parseInput :: String -> [Int]
parseInput = map (getNumbers . break (== '|') . drop (length "Card   1:")) . lines
    where
        winnings :: [Int] -> [Int] -> Int
        winnings w d = length $ filter (`elem` w) d
        getNumbers (w, _ : n) = winnings (read <$> words w) (read <$> words n)

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
