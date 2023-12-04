module Main where

import Data.Char
import Data.List
import Data.List.Split
import Text.Regex.TDFA

data Card = Card {winning :: [Int], drawn :: [Int]} deriving (Show, Eq)

parseInput :: String -> Card
parseInput = head . map (makeCard) . matchCard
    where
        matchCard s = s =~ "(.*)[|](.*)" :: [[String]]
        matchNumber s = s =~ "( *([0-9]+))" :: [[String]]
        makeNumber [_, _, n] = read n
        getNumbers = map (makeNumber) . matchNumber
        makeCard [_, w, d] = Card (drop 1 $ getNumbers w) (getNumbers d)

winnings :: Card -> [Int]
winnings (Card w d) = filter (`elem` w) d

part1 :: [Card] -> Int
part1 = sum . map (points . winning)
    where
        points [] = 0
        points (e:[]) = 1
        points (e:s) = 2 * points s

getCardsNb :: [Int] -> [Card] -> [Int]
getCardsNb acc [] = acc
getCardsNb acc (e:s) = getCardsNb (current:acc) s
    where
        nbMatching = length $ winnings e
        cardsWon = init [0..nbMatching]
        current = 1 + (sum $ map (acc !!) cardsWon)

part2 :: [Card] -> Int
part2 cards = sum $ map (cardsNb !!) [0..length cards - 1]
    where
        cardsNb = getCardsNb [] $ reverse $ cards

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = lines content
    let cards = map (parseInput) input

    print $ part1 cards
    print $ part2 cards
