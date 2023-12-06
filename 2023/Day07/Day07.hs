module Main where

import Data.List (sortOn, group, groupBy, sort, sortBy)
import Data.Ord
import Data.List.Split ( chunksOf, splitOn )
import Data.Set (Set, fromList, singleton, empty, unions)
import qualified Data.Set as S (map)

data Hand = Hand{raw :: [Int], groupped :: [[Int]], bid :: Integer} deriving (Show)

parseInput1 :: String -> [Hand]
parseInput1 = map (parseHand . words) . lines
    where
        readChar 'A' = 14
        readChar 'K' = 13
        readChar 'Q' = 12
        readChar 'J' = 11
        readChar 'T' = 10
        readChar c = read [c]
        groupHand = reverse . sortOn length . group . sort . map readChar
        parseHand [h, b] = Hand (map readChar h) (groupHand h) (read b)

instance Eq Hand where
    (==) (Hand r1 _ _) (Hand r2 _ _) = r1 == r2

instance Ord Hand where
    compare (Hand r1 g1 _) (Hand r2 g2 _)
        |                  length g1        < length g2        = GT
        |                  length g1        > length g2        = LT
        |                  length (g1 !! 0) < length (g2 !! 0) = LT
        |                  length (g1 !! 0) > length (g2 !! 0) = GT
        | length g1 > 1 && length (g1 !! 1) < length (g2 !! 1) = LT
        | length g1 > 1 && length (g1 !! 1) > length (g2 !! 1) = GT
        | otherwise                                            = compare r1 r2

parseInput2 :: String -> [Hand]
parseInput2 = map (parseHand . words) . lines
    where
        readChar 'A' = 14
        readChar 'K' = 13
        readChar 'Q' = 12
        readChar 'T' = 11
        readChar 'J' = 1
        readChar c = read [c]
        groupHand :: String -> [[Int]]
        groupHand h = newGroups (reverse . sortOn length $ (if 1 `elem` (head groups) then drop 1 groups else groups))
            where
                groups =  group . sort $ map readChar h
                hasJs = 1 `elem` (groups !! 0)
                js = if hasJs then head groups else []
                newGroups :: [[Int]] -> [[Int]]
                newGroups [] = [js]
                newGroups (e:l) = (e ++ js):l

        parseHand [h, b] = Hand (map readChar h) (groupHand h) (read b)

part1 :: [Hand] -> Integer
part1 = sum . map (uncurry (*)) . zip [1..] . map bid . sort

main :: IO ()
main = do
    print "Starting"
    content <- readFile "input.txt"
    print $ part1 $ parseInput1 content
    print $ part1 $ parseInput2 content
