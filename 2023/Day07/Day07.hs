module Main where

import Data.List (sortOn, group, sortOn, sort)
import Data.Ord
import Data.Map (Map, (!), fromList)

data Hand = Hand{raw :: [Int], groupped :: [[Int]], bid :: Integer} deriving (Show)

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

parseInput :: Map Char Int -> String -> [Hand]
parseInput values = map (parseHand . words) . lines
    where
        groupHand = reverse . sortOn length . group . sort . map (values !)
        parseHand [h, b] = Hand (map (values !) h) (groupHand h) (read b)

solve :: [Hand] -> Integer
solve = sum . map (uncurry (*)) . zip [1..] . map bid . sort

part1 :: Map Char Int -> String -> Integer
part1 m = solve . parseInput m

part2 :: Map Char Int -> String -> Integer
part2 m = solve . map fixHand . parseInput m
    where
        regroupHand groups = newGroups . reverse . sortOn length $ drop (fromEnum hasJs) groups
            where
                hasJs = (m ! 'J') `elem` (groups !! 0) -- Jockers have lowest values so first when sorted
                js = if hasJs then head groups else []
                newGroups [] = [js]
                newGroups (e:l) = (e ++ js):l

        fixHand (Hand r h b) = Hand r (regroupHand (sort h)) b

main :: IO ()
main = do
    print "Starting"
    content <- readFile "input.txt"
    let values1 = fromList [('A', 14), ('K', 13), ('Q', 12), ('J', 11), ('T', 10), ('9', 9), ('8', 8), ('7', 7), ('6', 6), ('5', 5), ('4', 4), ('3', 3), ('2', 2)]
    let values2 = fromList [('A', 14), ('K', 13), ('Q', 12), ('T', 10), ('9', 9), ('8', 8), ('7', 7), ('6', 6), ('5', 5), ('4', 4), ('3', 3), ('2', 2), ('J', 1)]


    print $ 245794640 == part1 values1 content
    print $ 247899149 == part2 values2 content
