module Main where

import Data.List (sortOn, group, sortOn, sort, isSuffixOf)
import Data.List.Split ( splitOn )
import Data.Ord ()
import Data.Map (Map, (!), fromList, keys, member, insert, empty)
import Debug.Trace(trace)


data Input = Input {instrs:: [((String, String) -> String)], mp :: Map String (String, String)}

parseInput :: String -> Input
parseInput s = Input instructions mp
    where
        lined = lines s
        readInstr 'L' = fst
        readInstr 'R' = snd
        instructions = cycle . map readInstr $ head lined
        readTuple = (\[e1, e2] -> (drop 1 e1, take 3 e2)) . splitOn ", "
        mp = fromList . map ((\[e1, e2] -> (e1, readTuple e2)) . splitOn " = ") $ drop 2 lined

recurse :: Input -> (String -> Bool) -> String -> Int
recurse (i@(Input (e:l) mp)) matchEnd s
    | matchEnd s = 0
    | otherwise  = 1 + recurse (i {instrs = l}) matchEnd (e (mp ! s))

part1 :: Input -> Int
part1 input = recurse input (== "ZZZ") "AAA"

part2 :: Input -> (String -> Bool) -> Int
part2 (input@(Input _ mp)) starting = foldl1 lcm $ map (recurse input ("Z" `isSuffixOf`)) (filter starting $ keys mp)

main :: IO ()
main = do
    print "Starting"
    content <- readFile "input.txt"
    let inp = parseInput content
    print $ part1 inp
    print $ part2 inp ("A" `isSuffixOf`)
