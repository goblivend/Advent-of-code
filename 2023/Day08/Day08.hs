module Main where

import Data.List (sortOn, group, sortOn, sort, isSuffixOf)
import Data.List.Split ( splitOn )
import Data.List.Unique (uniq)
import Data.Ord ()
import Data.Map (Map, (!), fromList, keys, member, insert, empty)
import Debug.Trace(trace)


data Input = Input {intrs:: [((String, String) -> String)], mps :: Map String (String, String)}

parseInput :: String -> Input
parseInput s = Input instructions mps
    where
        lined = lines s
        readInstr 'L' = fst
        readInstr 'R' = snd
        instructions = map readInstr $ head lined
        readTuple = (\[e1, e2] -> (drop 1 e1, take 3 e2)) . splitOn ", "
        mps = fromList . map ((\[e1, e2] -> (e1, readTuple e2)) . splitOn " = ") $ drop 2 lined

part1 :: Input -> String -> Int
part1 (Input instr mps) starting = recurse starting $ cycle instr
    where
        recurse "ZZZ" _ = 0
        recurse s (e:l) = 1 + recurse (e (mps ! s)) l

part2 :: Input -> (String -> Bool) -> Int
part2 (Input instr mps) starting = product . uniq . sort $ concatMap (multiplicators primes . findZ (cycle instr) 0) (filter starting $ keys mps)
    where
        findZ (e:l) i s
            | "Z" `isSuffixOf` s = i
            | otherwise          = findZ l (i+1) (e (mps ! s))
        primes = sieve [2..]
            where
                sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
        multiplicators (e:l) n
            | e > n =  []
            | n `mod` e == 0 = e : multiplicators (e:l) (div n e)
            | otherwise =  (multiplicators l n)



main :: IO ()
main = do
    print "Starting"
    content <- readFile "input.txt"
    let inp = parseInput content
    print $ part1 inp "AAA"
    print $ part2 inp ("A" `isSuffixOf`)
