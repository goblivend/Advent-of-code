module Main where

import Data.List (sortOn, group, sortOn, sort, isPrefixOf, intercalate)
import Data.List.Split ( splitOn )
import Data.Maybe
import Data.Ord ()
import Debug.Trace(trace)
import Data.Map (Map, empty, insert)
import qualified Data.Map as M (lookup)

type Input = [(String, [Int])]

parseInput :: String -> Input
parseInput = map ((\[s, r] -> (s, map read $ splitOn "," r)). words) . lines

updateCache :: Map (String, [Int]) Int -> (String, [Int]) -> Int -> (Map (String, [Int]) Int, Int)
updateCache m k v = (insert k v m, v)

nbArrangements :: (String, [Int]) -> Int
nbArrangements (s, groups) = (snd $ nbA empty "" s [0])
    where
        validate :: [Int] -> Int
        validate l = fromEnum $ length l' == length groups && last groups == head l'
            where
                l' = dropWhile (== 0) l

        conca :: [Int] -> Char -> [Int]
        conca (0:l) '.' = 0:l
        conca l     '.' = 0:l
        conca (e:l) '#' = (e+1:l)

        preVali acc = if null acc then True else (len <= length groups && first acc && second acc)
            where
                len = length $ dropWhile (== 0) acc
                second (_:e:_) = e == (groups !! (length acc - 2))
                second _ = True
                first (0:_) = True
                first (e:_) = (e <= (groups !! (length acc - 1)))

        nbA :: Map (String, [Int]) Int -> String -> String -> [Int] -> (Map (String, [Int]) Int, Int)
        nbA m seen s acc = if preVali acc then (updateCache m' (seen, acc) n) else (updateCache m (seen, acc) 0)
            where
                (m', n) = _nbA m seen s acc


        _nbA m seen s a
            | isJust v = (m, fromJust v)
                where v =  M.lookup (seen, a) m

        _nbA m seen [] acc = updateCache m (seen, acc) (validate acc)
        _nbA m seen ('?':s) acc = updateCache m'' (seen, acc) (n+n')
            where
                (m', n)   = nbA m  ('?':seen) s (conca acc '.')
                (m'', n') = nbA m' ('?':seen) s (conca acc '#')
        _nbA m seen (c:s) acc = nbA m (c:seen) s (conca acc c)

part1 :: Input -> Int
part1 = sum . map nbArrangements

part2 :: Input -> Int
part2 = part1 . map (\(s, g) -> (intercalate "?" . take 5 $ repeat s, concat . take 5 $ repeat g))

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    print $ part1 inp
    print $ part2 inp
