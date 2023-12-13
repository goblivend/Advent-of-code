module Main where

-- import Control.Parallel.Strategies
import Data.List (sortOn, group, sortOn, sort, isPrefixOf, intercalate, transpose)
import Data.List.Split ( splitOn, linesBy )
import Data.Maybe
import Data.Ord ()
import Debug.Trace(trace)
import Data.Map (Map, empty, insert)
import qualified Data.Map as M (lookup)

type Input = [[[Bool]]]

parseInput :: String -> Input
parseInput = map (map (map (\c -> case c of {'#' -> True; '.' -> False})) . lines) . splitOn "\n\n"

nbLinesHori2 :: Int -> [[Bool]] -> Int
nbLinesHori2 nbDiff mp = sum $ filter isSymmetric [1..length mp -1]
    where
        diffs i = filter (== False) . concat . map ((map (uncurry (==)) . uncurry zip)) $ zip (drop i mp) (reverse $ take i mp)
        isSymmetric i = nbDiff == length (diffs i)

solve :: ([[Bool]] -> Int) -> [[Bool]] -> Int
solve f inp = (100 * f inp) + (f $ transpose inp)

part1 :: Input -> Int
part1 = sum . map (solve (nbLinesHori2 0))

part2 :: Input -> Int
part2 = sum . map (solve (nbLinesHori2 1))

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    print $ part1 inp
    print $ part2 inp
