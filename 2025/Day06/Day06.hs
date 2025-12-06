module Main where

import Data.List (transpose)
import Data.List.Extra (split)
import Data.Tuple.Extra
import System.Environment

type Input = [((Int -> Int -> Int), [String])]

type Output = Int

parseInput :: String -> Input
parseInput = map getOp . map transpose . split (all (== ' ')) . transpose . lines
  where
    getOp = (***) (readOp . last) init . dupe
    readOp s
      | elem '*' s = (*)
      | elem '+' s = (+)

part1 :: Input -> Output
part1 = sum . map (uncurry foldl1 . second (map read))

part2 :: Input -> Output
part2 = part1 . map (second transpose)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print $ map snd input

  print $ part1 input
  print $ part2 input
