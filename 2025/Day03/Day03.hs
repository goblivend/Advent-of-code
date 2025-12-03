module Main where

import System.Environment

type Input = [[Int]]

type Output = Int

infixr 8 .:

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g = \x y -> f (g x y)

parseInput :: String -> Input
parseInput = map (map (read . pure)) . lines

bests :: Int -> [Int] -> [Int]
bests 0 l = []
bests i l
  | nbl <= i = l
  | otherwise = best : bests (i - 1) (tail $ dropWhile (/= best) l)
  where
    nbl = length l
    best = maximum $ take (nbl - i + 1) l

findBestsOf :: Int -> [Int] -> Int
findBestsOf = read . concatMap show .: bests

part1 :: Input -> Output
part1 = sum . map (findBestsOf 2)

part2 :: Input -> Output
part2 = sum . map (findBestsOf 12)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print input

  print $ part1 input
  print $ part2 input
