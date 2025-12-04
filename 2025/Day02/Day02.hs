module Main where

import Data.List.Extra (replace)
import Data.List.Split (splitOn)
import System.Environment

type Input = [(Int, Int)]

type Output = Int

parseInput :: String -> Input
parseInput = map (read . ("(" ++) . (++ ")") . replace "-" ",") . splitOn ","

isInvalidForN :: Int -> String -> Bool
isInvalidForN n s
  | l `mod` n /= 0 = False
  | otherwise = all null . splitOn (take delta s) $ drop delta s -- Could have changed to `uncurry splitOn $ splitAt delta s` but it makes the solution 10% slower
  where
    l = length s
    delta = div l n

sumInvalid :: (String -> Bool) -> [(Int, Int)] -> Int
sumInvalid isInvalid = sum . filter (isInvalid . show) . concat . map (uncurry enumFromTo)

part1 :: Input -> Output
part1 = sumInvalid isDouble
  where
    -- Sligthly faster
    -- isDouble s = uncurry (==) $ splitAt (length s `div` 2) s
    -- Using existing code
    isDouble = isInvalidForN 2

part2 :: Input -> Output
part2 = sumInvalid isInvalidForAll
  where
    isInvalidForAll s = not . null . filter (`isInvalidForN` s) $ [2 .. length s]

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print input

  print $ part1 input
  print $ part2 input
