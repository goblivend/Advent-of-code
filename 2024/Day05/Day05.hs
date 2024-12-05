module Main where

import Data.Bits
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Tuple.Extra
import Debug.Trace
import Text.Regex.TDFA ((=~))

data Input = Input {orders :: [(Int, Int)], updates :: [[Int]]} deriving (Show)

type Output = Int

parseInput :: String -> Input
parseInput s = Input order update
  where
    [orderStr, updateStr] = splitOn "\n\n" s
    order = map (\[a, b] -> (read a, read b)) . map (splitOn "|") $ lines orderStr
    update = map (\l -> read ("[" ++ l ++ "]")) $ lines updateStr

isUpdateValid :: [(Int, Int)] -> [Int] -> Bool
isUpdateValid order [] = True
isUpdateValid order (e : l)
  | any (`elem` l) . map fst $ filter ((== e) . snd) order = False
  | otherwise = (isUpdateValid order l)

middleOf :: [Int] -> Int
middleOf l = l !! (length l `div` 2)

part1 :: Input -> Output
part1 (Input order update) = sum . map middleOf $ filter (isUpdateValid order) update

part2 :: Input -> Output
part2 (Input order update) = sum . map middleOf $ map fixUpdate . filter (not . isUpdateValid order) $ update
  where
    orderUpdate :: [Int] -> [Int] -> [Int]
    orderUpdate [] completeUpdate = completeUpdate
    orderUpdate (e : l) currentOrder = orderUpdate l $ findCorrectSport e [] currentOrder
      where
        findCorrectSport e decided [] = decided ++ [e]
        findCorrectSport e decided (e2 : undecided)
          | isUpdateValid order (decided ++ [e, e2] ++ undecided) = decided ++ [e, e2] ++ undecided
          | otherwise = findCorrectSport e (decided ++ [e2]) undecided

    fixUpdate :: [Int] -> [Int]
    fixUpdate update = orderUpdate update []

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = parseInput content
  --   print input

  print $ part1 $ input

  print $ part2 $ input
