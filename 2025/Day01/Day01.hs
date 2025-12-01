module Main where

import Data.Tuple.Extra
import Debug.Trace
import System.Environment

type Input = [Int]

type Output = Int

parseInput :: String -> Input
parseInput = map mread . lines
  where
    mread ('L' : s) = -read s
    mread ('R' : s) = read s
    mread _ = 0

infixr 8 .:

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g = \x y -> f (g x y)

turnDial :: Int -> Int -> Int
turnDial = ((`mod` 100) .: (+))

part1 :: Int -> Input -> Output
part1 = length . filter (== 0) .: scanl turnDial

nbs0 :: Int -> Int -> Int
nbs0 prev n
  | n == 0 = 0
  | abs n >= 100 = cents n + nbs0 prev (signum n * rests n)
  | next == 0 = 1
  | signum n /= signum (next - prev) = 1
  | otherwise = 0
  where
    cents = (`div` 100) . abs
    rests = (`mod` 100) . abs
    next = turnDial prev n

part2 :: Int -> Input -> Output
part2 = sum . map (uncurry nbs0 . fix0) . uncurry zip .: (&&& id) . scanl turnDial
  where
    fix0 (0, n) | n < 0 = (100, n)
    fix0 p = p

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print $ part1 50 input
  print $ part2 50 input
