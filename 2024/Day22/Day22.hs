module Main where

import Data.Bits
import Data.Function
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Map (Map)
import Data.Map qualified as M
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))

-- TODO: Cleanup imports after day done

type Input = [Int]

type Output = Int

parseInput :: String -> Input
parseInput = map read . lines

simulate :: Int -> Int
simulate n = f3 . f2 . f1 $ n
  where
    mix = xor
    prune = (`mod` 16777216)
    f1 n' = prune . mix n' $ n' * 64
    f2 n' = prune . mix n' $ n' `div` 32
    f3 n' = prune . mix n' $ n' * 2048

part1 :: Input -> Output
part1 input = sum . map (!! 2000) $ map (iterate simulate) input

uniqBy :: (Ord b) => (a -> b) -> [a] -> [a]
uniqBy f [] = []
uniqBy f (e : l) = e : sub f (f e) l
  where
    sub :: (Ord b) => (a -> b) -> b -> [a] -> [a]
    sub f _ [] = []
    sub f lastE (e : l)
      | currVal == lastE = sub f lastE l
      | otherwise = e : sub f currVal l
      where
        currVal = f e

part2 :: Input -> Output
part2 input = maximum bananas
  where
    prices = {-# SCC prices #-} map (map (`mod` 10)) $ map (iterate simulate) input

    pricesZipped :: [[(Int, Int)]]
    pricesZipped = {-# SCC pricesZipped #-} map (\n -> zip n $ drop 1 n) $ prices

    pricesDiff :: [[(Int, Int)]]
    pricesDiff = {-# SCC pricesDiff #-} map (map (\n -> second ((-) 0) $ second ((-) $ fst n) n)) $ pricesZipped

    withDiffPrefix :: [[(Int, [Int])]]
    withDiffPrefix = {-# SCC withDiffPrefix #-} map (\n -> take 1997 $ map (\l -> (fst (l !! (4)), map snd $ take 4 $ l)) $ tails n) $ pricesDiff

    sortedByDiffPref :: [(Int, [Int])]
    sortedByDiffPref = {-# SCC sortedByDiffPref #-} sortOn snd . concat $ map (uniqBy snd . sortOn snd) $ withDiffPrefix

    groupedByDiffPref :: [[(Int, [Int])]]
    groupedByDiffPref = {-# SCC groupedByDiffPref #-} groupBy (\e1 e2 -> snd e1 == snd e2) $ sortedByDiffPref

    bananas :: [Int]
    bananas = {-# SCC bananas #-} map (sum . map fst) $ groupedByDiffPref

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ part1 input -- (== 17262627539) $
  print $ part2 input -- (== 1986) $
