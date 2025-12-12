module Main where

import Data.Bits
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

type Input = (Map Int [[Bool]], [((Int, Int), [Int])])

type Output = Int

parseInput :: String -> Input
parseInput = second (map (parseSpot . words) . last) . first (M.fromAscList . map parsePresent . init) . dupe . splitOn [[]] . lines
  where
    parsePresent (id : shape) = (read $ init id, map (map (== '#')) shape)
    parseSpot (dims : l) = (both read . second (tail . init) . break (== 'x') $ dims, map read l)

flipPresent = map reverse

rotatePresent = reverse . transpose

sizePresent :: [[Bool]] -> Int
sizePresent = length . filter id . concat

part1 :: Input -> Output
part1 (presents, spots) = length . filter canFit $ spots
  where
    canFit ((w, h), press) = (<= w * h) . sum . map (uncurry (*)) . zip press . map sizePresent $ M.elems presents

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print input

  print $ 433 == part1 input
