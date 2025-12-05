module Main where

import Data.Bits
import Data.List
import Data.List.Extra (replace)
import Data.List.Split
import Data.List.Unique
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))

-- TODO: Cleanup imports after day done

type Input = ([(Int, Int)], [Int])

type Output = Int

parseInput :: String -> Input
parseInput = (***) firstParse (map read) . both lines . toTuple . splitOn "\n\n"
  where
    toTuple (a : b : _) = (a, b)
    firstParse = map (toTuple . map read . splitOn "-")

part1 :: Input -> Output
part1 (fresh, ingredients) = length . filter (\i -> any (\(mi, ma) -> mi <= i && i <= ma) fresh) $ ingredients

part2 :: Input -> Output
part2 = sum . map ((+) 1 . uncurry (flip (-))) . mergeRanges . sortOn fst . fst
  where
    mergeRanges [lh] = [lh]
    mergeRanges ((l1, h1) : (l2, h2) : l)
      | h1 < l2 = (l1, h1) : mergeRanges ((l2, h2) : l)
      | otherwise = mergeRanges ((l1, max h1 h2) : l)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print input

  print $ 638 == part1 input
  print $ 352946349407338 == part2 input
