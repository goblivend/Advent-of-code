module Main where

import Data.Bits
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Map (Map)
import Data.Map qualified as M
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))

-- TODO: Cleanup imports after day done

type Input = ([[Bool]], Int)

type Output = Int

parseInput :: String -> Input
parseInput = first getMat . second getS . dupe . lines
  where
    getS = fromJust . elemIndex 'S' . head
    getMat = map (map (\c -> if c == '^' then True else False)) . filterLines
    filterLines :: [String] -> [String]
    filterLines = filter (not . all ((==) '.')) . tail

part1 :: Input -> Output
part1 = uncurry (help) . second (S.singleton)
  where
    help :: [[Bool]] -> Set Int -> Int
    help [] xs = 0
    help (line : m) xs = countHits + (help m newXs)
      where
        countHits = S.size (S.filter (\x -> line !! x) xs)
        newXs = S.fromAscList $ foldl (\l x -> l ++ if line !! x then [x - 1, x + 1] else [x]) [] xs

part2 :: Input -> Output
part2 = snd . head . uncurry help . second (singleton)
  where
    help :: [[Bool]] -> [Int] -> [(Int, Int)]
    help [] xs = map (\x -> (x, 1)) xs
    help (line : m) xs = map (\x -> (x, sum . map (nextRes M.!) $ newXsmp M.! x)) xs
      where
        newXsmp :: Map Int [Int]
        newXsmp = foldl (\mp x -> M.insert x (if line !! x then [x - 1, x + 1] else [x]) mp) M.empty xs
        newXs = sortUniq . concat $ M.elems newXsmp
        nextRes = M.fromList $ help m newXs

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ 1499 == part1 input
  -- print $ part1 input
  print $ 24743903847942 == part2 input
