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

type Input = ([String], [String])

type Output = Int

parseInput :: String -> Input
parseInput input = (towels, patterns)
  where
    [firstLine, rest] = splitOn "\n\n" input
    towels = splitOn ", " firstLine
    patterns = lines rest

part1 :: Input -> Output
part1 (towels, patterns) = length $ filter (doAble S.empty . S.singleton) patterns
  where
    doAble :: Set String -> Set String -> Bool
    doAble seen patterns
      | S.null patterns = False
      | [] `S.member` patterns = True
      | otherwise = doAble newSeen newPatterns
      where
        newSeen = S.union seen patterns
        newPatterns = (S.\\ newSeen) . S.fromList . concat . map (\patt -> map (\pre -> (drop (length pre) patt)) $ prefixesOf patt) $ S.toList patterns
        prefixesOf patt = filter (`isPrefixOf` patt) towels

part2 :: Input -> Output
part2 (towels, patterns) = sum $ map (snd . nbSolutions M.empty) patterns
  where
    nbSolutions :: Map String Int -> String -> (Map String Int, Int)
    nbSolutions seen pattern
      | pattern == [] = (seen, 1)
      | pattern `M.member` seen = (seen, seen M.! pattern)
      | otherwise = (M.insert pattern newRes newSeen, newRes)
      where
        newPatterns = map (\pre -> (drop (length pre) pattern)) $ filter (`isPrefixOf` pattern) towels
        (newSeen, newRes) = foldl (\(s, r) p -> second (+ r) $ nbSolutions s p) (seen, 0) newPatterns

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ part1 input
  print $ part2 input
