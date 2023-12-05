module Main where

import Data.List (sortOn)
import Data.List.Split ( chunksOf, splitOn )
import Data.Set (Set, fromList, singleton, empty, unions)
import qualified Data.Set as S (map)


data Almanac = Almanac {seeds :: [Int], converters :: [[(Int, Int, Int)]]} deriving (Show)

parseInput :: String -> Almanac
parseInput s = Almanac (seeds s) (converters s )
    where
        seeds = map read . words . drop (length "seeds: ") . head . lines
        getConverter = sortOn (\(_, s, _) -> s) . map (\[d, s, r] -> (d, s, r)) . map (map read . words) . drop 1 . lines
        converters = map (++[(0, 0, 9999999999)]) . map getConverter . drop 1 . splitOn "\n\n"

part1 :: Almanac -> Int
part1 = foldl1 min . locations
    where
        belongs e (_, s, r) = s <= e && e < s + r
        convert e (d, s, _) = e - s + d
        mapper c e = convert e . head $ dropWhile (not . belongs e) c
        folder curr c = mapper c <$> curr
        locations (Almanac seeds converters) = foldl (folder) seeds converters

part2 :: Almanac -> Int
part2 = foldl1 min . S.map fst . locations
    where
        before (_, s, _) (mi, ma)
            | s <= mi = empty
            | otherwise = singleton (mi, min (s-1) ma)
        inside (d, s, r) (mi, ma)
            | ma < s || s+r <= mi = empty
            | otherwise = singleton (cvrt (max s mi), cvrt (min (s+r-1) ma))
                where cvrt n = n - s + d
        after (_, s, r) (mi, ma)
            | ma < s+r = empty
            | otherwise = singleton (max mi (s+r), ma)

        convert :: [(Int, Int, Int)] -> (Int, Int) -> Set (Int, Int)
        convert [] r = singleton r
        convert (c:l) range = unions [(before c range), (inside c range), (unions $ S.map (convert l) (after c range))]
        folder :: Set (Int, Int) -> [(Int, Int, Int)] -> Set (Int, Int)
        folder ranges converters = unions $ S.map (convert converters) ranges
        seedRanges = fromList . map (\[e1, e2] -> (e1, e1 + e2 - 1)) . chunksOf 2
        locations :: Almanac -> Set (Int, Int)
        locations (Almanac seeds converters) = foldl (folder) (seedRanges seeds) converters





main :: IO ()
main = do
    print "Started"
    content <- readFile "input.txt"
    let almanac = parseInput content
    print $ 175622908 == part1 almanac
    print $ 5200543 == part2 almanac
