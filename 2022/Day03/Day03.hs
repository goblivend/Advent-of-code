module Main where
import Data.List
import Data.Char
import Data.Set

getPriority :: Char -> Int
getPriority c = if 'A' <= c && c <= 'Z' then (ord c) - (ord 'A') + 27
                else (ord c) - (ord 'a') + 1

splitHalf :: [a] -> ([a], [a])
splitHalf xs = Data.List.splitAt (length xs `div` 2) xs

inters :: ([Int], [Int]) -> [Int]
inters ([], _) = []
inters (_, []) = []
inters (xs, ys) = mkUniq $ Data.List.filter (\x -> x `elem` xs) ys

-- From Docs :
mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

sg :: Ord a => [a] -> [[a]]
sg = group . sort

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = Data.List.filter (p . length) . sg

unique :: Ord a => [a] -> [a]
unique = concat . filterByLength (==3)

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = Data.List.map (Data.List.take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (Data.List.drop i l) c n

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = Data.List.map (\line -> Data.List.map (\a -> getPriority a) line) $ lines content

    print $ sum $ concat $ Data.List.map (\line -> (inters $ splitHalf line)) $ input

    print $ sum $ concat $ Data.List.map (\chunk -> mkUniq $ unique $ concat chunk)$ chunksOf 3 $ Data.List.map (\line -> mkUniq line) input
    --print $ Data.List.map (\line -> (splitHalf line)) $ input
    --print $ inters (splitHalf [1,2,2,3,2,3,2,4])
