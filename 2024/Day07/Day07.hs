module Main where

import System.Environment

type Input = [(Int, [Int])]
type Output = Int

parseInput :: String -> Input
parseInput = map parseLine . lines
    where
        getTestValue = read . takeWhile (not . (== ':'))
        getOperation = map (\c -> if c == ' ' then ',' else c) . drop 2 . dropWhile (not . (== ':'))
        parseLine l = (getTestValue l, read ("[" ++ getOperation l ++ "]") :: [Int])

canBeDone :: [(Int -> Int -> Int)] -> Int -> [Int] -> Bool
canBeDone moves val l = sub (head l) (tail l)
    where
        {-# SCC sub #-}
        sub :: Int -> [Int] -> Bool
        sub curr [] = val == curr
        sub curr (e:l) = any (\op -> sub (op curr e) l) moves

part1 :: Input -> Output
part1 = sum . map fst . filter (uncurry (canBeDone [{-# SCC add #-}(+), {-# SCC time #-}(*)]))

part2 :: Input -> Output
part2 = sum . map fst . filter (uncurry (canBeDone [{-# SCC add #-}(+), {-# SCC times #-} (*), {-# SCC concat #-} concatNb]))
    where
        concatNb :: Int -> Int -> Int
        concatNb a b = a * (((^) 10) $ length $ show b) + b

main :: IO ()
main = do
  args  <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print $ (== 1298300076754)   $ part1 input
  print $ (== 248427118972289) $ part2 input
