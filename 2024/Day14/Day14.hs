module Main where

import Data.List
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import System.Environment
import Text.Regex.TDFA ((=~))

type Input = Set ((Int, Int), (Int, Int))

type Output = Int

parseInput :: String -> Input
parseInput = S.fromList . map parseLine . lines
  where
    parseLine = (\[p, v] -> (read p, read v)) . map parseTuple . words
    parseTuple s = (\[[_, t]] -> "(" ++ t ++ ")") $ ((s =~ "=(-?[0-9]+,-?[0-9]+)") :: [[String]])

moveAround :: (Int, Int) -> Input -> Input
moveAround (w, h) = S.map moveRobot
  where
    moveRobot ((px, py), (vx, vy)) = (((px + vx) `mod` w, (py + vy) `mod` h), (vx, vy))

getQuadrant :: (Int, Int) -> (Int, Int) -> (Bool, Bool)
getQuadrant (w, h) (x, y) = (x <= div w 2, y <= div h 2)

part1 :: (Int, Int) -> Input -> Output
part1 wh = product . map length . groupByQuadrants . notAtCenterLines . robotsPositions
  where
    robotsPositions = map fst . S.toList . (!! 100) . iterate (moveAround wh)
    midX = (fst wh) `div` 2
    midY = (snd wh) `div` 2
    notAtCenterLines = filter (\(x, y) -> x /= midX && y /= midY)
    groupByQuadrants = groupBy (\e1 e2 -> snd e1 == snd e2) . sortOn snd . map (second (getQuadrant wh) . dupe)

display :: (Int, Int) -> Set (Int, Int) -> [String]
display (w, h) inp = [[if (x, y) `S.member` inp then 'x' else '.' | x <- [0 .. w]] | y <- [0 .. h]]

part2 :: (Int, Int) -> Input -> Output
part2 wh = fst . head . filter (hasLongLine . snd) . zip [0 ..] . iterate (moveAround wh)
  where
    hasLongLine = (=~ "x{10}") . concat . display wh . S.map fst

showAftern :: (Int, Int) -> Input -> Int -> String
showAftern wh inp n = unlines . display wh . S.map fst . (!! n) $ iterate (moveAround wh) inp

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content
  let wh = (101, 103)

  print $ part1 wh input
  print $ part2 wh input

-- putStrLn . showAftern wh input $ part2 wh input
