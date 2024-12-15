module Main where

import Data.Bits
import Data.List
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

data Dir = UP | LEFT | RIGHT | DOWN deriving (Show, Eq, Ord)

type Input = (Set (Int, Int), Set (Int, Int), [Dir], (Int, Int))

type Output = Int

parseInput1 :: String -> Input
parseInput1 inp = (walls, boxes, parseMoves moves, robotPos)
  where
    [gridStr, moves] = splitOn "\n\n" inp
    (w, h) = (length $ head $ lines gridStr, length $ lines gridStr)
    wharehouse = lines $ gridStr
    parseMoves = map parseMove . concat . lines
    parseMove '<' = LEFT
    parseMove '^' = UP
    parseMove '>' = RIGHT
    parseMove 'v' = DOWN
    walls = S.fromList [(y, x) | y <- [0 .. h - 1], x <- [0 .. w - 1], wharehouse !! y !! x == '#']
    boxes = S.fromList [(y, x) | y <- [0 .. h - 1], x <- [0 .. w - 1], wharehouse !! y !! x == 'O']
    robotPos = head [(y, x) | y <- [0 .. h - 1], x <- [0 .. w - 1], wharehouse !! y !! x == '@']

parseInput2 :: String -> Input
parseInput2 inp = (walls, boxes, parseMoves moves, robotPos)
  where
    [gridStr, moves] = splitOn "\n\n" inp
    (w, h) = (length $ head $ lines gridStr, length $ lines gridStr)
    wharehouse = lines $ gridStr
    parseMoves = map parseMove . concat . lines
    parseMove '<' = LEFT
    parseMove '^' = UP
    parseMove '>' = RIGHT
    parseMove 'v' = DOWN
    walls = S.fromList $ concat [[(y, 2 * x), (y, 2 * x + 1)] | y <- [0 .. h - 1], x <- [0 .. w - 1], wharehouse !! y !! x == '#']
    boxes = S.fromList [(y, 2 * x) | y <- [0 .. h - 1], x <- [0 .. w - 1], wharehouse !! y !! x == 'O']
    robotPos = head [(y, 2 * x) | y <- [0 .. h - 1], x <- [0 .. w - 1], wharehouse !! y !! x == '@']

move :: (Int, Int) -> Dir -> (Int, Int)
move (y, x) UP = (y - 1, x)
move (y, x) DOWN = (y + 1, x)
move (y, x) LEFT = (y, x - 1)
move (y, x) RIGHT = (y, x + 1)

processMoves :: Input -> Input
processMoves (walls, boxes, [], pos) = (walls, boxes, [], pos)
processMoves (walls, boxes, (e : l), pos) = processMoves (walls, newBoxes, l, newPos)
  where
    (newBoxes, newPos) = processMove e
    processMove e
      | pos' `S.member` walls = (boxes, pos)
      | pos' `S.notMember` boxes = (boxes, pos')
      | pos' `S.notMember` boxes' = (boxes', pos')
      | otherwise = (boxes, pos)
      where
        pos' = move pos e
        boxes' = processMoveBox e pos'

    processMoveBox e pos'
      | pos'' `S.member` walls = boxes
      | pos'' `S.notMember` boxes = S.insert pos'' $ S.delete pos' boxes
      | pos'' `S.notMember` boxes' = S.insert pos'' $ S.delete pos' boxes'
      | otherwise = boxes
      where
        boxes' = processMoveBox e (move pos' e)
        pos'' = move pos' e

gps :: [(Int, Int)] -> Output
gps = sum . map (\(y, x) -> 100 * y + x)

part1 :: Input -> Output
part1 input = gps $ S.toList boxes
  where
    (_, boxes, _, _) = processMoves input

processMoves2 :: Input -> Input
processMoves2 (walls, boxes, [], pos) = (walls, boxes, [], pos)
processMoves2 (walls, boxes, (e : l), pos) = processMoves2 (walls, newBoxes, l, newPos)
  where
    (newBoxes, newPos) = processMove e
    processMove e
      | pos' `S.member` walls = (boxes, pos)
      | pos' `S.notMember` boxes && (y, x - 1) `S.notMember` boxes = (boxes, pos')
      | pos' `S.notMember` boxes' && (y, x - 1) `S.notMember` boxes' = (boxes', pos')
      | otherwise = (boxes, pos)
      where
        (pos'@(y, x)) = move pos e
        boxPos = (if pos' `S.member` boxes then pos' else (y, x - 1))
        boxes' = processMoveBox e boxPos

    processMoveBox :: Dir -> (Int, Int) -> Set (Int, Int)
    processMoveBox LEFT pos = processMoveBoxLeft pos
    processMoveBox RIGHT pos = processMoveBoxRight pos
    processMoveBox e pos = processMoveBoxY boxes e pos

    processMoveBoxLeft :: (Int, Int) -> Set (Int, Int)
    processMoveBoxLeft pos'
      | newBoxLeft `S.member` walls = boxes
      | (move newBoxLeft LEFT) `S.notMember` boxes = S.insert newBoxLeft $ S.delete pos' boxes
      | (move newBoxLeft LEFT) `S.notMember` boxes' = S.insert newBoxLeft $ S.delete pos' boxes'
      | otherwise = boxes
      where
        boxes' = processMoveBoxLeft (move newBoxLeft LEFT)
        newBoxLeft = move pos' LEFT
        newBoxRight = second (+ 1) newBoxLeft

    processMoveBoxRight :: (Int, Int) -> Set (Int, Int)
    processMoveBoxRight pos'
      | newBoxRight `S.member` walls = boxes
      | newBoxLeft `S.notMember` boxes && newBoxRight `S.notMember` boxes = S.insert newBoxLeft $ S.delete pos' boxes
      | newBoxLeft `S.notMember` boxes' && newBoxRight `S.notMember` boxes' = S.insert newBoxLeft $ S.delete pos' boxes'
      | otherwise = boxes
      where
        boxes' = processMoveBoxRight (move newBoxLeft RIGHT)
        newBoxLeft = move pos' RIGHT
        newBoxRight = second (+ 1) newBoxLeft

    processMoveBoxY :: Set (Int, Int) -> Dir -> (Int, Int) -> Set (Int, Int)
    processMoveBoxY boxes e pos'
      | newBoxLeft `S.member` walls || newBoxRight `S.member` walls = boxes
      | newBoxLeft `S.notMember` boxes && newBoxRight `S.notMember` boxes && (move newBoxLeft LEFT) `S.notMember` boxes = S.insert newBoxLeft $ S.delete pos' boxes
      | newBoxLeft `S.notMember` boxes' && newBoxRight `S.notMember` boxes' && (move newBoxLeft LEFT) `S.notMember` boxes' = S.insert newBoxLeft $ S.delete pos' boxes'
      | otherwise = boxes
      where
        boxes' = foldl (\b p -> processMoveBoxY b e p) boxes $ filter (`S.member` boxes) [(move newBoxLeft LEFT), newBoxLeft, newBoxRight]
        newBoxLeft = move pos' e
        newBoxRight = second (+ 1) newBoxLeft

part2 :: Input -> Output
part2 input = gps $ S.toList boxes
  where
    (_, boxes, _, _) = processMoves2 input

showGrid :: Input -> [String]
showGrid (walls, boxes, _, pos) = [[if (y, x) `S.member` walls then '#' else if (y, x) `S.member` boxes then 'O' else if (y, x) == pos then '@' else '.' | x <- [0 .. 13]] | y <- [0 .. 6]]

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input1 = parseInput1 content
  let input2 = parseInput2 content

  -- print $ input
  print $ part1 input1
  print $ part2 input2
