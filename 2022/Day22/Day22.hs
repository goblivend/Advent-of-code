module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

data Instruction = Number { nb :: Int } | Direction { direction :: (Int, Int)} deriving (Eq, Ord, Read, Show)

turn :: Char -> (Int, Int) -> (Int, Int)
turn rot dir
    | rot == 'L' = turnR $ turnR $ turnR dir
    | otherwise = turnR dir
        where
            turnR (1,  0) = (0,  1)
            turnR (0,  1) = (-1, 0)
            turnR (-1, 0) = (0, -1)
            turnR (0, -1) = (1,  0)

isNb :: Instruction -> Bool
isNb (Number _) = True
isNb _ = False

parseInstructions :: (Int, Int) -> String -> [Instruction]
parseInstructions dir [] = []
parseInstructions dir line
    | isDigit $ head line = Number (read (takeWhile (isDigit) line) :: Int) : parseInstructions dir (dropWhile (isDigit) line)
    | otherwise = Direction newDir : parseInstructions newDir (tail line)
        where
            newDir = turn (head line) dir

data Tile = Void | Wall | Walk deriving (Eq, Ord, Read, Show)

parseMap :: Char -> Tile
parseMap '#' = Wall
parseMap ' ' = Void
parseMap '.' = Walk

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

isOutBoard :: [[Tile]]  -> (Int, Int) -> Bool
isOutBoard board (x, y) = y < 0 || length board <= y || x < 0 || length (board !! y) <= x || board !! y !! x == Void

accessTuple :: [[Tile]] -> (Int, Int) -> Tile
accessTuple board (x, y) = board !! y !! x

getWrap :: [[Tile]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
getWrap board pos dir
    | not (isOutBoard board pos) && accessTuple board pos /= Void = pos
    | otherwise = getWrap board (addTuple pos dir) dir

getPosForWrap :: [[Tile]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
getPosForWrap board (x, y) (1,  0) = (0, y)
getPosForWrap board (x, y) (-1, 0) = (length (board !! y) - 1, y)
getPosForWrap board (x, y) (0,  1) = (x, 0)
getPosForWrap board (x, y) (0, -1) = (x, length board - 1)

getNext :: [[Tile]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
getNext board pos dir = newPos
    where
        addPos = addTuple pos dir
        nextPos = if isOutBoard board addPos then getWrap board (getPosForWrap board addPos dir) dir else addPos
        newPos = if accessTuple board nextPos == Wall then pos else nextPos


walk :: [[Tile]] -> [Instruction] -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
walk _ [] pos dir = (pos, dir)
walk board (inst:instructions) pos dir
    | not $ isNb inst = walk board instructions pos (direction inst)
    | nb inst == 0 = walk board instructions pos dir
    | otherwise = walk board (if pos == newPos then instructions else (Number (nb inst - 1) :instructions)) newPos dir
        where
            newPos = getNext board pos dir

calculateRes :: (Int, Int) -> (Int, Int) -> Int
calculateRes (x, y) dir = y*1000 + x*4 + getDirnb dir
    where
        getDirnb (1,  0) = 0
        getDirnb (0,  1) = 1
        getDirnb (-1, 0) = 2
        getDirnb (0, -1) = 3

part1 :: [[Tile]] -> [Instruction] -> Int
part1 board instructions = res
    where
        ((x, y), dir) = walk board instructions (getWrap board (0, 0) (1, 0)) (1, 0)
        res = calculateRes (x+1, y+1) dir



getWrap2 :: [[Tile]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
getWrap2 board pos dir
    | not (isOutBoard board pos) && accessTuple board pos /= Void = pos
    | otherwise = getWrap2 board (addTuple pos dir) dir

getPosForWrap2 :: [[Tile]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
getPosForWrap2 board (x, y) (1,  0)
    | y < 100 = (50, y)
    | otherwise = (0, y)
getPosForWrap2 board (x, y) (-1, 0)
    | y < 50 = (149, y)
    | y < 150 = (99, y)
    | otherwise = (49, y)
getPosForWrap2 board (x, y) (0,  1)
    | x < 50 = (x, 100)
    | otherwise = (0, y)
getPosForWrap2 board (x, y) (0, -1)
    | x < 50 = (x, 199)
    | x < 100 = (x, 149)
    | otherwise = (x, )

getNext2 :: [[Tile]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
getNext2 board pos dir = newPos
    where
        addPos = addTuple pos dir
        nextPos = if isOutBoard board addPos then getWrap2 board (getPosForWrap2 board addPos dir) dir else addPos
        newPos = if accessTuple board nextPos == Wall then pos else nextPos


walk2 :: [[Tile]] -> [Instruction] -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
walk2 _ [] pos dir = (pos, dir)
walk2 board (inst:instructions) pos dir
    | not $ isNb inst = walk2 board instructions pos (direction inst)
    | nb inst == 0 = walk2 board instructions pos dir
    | otherwise = walk2 board (if pos == newPos then instructions else (Number (nb inst - 1) :instructions)) newPos dir
        where
            newPos = getNext2 board pos dir


part2 :: [[Tile]] -> [Instruction] -> Int
part2 board instructions = res
    where
        ((x, y), dir) = walk2 board instructions (getWrap board (0, 0) (1, 0)) (1, 0)
        res = calculateRes (x+1, y+1) dir



main :: IO ()
main = do
    content <- readFile "input.txt"
    let inst = parseInstructions (1, 0) $ last $ lines content
    let board = map (parseMap <$>) $ init $ init $ lines content

    -- print $ part1 board inst
    print $ part2 board inst
