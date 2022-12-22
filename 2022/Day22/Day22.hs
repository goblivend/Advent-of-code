module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

data Instruction = Number { nb :: Int } | Direction { direction :: Int} deriving (Eq, Ord, Read, Show)

rots :: [(Int, Int)]
rots = [(1, 0), (0, 1), (-1, 0), (0,  -1)]


isNb :: Instruction -> Bool
isNb (Number _) = True
isNb _ = False

parseInstructions :: String -> [Instruction]
parseInstructions [] = []
parseInstructions line
    | isDigit $ head line = Number (read (takeWhile (isDigit) line) :: Int) : parseInstructions (dropWhile (isDigit) line)
    | head line == 'R' = Direction 1 :parseInstructions (tail line)
    | otherwise = Direction (-1) : parseInstructions (tail line)

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
getWrap board (x, y) (1,  0)
    | y < 100 = (50, y)
    | otherwise = (0, y)
getWrap board (x, y) (-1, 0)
    | y < 50 = (149, y)
    | y < 150 = (99, y)
    | otherwise = (49, y)
getWrap board (x, y) (0,  1)
    | x < 50 = (x, 100)
    | otherwise = (x, 0)
getWrap board (x, y) (0, -1)
    | x < 50 = (x, 199)
    | x < 100 = (x, 149)
    | otherwise = (x, 49)

getNext :: [[Tile]] -> (Int, Int) -> Int -> (Int, Int)
getNext board pos dir = newPos
    where
        addPos = addTuple pos (rots !! dir)
        nextPos = if isOutBoard board addPos then getWrap board addPos (rots !! dir) else addPos
        newPos = if accessTuple board nextPos == Wall then pos else nextPos


walk :: [[Tile]] -> [Instruction] -> (Int, Int) -> Int -> ((Int, Int), Int)
walk _ [] pos dir = (pos, dir)
walk board (inst:instructions) pos dir
    | not $ isNb inst = walk board instructions pos ((dir + direction inst) `mod` 4)
    | nb inst == 0 = walk board instructions pos dir
    | otherwise = walk board (if pos == newPos then instructions else (Number (nb inst - 1) :instructions)) newPos dir
        where
            newPos = getNext board pos dir

calculateRes :: (Int, Int) -> Int -> Int
calculateRes (x, y) dir = trace (show ((x, y), dir)) (y*1000 + x*4 + dir)

part1 :: [[Tile]] -> [Instruction] -> Int
part1 board instructions = res
    where
        ((x, y), dir) = walk board instructions (getWrap board (0, 0) (1, 0)) 0
        res = calculateRes (x+1, y+1) dir

getComplement :: Int -> Int
getComplement x = 49- x `mod` 50

getWrap2 :: [[Tile]] -> (Int, Int) -> (Int, Int) -> ((Int, Int), Int)
getWrap2 board (x, y) (1,  0)
    | y < 50 = ((99, 100 + getComplement y), 2)
    | y < 100 = ((100 + (y `mod` 50), 49), 3)
    | y < 150 = ((149, getComplement y), 2)
    | otherwise = ((50 + (y `mod` 50), 149), 3)
getWrap2 board (x, y) (-1, 0)
    | y < 50 = ((0, 100 + getComplement y), 0)
    | y < 100 = (((y `mod` 50), 100), 1)
    | y < 150 = ((50, getComplement y), 0)
    | otherwise = ((50 + (y `mod` 50), 0), 1)
getWrap2 board (x, y) (0,  1)
    | x < 50 = ((100 + (x `mod` 50), 0), 1)
    | x < 100 = ((49, 150 + (x `mod` 50)), 2)
    | otherwise = ((99, 50 + (x `mod` 50)), 2)
getWrap2 board (x, y) (0, -1)
    | x < 50 = ((50, 50 + (x `mod` 50)), 0)
    | x < 100 = ((0, 150 + (x `mod` 50)), 0)
    | otherwise = (((x `mod` 50), 199), 3)

 -- (5, 100) (0, -1) == (50, 55)

getNext2 :: [[Tile]] -> (Int, Int) -> Int -> ((Int, Int), Int)
getNext2 board pos dir = (newPos, newDir)
    where
        addPos = addTuple pos (rots !! dir)
        (nextPos, nextDir) = if isOutBoard board addPos then getWrap2 board pos (rots !! dir) else (addPos, dir)
        newPos = if accessTuple board nextPos == Wall then pos else nextPos
        newDir = if accessTuple board nextPos == Wall then dir else nextDir


walk2 :: [[Tile]] -> [Instruction] -> (Int, Int) -> Int -> ((Int, Int), Int)
walk2 _ [] pos dir = (pos, dir)
walk2 board (inst:instructions) pos dir
    | not $ isNb inst = walk2 board instructions pos ((dir + direction inst) `mod` 4)
    | nb inst == 0 = walk2 board instructions pos dir
    | otherwise = walk2 board (if pos == newPos then instructions else (Number (nb inst - 1) :instructions)) newPos newDir
        where
            (newPos, newDir) = getNext2 board pos dir

part2 :: [[Tile]] -> [Instruction] -> Int
part2 board instructions = res
    where
        ((x, y), dir) = walk2 board instructions (50, 0) 0
        res = calculateRes (x+1, y+1) dir

-- testWrap2 :: [[Tile]] -> Bool
-- testWrap2 board =   (trace ("b1L : " ++ show b1L) (b1L)) &&
--                     (trace ("b1U : " ++ show b1U) (b1U)) &&
--                     (trace ("b2U : " ++ show b2U) (b2U)) &&
--                     (trace ("b2R : " ++ show b2R) (b2R)) &&
--                     (trace ("b2D : " ++ show b2D) (b2D)) &&
--                     (trace ("b3R : " ++ show b3R) (b3R)) &&
--                     (trace ("b5R : " ++ show b5R) (b5R)) &&
--                     (trace ("b5D : " ++ show b5D) (b5D)) &&
--                     (trace ("b6R : " ++ show b6R) (b6R)) &&
--                     (trace ("b6D : " ++ show b6D) (b6D)) &&
--                     (trace ("b6L : " ++ show b6L) (b6L)) &&
--                     (trace ("b4L : " ++ show b4L) (b4L)) &&
--                     (trace ("b4U : " ++ show b4U) (b4U)) &&
--                     (trace ("b3L : " ++ show b3L) (b3L))
--     where
--         b1L = getWrap2 board ( 50,   5) (-1, 0) == ((  0, 144), 0)
--         b1U = getWrap2 board ( 55,   0) (0, -1) == ((  0, 155), 0)
--         b2U = getWrap2 board (105,   0) (0, -1) == ((  5, 199), 3)
--         b2R = getWrap2 board (149,   5) (1,  0) == (( 99, 144), 2)
--         b2D = getWrap2 board (105,  49) (0,  1) == (( 99,  55), 2)
--         b3R = getWrap2 board ( 99,  55) (1,  0) == ((105,  49), 3)
--         b5R = getWrap2 board ( 99, 144) (1,  0) == ((149,   5), 2)
--         b5D = getWrap2 board ( 55, 149) (0,  1) == (( 49, 155), 2)
--         b6R = getWrap2 board ( 49, 155) (1,  0) == (( 55, 149), 3)
--         b6D = getWrap2 board (  5, 199) (0,  1) == ((105,   0), 1)
--         b6L = getWrap2 board (  0, 155) (-1, 0) == (( 55,   0), 1)
--         b4L = getWrap2 board (  0, 105) (-1, 0) == (( 50,  44), 0)
--         b4U = getWrap2 board (  5, 100) (0, -1) == (( 50,  55), 0)
--         b3L = getWrap2 board ( 50,  55) (-1, 0) == ((  5, 100), 1)



main :: IO ()
main = do
    content <- readFile "input.txt"
    let inst = parseInstructions $ last $ lines content
    let board = map (parseMap <$>) $ init $ init $ lines content

    print $ part1 board inst
    print $ part2 board inst
    -- let b =  [[Void | x <- [1..150]] | y <- [1..200]]
    -- print $ testWrap2 b
