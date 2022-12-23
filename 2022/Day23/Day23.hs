module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

data Tile = Elf | Void  deriving (Eq, Ord, Read, Show)

type Board = [[Tile]]
type Pos = (Int, Int)

directions :: [[Pos]]
directions = [[(-1, -1), (0, -1), (1, -1)], [(-1, 1), (0, 1), (1, 1)], [(-1, -1), (-1, 0), (-1, 1)], [(1, -1), (1, 0), (1, 1)]]

accessTuple :: [[a]] -> (Int, Int) -> a
accessTuple board (x, y) = board !! y !! x

replaceAt :: Int -> a -> [a] -> [a]
replaceAt x val l = take x l ++ [val] ++ drop (x+1) l

replaceXY :: Pos -> a -> [[a]] -> [[a]]
replaceXY (x, y) val l = replaceAt y (replaceAt x val (l !! y)) l

exchangeXY :: Pos -> Pos -> [[a]] -> [[a]]
exchangeXY pos1 pos2  l = replaceXY pos1 (accessTuple l pos2) $ replaceXY pos2 (accessTuple l pos1) l

elfList :: Board -> [Pos]
elfList board = concat $ map (\y -> [(x, y) | x <- [0..widthB - 1], accessTuple board (x, y) == Elf]) [0..heightB-1]
    where
        widthB = length (board !! 0)
        heightB = length board

positionsVoid :: Board -> [Pos] -> Bool
positionsVoid board p = all (\pos -> Void == accessTuple board pos) p

addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

addPoss :: Pos -> [Pos] -> [Pos]
addPoss pos p= map (addPos pos) p


proposeMove :: Board -> Int -> Pos -> Pos
proposeMove board dir pos
    | positionsVoid board (addPoss pos (nub . concat $ directions)) = pos
    | positionsVoid board (addPoss pos (directions !! ((dir + 0) `mod` 4))) = addPos pos (directions !! ((dir + 0) `mod` 4) !! 1)
    | positionsVoid board (addPoss pos (directions !! ((dir + 1) `mod` 4))) = addPos pos (directions !! ((dir + 1) `mod` 4) !! 1)
    | positionsVoid board (addPoss pos (directions !! ((dir + 2) `mod` 4))) = addPos pos (directions !! ((dir + 2) `mod` 4) !! 1)
    | positionsVoid board (addPoss pos (directions !! ((dir + 3) `mod` 4))) = addPos pos (directions !! ((dir + 3) `mod` 4) !! 1)
    | otherwise = pos

move :: Board -> Int -> Board
move board dir = newBoard
    where
        lst = elfList board
        poss = map (proposeMove board dir) lst
        movingElves = [if 1 == (length . filter (poss !! x==)) poss then poss !! x else lst !! x | x <- [0..length lst - 1]]
        newBoard = foldl (\b (old, new) -> exchangeXY new old b) board $ zip lst movingElves

getRes :: Board -> Int
getRes board = size - nbelves
    where
        elfl = elfList board
        nbelves = length $ elfl
        minX = minimum $ map (fst) elfl
        maxX = maximum $ map (fst) elfl
        width = maxX - minX + 1
        minY = minimum $ map (snd) elfl
        maxY = maximum $ map (snd) elfl
        height = maxY - minY + 1
        size = height * width


space :: Board -> Board
space board = spaceU $ spaceD $ spaceL $ spaceR board
    where
        spaceU b = if any (Elf ==) (head b) then ([Void | x <- [1..length (b !! 0)]] : b) else b
        spaceD b = if any (Elf ==) (last b) then (b ++ [[Void | x <- [1..length (b !! 0)]]]) else b
        spaceL b = if any (Elf ==) [head (b !! y) | y <- [0..length b - 1]] then (map (\l -> Void:l) b) else b
        spaceR b = if any (Elf ==) [last (b !! y) | y <- [0..length b - 1]] then (map (\l -> l ++ [Void]) b) else b

part1 :: Board -> Int -> Int -> Int
part1 board dir nb
    | nb == 0 = getRes board
    | otherwise = (part1 newB ((dir + 1) `mod` 4) (nb-1))
    where
        newB = move (space board) dir

prettyShow :: Board -> String
prettyShow b = unlines $ ((\e -> if e == Elf then '#' else '.') <$>) <$> b

debug :: Bool -> String -> a -> a
debug cond str call = (if cond then trace (str) else id) call


part2 :: Board -> Int -> Int
part2 board dir = if newB /= board then 1+part2 (debug (dir `mod` 2 == 0) (show dir) (newB)) (dir + 1) else 1
    where
        newB = move (space board) (dir `mod` 4)


main :: IO ()
main = do
    content <- readFile "input.txt"
    let board = ((\c -> if c == '#' then Elf else Void) <$>) <$> lines content

    let pprint b = putStrLn $ prettyShow b
    print $ part1 board 0 10
    print $ part2 board 0
