module Main where

import Data.List (sortOn, group, sortOn, sort, isSuffixOf)
import Data.List.Split ( splitOn )
import Data.Ord ()
import Data.Set (Set, fromList, member, insert, empty, size)
import Debug.Trace(trace)

data Pipe = Ground | Start | Pipe { connections ::[(Int, Int)]} deriving (Read, Eq, Ord)
type Input = [[Pipe]]

instance Show Pipe where
    show Start                      = "S"
    show Ground                     = "."
    show (Pipe [( 0, -1), ( 0, 1)]) = "|"
    show (Pipe [(-1,  0), ( 1, 0)]) = "-"
    show (Pipe [( 0, -1), (-1, 0)]) = "J"
    show (Pipe [( 0, -1), ( 1, 0)]) = "L"
    show (Pipe [(-1,  0), ( 0, 1)]) = "7"
    show (Pipe [( 1,  0), ( 0, 1)]) = "F"
    show (Pipe c)                   = show c

inputToString :: Input -> String
inputToString = unlines . map (concatMap show)

parseInput :: String -> Input
parseInput = map (map readPipe) . lines
    where
        readPipe 'S' = Start
        readPipe '.' = Ground
        readPipe '|' = Pipe [( 0, -1), ( 0, 1)]
        readPipe '-' = Pipe [(-1,  0), ( 1, 0)]
        readPipe 'J' = Pipe [( 0, -1), (-1, 0)]
        readPipe 'L' = Pipe [( 0, -1), ( 1, 0)]
        readPipe '7' = Pipe [(-1,  0), ( 0, 1)]
        readPipe 'F' = Pipe [( 1,  0), ( 0, 1)]

add ::(Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1+x2, y1+y2)

findStart :: Input -> (Int, Int)
findStart inp = (x, y)
    where
        y = length $ takeWhile (not . elem Start) inp
        x = length $ takeWhile (/= Start) (inp !! y)

outOfRange :: (Int, Int) -> (Int, Int) -> Bool
outOfRange (w, h) (x, y) = y < 0 || h <= y || x < 0 || x >= w

replaceStart :: Input -> Input
replaceStart inp = map (map fixMe) inp
    where
        (width, height) = (length $ inp !! 0, length inp)
        sxy = findStart inp
        newStart = Pipe . concat $ map matching [(0, -1), (-1, 0), (1, 0), (0, 1)]
            where
                matching dxy
                    | outOfRange (width, height) (add sxy dxy) = []
                    | otherwise = filter (any (== (-fst dxy, -snd dxy)) . connections . getPipe . add sxy) nonGround
                        where
                            nonGround = filter ((/= Ground) . getPipe . add sxy) [dxy]
                            getPipe (x, y) = inp !! y !! x
        fixMe Start = newStart
        fixMe e = e

pipePath :: Input -> Set (Int, Int)
pipePath inp = pathFind sxy empty
    where
        sxy = findStart inp
        newInp = replaceStart inp
        pathFind xy seen
            | length nexts /= 0 = pathFind (head nexts) (xy `insert` seen)
            | otherwise = xy `insert` seen
                where
                    nexts = filter (not . (`member` seen)) . map (add xy) . connections $ newInp !! snd xy !! fst xy

part1 :: Input -> Int
part1 = (`div` 2) . size . pipePath

cleanupInput :: Input -> Input
cleanupInput inp = map (map (\(x, y) -> if (x,y) `member` path then inp2 !! y !! x else Ground)) [[(x, y) | x<-[0..width-1]] | y<-[0..height-1]]

    where
        path = pipePath inp
        (width, height) = (length $ inp !! 0, length inp)
        inp2 = replaceStart inp



part2 :: Input -> Int
part2 inp = length $ filter hasWallAbove grounds
    where
        (width, height) = (length $ inp !! 0, length inp)
        path = pipePath inp
        newInp = replaceStart inp
        topipe (x, y) = newInp !! y !! x

        grounds = filter (not . (`member` path)) [(x, y) | x<-[1..width-2], y<-[1..height-2]]
        walls dxy xy
            | outOfRange (width, height) xy' = []
            | xy' `member` path              = xy' : walls dxy xy'
            | otherwise                      = walls dxy xy'
                where
                    xy' = add xy dxy

        nbWallsHori [] = 0
        -- p is always ew in this case
        nbWallsHori (p:[]) = 1
        nbWallsHori (p1:p2:l)
            | p1 == ew                         = 1 + nbWallsHori (p2:l)
            | any (== (fromList [p1, p2])) ews = 1 + nbWallsHori l
            | otherwise                        = nbWallsHori l

        ns = Pipe [( 0, -1), ( 0, 1)]
        ew = Pipe [(-1,  0), ( 1, 0)]
        nw = Pipe [( 0, -1), (-1, 0)]
        ne = Pipe [( 0, -1), ( 1, 0)]
        sw = Pipe [(-1,  0), ( 0, 1)]
        se = Pipe [( 1,  0), ( 0, 1)]

        ews = [fromList [ne, sw], fromList [se, nw]]

        hasWallAbove = (/= 0) . (`mod` 2) . nbWallsHori . filter (/= ns) . map topipe . walls (0, -1)



main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content

    print $ part1 inp
    print $ part2 inp


    -- putStr . inputToString  $ cleanupInput inp
