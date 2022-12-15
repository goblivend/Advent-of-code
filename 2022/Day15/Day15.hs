module Main where

import Data.List
import Data.List.Split

data Point = Point { x::Int, y::Int } deriving (Eq, Read, Show)

instance Ord Point where
    compare p1 p2 = compare (y p1) (y p2) `mappend` compare (x p1) (x p2)

distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

data Sensor = Sensor { pos :: Point, beacon :: Point, dist:: Int } deriving (Read, Show)

parseLine :: String -> Sensor
parseLine [] = Sensor (Point 0 0) (Point 0 0) 0
parseLine l = Sensor p1 p2 (distance p1 p2)
    where
        parts = splitOn ":" l
        getPart s = splitOn "," s
        getValue s = drop 1 $ dropWhile (/='=') s
        getV x y = read (getValue $ (getPart (parts !! x)) !! y) :: Int
        p1 = Point (getV 0 0)  (getV 0 1)
        p2 = Point (getV 1 0)  (getV 1 1)

getRangeAtHeight :: Int -> Sensor -> (Int, Int)
getRangeAtHeight h (Sensor p b d) = (min, max)
    where
        delta = abs (h - (y p))
        min = (x p) - (d - delta)
        max = (x p) + (d - delta)

getRangesAtHeight :: [Sensor] -> Int -> [(Int, Int)]
getRangesAtHeight [] _ = []
getRangesAtHeight (s:sensors) h = getRangeAtHeight h s : getRangesAtHeight sensors h

mergeRange :: (Int, Int) -> (Int, Int) -> (Int, Int)
mergeRange (min1, max1) (min2, max2) = (min1, max max1 max2)

cleanRanges :: [(Int, Int)] -> [(Int, Int)]
cleanRanges [] = []
cleanRanges (r:[]) = [r]
cleanRanges (r1:r2:ranges) = if (fst r2) <= (snd r1)+1 then cleanRanges ((mergeRange r1 r2):ranges) else r1 : cleanRanges ranges

cropRanges :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
cropRanges _ _ [] = []
cropRanges min max (r:ranges)
    | snd r < min || fst r > max = cropRanges min max ranges
    | fst r < min = cropRanges min max ((min, snd r):ranges)
    | snd r > max = cropRanges min max ((fst r, max):ranges)
    | otherwise                  = r : cropRanges min max ranges

getFree :: [(Int, Int)] -> [Int]
getFree [] = []
getFree (_:[]) = []
getFree (r1:r2:r) = [snd r1 + 1.. fst r2 - 1] ++ getFree (r2:r)


getFreeSpaces :: Int -> Int -> Int -> [Sensor] -> [(Int, Int)]
getFreeSpaces min max cur s
    | max <= cur = []
    | otherwise = myres ++ getFreeSpaces min max (cur + 1) s
    where
        ranges = cropRanges 0 4000001 . cleanRanges . sort $ getRangesAtHeight s cur
        free = getFree ranges
        myres = zip free (repeat cur)



main :: IO ()
main = do
    content <- readFile "input.txt"
    let sensors = map parseLine $  lines content

    print $ sum . map (\(x, y) -> y - x) . cleanRanges . sort $ getRangesAtHeight sensors 2000000

    --print $ getFreeSpaces 0 20 0 sensors
    let pt =  getFreeSpaces 0 4000001 0 sensors
    print $ head pt

    print $ head $ map (\(x, y) -> 4000000*x + y) pt
