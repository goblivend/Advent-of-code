module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

data Point = Point { x::Int, y::Int } deriving (Eq, Read)

instance Show Point where
    show (Point x y)= "(" ++ (show x) ++ "," ++ (show y) ++ ")"

distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

instance Ord Point where
    compare p1 p2 = compare (distance (Point 0 0) p1) (distance (Point 0 0) p2)

data Sensor = Sensor { pos :: Point, beacon :: Point } deriving (Eq, Read, Ord, Show)

parseLine :: String -> Sensor
parseLine [] = Sensor (Point 0 0) (Point 0 0)
parseLine l = read parsedLine :: Sensor
    where
        parts = splitOn ":" l
        getPart s = splitOn "," s
        getValue s = drop 1 $ dropWhile (/='=') s
        getV x y = getValue $ (getPart (parts !! x)) !! y
        parsedLine = "Sensor {pos = (Point {x = " ++ getV 0 0 ++ ", y = " ++ getV 0 1
                   ++ "}), beacon = (Point {x = " ++ getV 1 0 ++ ", y = " ++ getV 1 1 ++ "})}"



--getMinMax :: (Int -> Int) -> Sensor -> Int
--getMinMax op sensor = op (x (pos sensor)) $ distance (pos sensor) (beacon sensor)
-- Can't call it with : map (getMinMax (-)) sensors

getMin :: Sensor -> Int
getMin sensor = (-) (x (pos sensor)) $ distance (pos sensor) (beacon sensor)

getMax :: Sensor -> Int
getMax sensor = (+) (x (pos sensor)) $ distance (pos sensor) (beacon sensor)

isInRangeSensor :: Point -> Sensor -> Bool
isInRangeSensor point sensor = (distance point (pos sensor)) <= (distance (pos sensor) (beacon sensor))

isInRange :: Point -> [Sensor] -> Bool
isInRange point sensors = any (isInRangeSensor point) sensors && not (any (point==) $ map beacon sensors)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let sensors = map parseLine $  lines content
    --print sensors
    --let minX = minimum $ map getMin sensors
    --print minX
    --let maxX = maximum $ map getMax sensors
    --print $ maxX-  minX

    --print $ length $ filter (\x -> isInRange (Point x 2000000) sensors) [minX..maxX]
    let pts = head . filter (\p -> (not $ isInRange p sensors) && (not (any (p==) $ map beacon sensors))) $ [Point x y | x <- [0..4000000], y <- [0..4000000]]
    print pts
    print $ map (\p -> 4000000*(x p) + (y p)) [pts]
