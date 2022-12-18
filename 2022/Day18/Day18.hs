module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

-- Part 1

checkPresence :: [(Int, Int, Int)] -> (Int, Int, Int) -> Bool
checkPresence cubes cube = elem cube cubes

checkSides :: (Int, Int, Int) -> [(Int, Int, Int)] -> Int
checkSides (x, y, z) cubes = length . filter (not . checkPresence cubes) $ co
    where
        deltas = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]
        co = map (\(dx, dy, dz) -> (x+dx, y+dy, z+dz)) deltas

countNbOpen :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Int
countNbOpen _ [] = 0
countNbOpen cubes (check:toCheck) = checkSides check cubes + countNbOpen cubes toCheck

-- Part 2

getX :: (Int, Int, Int) -> Int
getX (x, _, _) = x

getY :: (Int, Int, Int) -> Int
getY (_, y, _) = y

getZ :: (Int, Int, Int) -> Int
getZ (_, _, z) = z


getMat :: [(Int, Int, Int)] -> [[[Bool]]]
getMat pts = replicate rangeX $ replicate rangeY $ replicate rangeZ False
    where
        rangeX = maximum (map getX pts) + 1
        rangeY = maximum (map getY pts) + 1
        rangeZ = maximum (map getZ pts) + 1

replaceAt :: Int -> a -> [a] -> [a]
replaceAt x val l = take x l ++ [val] ++ drop (x+1) l

replaceXY :: Int -> Int -> a -> [[a]] -> [[a]]
replaceXY x y val l = replaceAt x (replaceAt y val (l !! x)) l

replaceXYZ :: Int -> Int -> Int -> a -> [[[a]]] -> [[[a]]]
replaceXYZ x y z val m = replaceAt x (replaceXY y z val (m !! x)) m

fillMat :: [(Int, Int, Int)] -> [[[Bool]]] -> [[[Bool]]]
fillMat [] mat = mat
fillMat ((x, y, z):pts) mat = fillMat pts (replaceXYZ x y z True mat)

isOutBound :: [a] -> Int -> Bool
isOutBound l x = x < 0 || length l <= x

isOutBoundXYZ :: [[[a]]] -> (Int, Int, Int) -> Bool
isOutBoundXYZ mat (x, y, z) = isOutBound mat x || isOutBound (mat !! x) y || isOutBound (mat !! x !! y) z

isOutSide :: [[[Bool]]] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Bool
isOutSide _ _ [] = False
isOutSide mat seen pts
    | any (isOutBoundXYZ mat) pts = True
    | otherwise = if outBound then True else isOutSide mat (pts ++ seen) co
    where
        deltas = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]
        newPts = nub $ concat $ map (\(x, y, z) -> map (\(dx, dy, dz) -> (x+dx, y+dy, z+dz)) deltas) pts
        newPtsNotSeen = filter (\p -> not $ elem p seen) newPts
        outBound = any (isOutBoundXYZ mat) newPtsNotSeen
        co = filter (\(x, y, z) -> not (mat !! x !! y !! z)) $ newPtsNotSeen

checkOutSide :: (Int, Int, Int) -> [[[Bool]]] -> Int
checkOutSide (x, y, z) mat = nbDirectoutBound + nbIndirectOutBound
    where
        deltas = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]
        coords = map (\(dx, dy, dz) -> (x+dx, y+dy, z+dz)) deltas
        outBound = filter (isOutBoundXYZ mat) coords
        nbDirectoutBound = length $ outBound
        co = filter (\(x, y, z) -> not (mat !! x !! y !! z)) $ coords \\ outBound
        nbIndirectOutBound = length $ filter (\p -> isOutSide mat [] [p]) co

countNbOutside :: [[[Bool]]] -> [(Int, Int, Int)] -> Int
countNbOutside _ [] = 0
countNbOutside mat (check:toCheck) = trace ( "Left " ++ show (length toCheck)) (checkOutSide check mat + countNbOutside mat toCheck)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let points = map (\s -> read ("(" ++ s ++ ")") :: (Int, Int, Int)) $ lines content
    print $ countNbOpen points points
    let mat = fillMat points $ getMat points

    print $ countNbOutside mat points
