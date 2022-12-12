module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

data Map = Map { curr :: (Int, Int), dir :: (Int, Int), hMap :: [[Int]] }

parseMap :: [String] -> Map
parseMap content = Map currP endP heightMap
    where
        currP = head $ filter (\p -> (fst p) /= (-1)) $ concat $ map (\y -> map (\x -> if (content !! y !! x) == 'S' then (x, y) else (-1, -1)) [0..(length (content !! y) - 1)]) [0..(length content - 1)]
        endP  = head $ filter (\p -> (fst p) /= (-1)) $ concat $ map (\y -> map (\x -> if (content !! y !! x) == 'E' then (x, y) else (-1, -1)) [0..(length (content !! y) - 1)]) [0..(length content - 1)]
        heightMap = map (map (\e -> (if e == 'S' then ord 'a' else if e == 'E' then ord 'z' else ord e) - ord 'a')) content

tupleAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

isInRange :: (Int, Int) -> Map -> Bool
isInRange (x, y) myMap = x >= 0 && y >= 0 && y < (length (hMap myMap)) && (x < length ((hMap myMap) !! y))

isPossibleRoute :: Map -> (Int, Int) -> Bool
isPossibleRoute myMap (x2, y2) = isInRange (x2, y2) myMap && ((hMap myMap) !! y2 !! x2) <= (1 + ((hMap myMap) !! y !! x))
    where
        (x, y) = curr myMap

updateRoute :: (Map, [(Int, Int)]) -> [(Map, [(Int, Int)])]
updateRoute (myMap, route) = routes
    where
        directions = filter (\(x, y) -> isPossibleRoute myMap (x, y) && (x, y) `notElem` route) $ map (tupleAdd (curr myMap)) [(0, 1), (1, 0), (0, -1), (-1, 0)]
        routes = map (\d -> (myMap {curr = d}, route ++ [d])) directions


findRoute :: [(Map, [(Int, Int)])] -> [(Int, Int)]
findRoute routes
    | winningRoutes /= [] = head winningRoutes
    | otherwise = findRoute newRoutes
        where
            winningRoutes = map snd $ filter (\r -> (dir (fst r)) == (curr (fst r))) routes -- sortBy (\a b -> compare (length a) (length b)) $
            allRoutes = concat $ map (\r -> updateRoute r) routes
            newRoutes = (map head $ (groupBy (\(m1, r1) (m2, r2) -> (curr m1) == (curr m2) ) $ sortBy (\(m1, r1) (m2, r2) -> compare (curr m1) (curr m2)) allRoutes))

isShortestPossibleRoute :: Map -> (Int, Int) -> Bool
isShortestPossibleRoute myMap (x2, y2) = isInRange (x2, y2) myMap && ((hMap myMap) !! y2 !! x2) <= (1 + ((hMap myMap) !! y !! x)) && ((hMap myMap) !! y2 !! x2) > 0
    where
        (x, y) = curr myMap

updateShortestRoute :: (Map, [(Int, Int)]) -> [(Map, [(Int, Int)])]
updateShortestRoute (myMap, route) = routes
    where
        directions = filter (\(x, y) -> isShortestPossibleRoute myMap (x, y) && (x, y) `notElem` route) $ map (tupleAdd (curr myMap)) [(0, 1), (1, 0), (0, -1), (-1, 0)]
        routes = map (\d -> (myMap {curr = d}, route ++ [d])) directions

findShortestRoute :: [(Map, [(Int, Int)])] -> [(Int, Int)]
findShortestRoute [] = []
findShortestRoute routes
    | winningRoutes /= [] = head winningRoutes
    | otherwise = findShortestRoute newRoutes
        where
            winningRoutes = map snd $ filter (\r -> (dir (fst r)) == (curr (fst r))) routes -- sortBy (\a b -> compare (length a) (length b)) $
            allRoutes = concat $ map (\r -> updateShortestRoute r) routes
            newRoutes = (map head $ (groupBy (\(m1, r1) (m2, r2) -> (curr m1) == (curr m2) ) $ sortBy (\(m1, r1) (m2, r2) -> compare (curr m1) (curr m2)) allRoutes))




main :: IO ()
main = do
    content <-  readFile "input.txt"
    let heightMap = parseMap $ lines content
    print $ length $ tail $ findRoute [(heightMap, [curr heightMap])]

    let maps = concat $ map (\y -> map (\x -> (heightMap {curr = (x, y)}, [(x, y)])) [0..(length (hMap heightMap) - 1)]) [0..(length (hMap heightMap) - 1)]
    let startingMaps = filter (\(m, r) -> (hMap m) !! (snd (curr m)) !! (fst (curr m)) == 0) maps
    print $ length $ tail $ findShortestRoute startingMaps
