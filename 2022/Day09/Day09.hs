module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

getPositions :: [(Char, Int)] -> Int -> Int -> Int -> Int -> [(Int, Int)]
getPositions [] hx hy tx ty = [(tx, ty)]
getPositions ((dir, dist):xs) headx heady tailx taily
    | dist == 0 = getPositions xs headx heady tailx taily
    | otherwise = (tx, ty) : getPositions ((dir, dist-1):xs) x y tx ty
    where
        x = headx + (if dir == 'R' then 1 else if dir == 'L' then -1 else 0)
        y = heady + (if dir == 'U' then 1 else if dir == 'D' then -1 else 0)
        cx = abs (headx - tailx)
        cy = abs (heady - taily)
        tx = tailx + (if cx+cy > 2 && cx >= 1 || cx+cy>=2 && cx >= 2 then signum (x - tailx) else 0)
        ty = taily + (if cx+cy > 2 && cy >= 1 || cx+cy>=2 && cy >= 2 then signum (y - taily) else 0)

main :: IO ()
main = do
    content <- readFile "shortinput.txt"
    let input = map (\line -> (line !! 0 !! 0, read (line !! 1) :: Int)) $ map words $ lines content
    print $ getPositions input 0 0 0 0
    print $ length $ nub $ getPositions input 0 0 0 0


-- Part 1: 6354
-- Part 2: 2651
