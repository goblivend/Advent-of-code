module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

getNewTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
getNewTail (headx, heady) (tailx, taily) = (tx, ty)
    where
        cx = abs (headx - tailx)
        cy = abs (heady - taily)
        tx = tailx + (if cx+cy > 2 && cx >= 1 || cx+cy>=2 && cx >= 2 then signum (headx - tailx) else 0)
        ty = taily + (if cx+cy > 2 && cy >= 1 || cx+cy>=2 && cy >= 2 then signum (heady - taily) else 0)

getNewHead :: (Int, Int) -> Char -> (Int, Int)
getNewHead (headx, heady) dir = (x, y)
    where
        x = headx + (if dir == 'R' then 1 else if dir == 'L' then -1 else 0)
        y = heady + (if dir == 'U' then 1 else if dir == 'D' then -1 else 0)


get2knots :: [(Char, Int)] -> Int -> Int -> Int -> Int -> [(Int, Int)]
get2knots [] hx hy tx ty = [(tx, ty)]
get2knots ((dir, dist):xs) headx heady tailx taily
    | dist == 0 = (tx, ty) : get2knots xs headx heady tx ty
    | otherwise = (tx, ty) : get2knots ((dir, dist-1):xs) x y tx ty
    where
        (x, y) = getNewHead (headx, heady) dir
        (tx, ty) = getNewTail (headx, heady) (tailx, taily)

getNknots :: [(Char, Int)] -> [(Int, Int)] -> [(Int, Int)]
getNknots [] rope = last rope :[]
getNknots ((dir, dist):xs) (head:rest) 
    | dist ==



main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = map (\line -> (line !! 0 !! 0, read (line !! 1) :: Int)) $ map words $ lines content
    --print $ get2knots input 0 0 0 0
    --print $ get2knots [('U', 0)] 2 0 0 0
    print $ length $ nub $ get2knots input 0 0 0 0


-- Part 1: 6354
-- Part 2: 2651
