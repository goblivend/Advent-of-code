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

getNknots :: [(Char, Int)] -> [(Int, Int)] -> [(Int, Int)]
getNknots [] rope = last rope :[]
getNknots ((dir, dist):xs) (head:rest)
    | dist == 0 = last ntail : getNknots xs (head : ntail)
    | otherwise = last ntail : getNknots ((dir, dist-1):xs) (nhead : ntail)
        where
            nhead = getNewHead head dir
            ntail = map (\(h, t) -> getNewTail h t) $ zip (head:rest) rest

createNlist :: Int -> [(Int, Int)]
createNlist n = map (\_ -> (0, 0)) [1..n]


main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = map (\line -> (line !! 0 !! 0, read (line !! 1) :: Int)) $ map words $ lines content
    print $ length $ nub $ getNknots input (createNlist 2)
    print $ length $ nub $ getNknots input (createNlist 10)
