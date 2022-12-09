module Main where

import Data.Char
import Data.List

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

updateTails :: (Int, Int) -> [(Int, Int)] -> [[(Int, Int)]]
updateTails head tail
    | ntail == tail = [tail]
    | otherwise = ntail : updateTails head (ntail)
        where
            ntail = map (\(h, t) -> getNewTail h t) $ zip (head:tail) tail

getNknots :: [(Char, Int)] -> [(Int, Int)] -> [(Int, Int)]
getNknots [] rope = last rope :[]
getNknots ((dir, dist):xs) (head:tail)
    | dist == 0 = (map last ntail) ++ getNknots xs (head : last ntail)
    | otherwise = (map last ntail) ++ getNknots ((dir, dist-1):xs) (nhead : last ntail)
        where
            nhead = getNewHead head dir
            ntail = updateTails head tail


main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = map (\line -> (line !! 0 !! 0, read (line !! 1) :: Int)) $ map words $ lines content
    print $ length $ nub $ getNknots input (replicate 2 (0, 0))
    print $ length $ nub $ getNknots input (replicate 10 (0, 0))
    
    content <- readFile "input2.txt"
    let input = map (\line -> (line !! 0 !! 0, read (line !! 1) :: Int)) $ map words $ lines content
    print $ length $ nub $ getNknots input (replicate 2 (0, 0))
    print $ length $ nub $ getNknots input (replicate 10 (0, 0))
