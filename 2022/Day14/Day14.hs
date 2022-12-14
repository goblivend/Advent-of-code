module Main where

import Data.Char
import Debug.Trace

data Land = Void | Rock | Sand deriving (Eq)

instance Show Land where
    show Void = " "
    show Rock = "#"
    show Sand = "o"

data Map = Map { lpad :: Int, slice :: [[Land]] } deriving (Eq)

instance Show Map where
    show (Map lp sl) = "Lpad = " ++ (show lp) ++ "\n" ++ (concat $ map (\l -> "\n" ++ concat (map show l)) sl)

parseLine :: String -> [(Int, Int)]
parseLine [] = []
parseLine (' ':'-':'>':' ':l) = parseLine l
parseLine l = (read $ takeWhile isDigit l, read (takeWhile isDigit . drop 1 $ dropWhile isDigit l) :: Int):parseLine (dropWhile (/= ' ') l)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x l = take n l ++ [x] ++ drop (n+1) l

generateRock :: Map -> (Int, Int) -> Map
generateRock (Map lpad slice) (x, y) = Map lpad $ replaceAt y (replaceAt (x-lpad) Rock (slice !! y)) slice

generateRocks :: Map -> [(Int, Int)] -> Map
generateRocks mp [] = mp
generateRocks mp (_:[]) = mp
generateRocks mp (e1:e2:l) = generateRocks newMp (e2:l)
    where
        newMp = (foldl generateRock mp [(x, y) | x <- xrange, y <- yrange])
        xrange = [minimum [fst e1, fst e2] .. maximum [fst e1, fst e2]]
        yrange = [minimum [snd e1, snd e2] .. maximum [snd e1, snd e2]]

pourSand :: Map -> (Int, Int) -> Map
pourSand (Map lpad slice) (x, y)
    | (y+1) == length slice = Map lpad slice
    | slice !! (y+1) !! (x-lpad)   == Void = pourSand (Map lpad slice) (x, y+1)
    | slice !! (y+1) !! (x-lpad-1) == Void = pourSand (Map lpad slice) (x-1, y+1)
    | slice !! (y+1) !! (x-lpad+1) == Void = pourSand (Map lpad slice) (x+1, y+1)
    | otherwise = newMp
        where newMp = Map lpad $ replaceAt y (replaceAt (x-lpad) Sand (slice !! y)) slice

printPour :: Map -> Map
printPour mp
    | mp /= newMp = printPour newMp
    | otherwise = mp
        where newMp = pourSand mp (500, 0)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rocksPositions = map parseLine $ lines content
    let maxY = (+) 2 $ maximum $ map snd $ concat rocksPositions
    let maxX = maximum $ map fst $ concat rocksPositions
    let minX = minimum $ map fst $ concat rocksPositions

    let lp = minimum [500-maxY, minX - 1]
    let range = maximum [maxX-lp + 1, 500+maxY-lp + 1]

    let mp = Map lp $ replicate (maxY+1) $ replicate range Void
    let newMp = foldl generateRocks mp rocksPositions
    --print (length ((slice newMp) !! 0), length . slice $ newMp)

    print $ length . filter (==Sand) . concat . slice $ printPour newMp

    let filledmp = generateRocks newMp [(lp, maxY), (lp + length (slice newMp !! 0)-1, maxY)]
    print $ length . filter (==Sand) . concat . slice $ printPour filledmp

    -- TO GET PRETTY PRINT
    -- (press <enter> to get next recursion : keep pressed to get timelapse)
    --mypour filledmp

    where
        mypour mp = do
            let newMp = pourSand mp (500, 0)
            if newMp == mp
                then return () else do
                    print mp
                    l <- getLine
                    mypour newMp
