module Main where

import Data.List
import Data.List.Unique
import System.Environment

type Input = ((Int, Int), [(Char, [(Int, Int)])])
type Output = Int

parseInput :: String -> Input
parseInput s = ((width, height), freqs)
    where
        ls = lines s
        height = length $ ls
        width = length . head $ ls

        freqs = map (\f -> (f, locations f)) frequencies
        frequencies = uniq . sort . filter (/= '.') $ concat ls
        locations c = [(x, y) | x<-[0..width-1], y<-[0..height-1], (ls !! y) !! x == c]

isOut :: (Int, Int) -> (Int, Int) -> Bool
isOut (width, height) (x, y) = x < 0 || y < 0 || x >= width || y >= height

diffVect :: (Int, Int) -> (Int, Int) -> (Int, Int)
diffVect (x, y) (x', y') = (x-x', y-y')

addVect :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVect (x, y) (x', y') = (x+x', y+y')

addVectn :: (Int, Int) -> Int -> (Int, Int) -> (Int, Int)
addVectn (x, y) n (x', y') = (x+n*x', y+n*y')

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

algo :: [Int] -> Bool -> Input -> [(Int, Int)]
algo range authorizeAntenna (dim, antennaGrid) = nub . concat $ map getAntinodes antennaGrid --
    where
        getAntinode :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
        getAntinode ant1 ant2 = concat [side ant1 $ diffVect ant1 ant2, side ant2 $ diffVect ant2 ant1]
            where
                side :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
                side ant dist = takeWhile (not . isOut dim) $ map (\n -> addVectn ant n dist) range

        getAntinodes (_, antennas) = filter ((||) authorizeAntenna . (`notElem` antennas)) . concat . map (uncurry getAntinode) $ pairs antennas

part1 :: Input -> Output
part1 = length . algo [1] False

part2 :: Input -> Output
part2 = length . algo [0..] True

insertAt :: Char -> [String] -> (Int, Int) -> [String]
insertAt c grid (x, y) = linesBefore ++ [charsBefore ++ [c] ++ charsAfter] ++ linesAfter
    where
        (linesBefore, currLine:linesAfter) = splitAt y grid
        (charsBefore, _:charsAfter) = splitAt x currLine

getGrid :: Input -> [(Int, Int)] -> [String]
getGrid ((w, h), freqs) antinodes = antennasGrid
    where
        blankGrid = [['.' | _<-[1..w]] | _<-[1..h]]
        antinodesGrid = foldl (insertAt '#') blankGrid antinodes
        antennasGrid = foldl (\g (c, as) -> foldl (insertAt c) g as) antinodesGrid freqs

getGrid1 :: Input -> [String]
getGrid1 input = getGrid input $ algo [1] False input

getGrid2 :: Input -> [String]
getGrid2 input = getGrid input $ algo [0..] True input

main :: IO ()
main = do
    args  <- getArgs
    content <- readFile (last args)
    let input = parseInput content

    print $ part1 input
    print $ part2 input

    -- putStrLn $ unlines $ getGrid1 input
    -- putStrLn $ unlines $ getGrid2 input
