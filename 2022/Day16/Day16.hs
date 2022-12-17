module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

data Valve = Valve { name :: String, rate :: Int, tunnels :: [String], opened :: Bool } deriving (Eq, Ord, Show, Read)

parseLine :: String -> Valve
parseLine line = Valve _name _rate _tunnels False
    where
        _name = take 2 $ drop (length "Valve ") line
        _rate = read $ takeWhile isDigit $ dropWhile (not . isDigit) line
        _tunnels = splitOn ", " $ dropWhile (not . isUpper) $ (splitOn ";" line) !! 1

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x l = take n l ++ [x] ++ drop (n+1) l

getValve :: [Valve] -> String -> Valve
getValve valves _name = head . filter (\v -> name v == _name) $ valves

getIndex :: [Valve] -> String -> Int
getIndex (v:valves) _name
    | name v == _name = 0
    | otherwise = 1 + getIndex valves _name

allOpened :: [Valve] -> Bool
allOpened valves = all (\v -> opened v || rate v == 0) valves

-- The path is the valves I did not visited in a go
getMostPressure :: Int -> [Valve] -> Int -> [String] -> String -> Int
getMostPressure timeLeft valves acc path curr
    | timeLeft == 0 = acc
    | timeLeft <= 0 = 0
    | allOpened valves = acc*timeLeft
    | otherwise = acc + maxPressure
        where
            v = getValve valves curr
            idx = getIndex valves curr
            maxPressureNotOpening =  maximum . map (\n -> if elem n path then 0 else getMostPressure (timeLeft - 1) valves acc (curr:path) n) . tunnels $ v
            newAcc = if opened v then acc else acc + rate v
            -- maxPressureOpening = if opened v || rate v == 0 || timeLeft == 1 then newAcc else maximum . map (getMostPressure (timeLeft - 2) (replaceAt idx (v {opened = True}) valves) newAcc []) . tunnels $ v
            maxPressureOpening = if opened v || rate v == 0 then 0 else getMostPressure (timeLeft - 1) (replaceAt idx (v {opened = True}) valves) newAcc [] curr
            maxPressure = max maxPressureNotOpening $ maxPressureOpening




main :: IO ()
main = do
    content <- readFile "shortinput.txt"
    let valves = map parseLine $  lines content

    print $ getMostPressure 30 valves 0 [] "AA"
