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
allOpened valves = all (opened) valves



getMostPressure :: Int -> [Valve] -> Int -> String -> Int
getMostPressure timeLeft valves acc curr
    | timeLeft == 0 = acc
    | timeLeft <= 0 = 0
    | allOpened valves = trace ("Stopping at : " ++ show timeLeft ++ " With " ++ show acc) (acc*timeLeft)
    | otherwise = {-trace (show timeLeft ++ " At " ++ curr ++ " with max " ++ show maxPressure) -}(acc + maxPressure)
        where
            v = getValve valves curr
            idx = getIndex valves curr
            maxPressureNotOpening =  maximum . map (getMostPressure (timeLeft - 1) valves acc) . tunnels $ v
            newAcc = if opened v then acc else acc + rate v
            maxPressureOpening = if opened v && rate v /= 0 then 0 else maximum . map (getMostPressure (timeLeft - 2) (replaceAt idx (v {opened = True}) valves) (newAcc)) . tunnels $ v
            maxPressure = max maxPressureOpening $ max newAcc maxPressureNotOpening




main :: IO ()
main = do
    content <- readFile "shortinput.txt"
    let valves = map parseLine $  lines content
    -- putStrLn $ unlines $ map show valves

    print $ getMostPressure 30 valves 0 "AA"
