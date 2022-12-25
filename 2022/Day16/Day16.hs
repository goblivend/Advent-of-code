module Main where

import Data.Char
import Data.List.Split
import Data.List hiding (insert)
import Data.Map (Map, (!), fromList, insert, empty, member, size, filter, keys, adjust)

data Valve = Valve { rate :: Int, tunnels :: [String]}

parseLine :: String -> (String, Valve)
parseLine line = (_name, Valve _rate _tunnels)
    where
        _name = take 2 $ drop (length "Valve ") line
        _rate = read $ takeWhile isDigit $ dropWhile (not . isDigit) line
        _tunnels = splitOn ", " $ dropWhile (not . isUpper) $ (splitOn ";" line) !! 1
        
getShortest :: Map String Valve -> String -> [String] -> Int
getShortest valves dst lst 
            | any (==dst) lst = 0
            | otherwise = 1 + getShortest valves dst (concat $ map (\c -> tunnels $ valves ! c) lst)

getDistance :: Map String Valve -> (String, String) -> ((String, String), Int)
getDistance valves (src, dst) = ((src, dst), getShortest valves dst [src])


getMaxRelease :: Map String Valve -> Map (String, String) Int -> Int -> String -> [String] -> Int
getMaxRelease valves distances timeLeft curr [] = 0 
getMaxRelease valves distances timeLeft curr closed 
    | timeLeft <= 0 = 0
    | otherwise = maxRelease
    where
        newTimeLeft dst = timeLeft - 1 - distances ! (curr, dst)
        possibleClosed = Data.List.filter (\d -> distances ! (curr, d) < timeLeft) closed
        releases = map (\dst -> (rate $ valves ! dst) * (newTimeLeft dst) + getMaxRelease valves distances (newTimeLeft dst) dst (delete dst closed)) $ possibleClosed
        maxRelease = if releases == [] then 0 else maximum releases


main :: IO ()
main = do
    content <- readFile "input.txt"
    let valves = fromList $ map parseLine $  lines content
    let usefulValves = (keys $ Data.Map.filter ((>0) . rate) valves)
    let distances = fromList $ getDistance valves <$> [(src, dst) | src <- ("AA":usefulValves), dst <- usefulValves, src /= dst]
    -- print $ keys valves
    -- print distances
    print $ getMaxRelease valves distances 30 "AA" usefulValves
