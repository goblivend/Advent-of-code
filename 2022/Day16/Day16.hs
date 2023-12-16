module Main where

import Data.Char
import Data.List.Split
import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set (Set, singleton)
import Data.Set qualified as S
import Debug.Trace

data Valve = Valve { rate :: Int, tunnels :: [String]}
type Input = Map String Valve

parseInput :: String -> Input
parseInput = M.fromList . map parseLine . lines
    where
        parseLine line = (_name, Valve _rate _tunnels)
            where
                _name = take 2 $ drop (length "Valve ") line
                _rate = read . takeWhile isDigit $ dropWhile (not . isDigit) line
                _tunnels = splitOn ", " . dropWhile (not . isUpper) $ (splitOn ";" line) !! 1

getDistances :: Input -> Map (String, String) Int
getDistances valves = M.fromList $ getDistance <$> [(src, dst) | src <- M.keys valves, dst <- M.keys valves, src /= dst]
    where
        getShortest dst lst
            | dst `S.member` lst = 0
            | otherwise = 1 + getShortest dst (S.unions $ S.map (\c -> S.fromList . tunnels $ valves ! c) lst)
        getDistance (src, dst) = ((src, dst), getShortest dst $ S.singleton src)

part1 :: Input -> Int
part1 valves =
    getMaxRelease 30 ("AA", 0) 0 usefulValves
    where
        usefulValves = M.keys $ M.filter ((>0) . rate) valves
        distances = getDistances valves
        getMaxRelease timeLeft (c, t) acc closed
            | timeLeft <= 0 = acc
            | t > 0         = acc + getMaxRelease (timeLeft-1) (c, t-1) acc closed
            | closed == []  = acc + timeLeft * (acc + (rate $ valves ! c))
            | otherwise     = acc + maxRelease
            where
                possibleClosed = filter (\d -> distances ! (c, d) <= timeLeft) closed
                releases = map (\dst -> getMaxRelease (timeLeft - 1) (dst, distances ! (c, dst)) (acc + (rate $ valves ! c)) (delete dst closed)) possibleClosed
                maxRelease = maximum $ 0:releases

part2 :: Input -> Int
part2 valves = getMaxRelease 26 ("AA", 0) ("AA", 0) 0 usefulValves
    where
        usefulValves = M.keys $ M.filter ((> 0) . rate) valves
        distances = getDistances valves
        getMaxRelease tl (c1, t1) (c2, t2) acc closed
            | tl <= 0 = 0
            | closed == []      = acc * (tl+1) + (rate $ valves ! c1) * (tl-t1) + (rate $ valves ! c2) * (tl-t2)
            | t1 > 0 && t2 > 0  = acc *mint + getMaxRelease (tl-mint) (c1, t1-mint) (c2, t2-mint) acc closed
            | t1 > 0 && t2 == 0 = getMaxRelease tl (c2, t2) (c1, t1) acc closed
            | t2 > 0    = acc + (maximum $ 0:releases1)
            | otherwise = acc + (maximum $ 0:releases2)
                where
                    mint = min t1 t2
                    possibleClosed c' closed' = filter (\d -> distances ! (c', d) <= tl) closed'
                    releases1 = map (\dst -> getMaxRelease (tl-1) (dst, distances ! (c1, dst)) (c2, t2-1) (acc + (rate $ valves ! c1)) (delete dst closed)) $ possibleClosed c1 closed
                    possibilities = concat [[(v1, v2) | v1<-possibleClosed c1 closed, v1/=v2]|v2<-possibleClosed c2 closed]
                    releases2 = map (\(d1, d2) -> getMaxRelease (tl-1) (d1, distances ! (c1, d1)) (d2, distances ! (c2, d2)) (acc + (rate $ valves ! c1) + (rate $ valves ! c2)) (delete d2 . delete d1 $ closed)) possibilities
main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    putStrLn "Part1: 1651"
    print $ part1 inp
    putStrLn "Part2: 1707"
    print $ part2 inp
