module Main where

import Data.Char
import Data.List
import Data.List.Unique
import Data.List.Split
import Data.Maybe ( fromJust, isJust )
import Debug.Trace(trace)
import Data.Set (Set, member, fromList, toList)
import Data.Map (Map)
import Data.Tuple.Extra
import qualified Data.Set as S (insert, empty, union, size, member, map)
import qualified Data.Map as M (insert, empty, lookup, size)
import Data.Complex

data Input = Input {area :: (Int, Int), grid :: [String]} deriving (Show)

type Direction = (Int, Int)
type Point = (Int, Int)

north = (0, -1)
south = (0, 1)
east  = (1, 0)
west  = (-1, 0)

add :: Point -> Direction -> Point
add (x, y) (dx, dy) = (x+dx, y+dy)

parseInput :: String -> Input
parseInput inp = Input area (lns)
    where
        lns = lines inp
        area = (length (lns !! 0), length lns)

lightbeam :: Point -> Direction -> Set (Point, Direction) -> Input -> Set (Point, Direction)
lightbeam p dir seen inp@(Input (w, h) grid)
    | fst p < 0 || w <= fst p || snd p < 0 || h <= snd p = seen
    | (p, dir) `S.member` seen = seen
    | c == '.'  = light dir
    | c == '-'  && (dir == north || dir == south) = lightBoth west east
    | c == '-' =  light dir -- Already in the way of the length
    | c == '|'  && (dir == east || dir == west) = lightBoth north south
    | c == '|'  = light dir
    | c == '\\' && dir == north = light west
    | c == '\\' && dir == east  = light south
    | c == '\\' && dir == south = light east
    | c == '\\' && dir == west  = light north
    | c == '/'  && dir == north = light east
    | c == '/'  && dir == south = light west
    | c == '/'  && dir == east  = light north
    | c == '/'  && dir == west  = light south
    where
        c = grid !! (snd p) !! (fst p)
        lightBoth e f = lightSet f (light e)
        light e       = lightSet e ((p, dir) `S.insert` seen)
        lightSet e s  = lightbeam (add p e) e s inp

putLight :: Point -> Direction -> Input -> Set Point
putLight p d = S.map fst . lightbeam p d S.empty

part1 :: Input -> Int
part1 = S.size . putLight (0, 0) east

part2 :: Input -> Int
part2 inp@(Input (w,h) _) = maximum . map (S.size) $ map (\(p, d) -> putLight p d inp) ins
    where
        ins = [((x, 0), south) | x<-[0..w-1]] ++ [((x, h-1), north) | x<-[0..w-1]]  ++ [(((0, y), east)) | y<-[0..h-1]] ++ [(((y, w-1), west)) | y<-[0..h-1]]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    -- print inp
    print $ part1 inp
    print $ part2 inp
