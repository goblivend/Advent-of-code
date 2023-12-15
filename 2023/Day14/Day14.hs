module Main where

import Data.List
import Data.List.Unique
import Data.List.Split
import Data.Maybe ( fromJust, isJust )
import Debug.Trace(trace)
import Data.Set (Set, member, fromList, toList)
import Data.Map (Map)
import Data.Tuple.Extra
import qualified Data.Set as S (insert, empty)
import qualified Data.Map as M (insert, empty, lookup, size)
import Data.Ord
import Data.Function


data Direction = North | South | East | West deriving (Eq, Show)
data Input = Input {area::(Int, Int), cubes::Set (Int, Int), rocks::[(Int, Int)]} deriving (Eq, Show, Read)

parseInput :: String -> Input
parseInput s = Input (w,h) (fromList $ getC '#') (getC 'O')
    where
        ls = lines s
        h = length ls
        w = length (ls !! 0)
        getC c = [(x, y)| (y, l)<-(zip [1..] ls), (x, c')<-(zip [1..] l), c == c']

tilt :: Input -> Direction -> Input
tilt (Input area cubes rocks) dir = Input area cubes newRocks
    where
        highest :: Set (Int, Int) -> (Int, Int) -> (Int, Int)
        highest seen xy
            | limit dir xy area = xy
            | (next dir xy) `member` seen = xy
            | otherwise              = highest seen $ next dir xy

        moveRock :: (Set (Int, Int), [(Int, Int)]) -> (Int, Int) -> (Set (Int, Int), [(Int, Int)])
        moveRock (seen, rs) xy = (newRock `S.insert` seen, newRock:rs)
            where
                newRock = highest seen xy

        newRocks = snd $ foldl moveRock (cubes, []) $ sortBy (sortDir dir) rocks

next :: Direction -> (Int, Int) -> (Int, Int)
next North (x, y) = (x, y-1)
next South (x, y) = (x, y+1)
next East  (x, y) = (x+1, y)
next West  (x, y) = (x-1, y)

limit :: Direction -> (Int, Int) -> (Int, Int) -> Bool
limit North (_, y) _      = y <= 1
limit South (_, y) (_, h) = y >= h
limit East  (x, _) (w, _) = x >= w
limit West  (x, _) _      = x <= 1

sortDir :: Direction -> (Int, Int) -> (Int, Int) -> Ordering
sortDir North = compare `on` snd
sortDir South = flip (sortDir North)
sortDir East = compare
sortDir West = flip (sortDir East)

solve :: Input -> Int
solve (Input (w,h) cubes rocks) = sum . map ((-) (h+1)) $ map snd rocks

part1 :: Input -> Int
part1 inp = solve $ tilt inp North

move1Cycle :: Input -> Input
move1Cycle inp = foldl tilt inp [North, West, South, East]

part2 :: Input -> Int -> Int
part2 inp maxi = (solve . fst $ recurse (inp, M.empty) (maxi))
    where
        recurse n i
            | i < 1 = n
        recurse (inp@(Input a c rs), m) i
            | (isJust $ memoed) && i > loopSize = (recurse (inp, m) (i `mod` loopSize))
            where
                memoed = rs `M.lookup` m
                loopSize = (+ (-i)) . snd $ fromJust memoed
        recurse (inp, memo) i = (recurse (newInp, newMemo) (i-1))
            where
                newInp = move1Cycle inp
                newMemo = M.insert (rocks inp) (rocks newInp, i) memo

gridStr :: Input -> String
gridStr (Input (w, h) c rs) = "\n--------------\n" ++ (unlines $ [[if (x, y) `elem` rs then 'O' else (if (x, y) `member` c then '#' else ' ') | x<-[1..w]] | y<-[1..h]]) ++ "--------------\n"

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    print $ 106648 == part1 inp
    print $ 87700 ==  part2 inp (1000000000)

    -- putStr . unlines $ map (\i -> show (i, part2 inp i)) [1000000000-40..1000000000+40] -- ..1000000000+50]
