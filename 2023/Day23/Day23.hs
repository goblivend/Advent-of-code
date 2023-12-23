module Main where

import Data.Char
import Data.Maybe ( Maybe, fromJust, isJust )
import Debug.Trace(trace)
import Data.Tuple
import Data.Tuple.Extra;
import Data.List.Split
import Data.List.Unique
import Data.Set (Set, member, fromList, toList)
import Data.Map (Map, (!))
import Data.NumInstances.Tuple
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

data Block = Wall | Path | Slope {dir::(Int, Int)} deriving (Eq)
data Input = Input {start ::(Int, Int), end::(Int, Int), grid:: [[Block]]}

instance Show Block where
    show Wall = "#"
    show Path = "."
    show (Slope (-1,  0)) = "<"
    show (Slope ( 1,  0)) = ">"
    show (Slope ( 0, -1)) = "^"
    show (Slope ( 0,  1)) = "v"

parseInput :: String -> Input
parseInput inp = Input (xStart, 0) (yStart, length lined-1) grid
    where
        readBlock '.' = Path
        readBlock '#' = Wall
        readBlock '<' = Slope (-1,  0)
        readBlock '>' = Slope ( 1,  0)
        readBlock '^' = Slope ( 0, -1)
        readBlock 'v' = Slope ( 0,  1)
        lined = lines inp
        grid = map (map readBlock) $ lined
        xStart = length . takeWhile (/= '.') . (!! 0) $ lined
        yStart = length . takeWhile (/= '.') . (!! (length lined -1)) $ lined

at :: [[Block]] -> (Int, Int) -> Block
at grid (x, y) = grid !! y !! x

pathFind :: Input -> Set (Int, Int) -> (Int, Int) -> Block -> Set (Int, Int)
pathFind inp seen curr Wall = S.empty
pathFind inp seen curr block
    | curr == start inp = pathFind inp (S.insert curr seen) (curr + (0, 1)) (at (grid inp) (curr + (0, 1)))
    | curr == end inp = seen
    | curr `S.member` seen = S.empty
pathFind inp seen curr (Slope dir) = pathFind inp (S.insert curr seen) (curr + dir) (at (grid inp) (curr + dir))
pathFind inp seen curr Path
    | otherwise         =  (last . L.sortBy (\s1 s2 -> compare (S.size s1) (S.size s2)) . map ((\next -> pathFind inp (S.insert curr seen) next (at (grid inp) next)) . (+curr)) $ [(1, 0), (-1, 0), (0, -1), (0, 1)])

part1 :: Input -> Int
part1 inp = S.size $ pathFind inp S.empty (start inp) (at (grid inp) (start inp))

toWeightedGraph :: Input -> Map (Int, Int) [((Int, Int), Int)]
toWeightedGraph (Input st en grid) = M.fromList . map (second (M.toList)) . M.toList . clean . M.fromList . map (second M.fromList) $ allNodes
    where
        start = (st, [(st + (0, 1), 1)])
        end   = (en, [(en - (0, 1), 1)])
        allNodes = start:end:[((x, y), neighboursOf (x,y))| x<-[1..fst en], y<-[1..snd en-1], at grid (x, y) /= Wall]
        neighboursOf :: (Int, Int) -> [((Int, Int), Int)]
        neighboursOf xy = zip neighbours (cycle [1])
            where
                neighbours = filter ((/= Wall) . (at grid)) $
                    map (+xy) [(1, 0), (-1, 0), (0, -1), (0, 1)]
        clean :: Map (Int, Int) (Map (Int, Int) Int) -> Map (Int, Int) (Map (Int, Int) Int)
        clean m
            | null paths = m
            | otherwise  = clean cleaned
                where
                    (xy, [(xy1, d1), (xy2, d2)]) = second M.toList $ head paths
                    paths = filter ((== 2) . length . snd) $ M.toList m

                    addIt :: (Int, Int) -> Int -> Maybe (Map (Int, Int) Int) -> Maybe (Map (Int, Int) Int)
                    addIt dxy d (Just m) = Just $ alterArr dxy d m
                    alterArr :: (Int, Int) -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int
                    alterArr dxy d mp = M.delete xy . M.insert dxy (d+ mp ! xy) $ mp
                    cleaned :: Map (Int, Int) (Map (Int, Int) Int)
                    cleaned = M.delete xy . M.alter (addIt xy1 d1) xy2 . M.alter (addIt xy2 d2) xy1 $ m

pathFindGraph :: (Int, Int) -> Map (Int, Int) [((Int, Int), Int)] -> Set (Int, Int) -> ((Int, Int), Int) -> [Int]
pathFindGraph end graph seen (curr, acc)
    | end == curr = [acc]
    | otherwise   = concat . map ((pathFindGraph end graph (S.insert curr seen))) .
                    map (second (+acc)) . filter ((`S.notMember` seen) . fst) $ graph ! curr

part2 :: Input -> Int
part2 inp = maximum $ pathFindGraph (end inp) (toWeightedGraph inp) S.empty (start inp, 0)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    -- print inp
    print $ part1 inp
    print $ part2 inp
    -- putStr . unlines . map show $ M.toList $ toWeightedGraph inp
